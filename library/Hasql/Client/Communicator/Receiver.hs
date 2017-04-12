module Hasql.Client.Communicator.Receiver where

import Hasql.Prelude hiding (peek)
import qualified Hasql.Protocol.Model as J
import qualified Hasql.Client.Socket as F
import qualified Hasql.Buffer as E
import qualified Hasql.Ptr.Peek as C


data Error =
  TransportError Text |
  ParsingError Text


{-|
A specialized buffered socket reader.
-}
data Receiver =
  Receiver F.Socket E.Buffer

acquire :: F.Socket -> IO Receiver
acquire socket =
  Receiver socket <$> acquireBuffer
  where
    acquireBuffer =
      E.new (shiftL 1 14)


newtype Do result =
  Do (ReaderT Receiver (ExceptT Error IO) result)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Error)

primitiveDo :: (F.Socket -> E.Buffer -> IO (Either Error result)) -> Do result
primitiveDo def =
  Do (ReaderT (\(Receiver socket buffer) -> ExceptT (def socket buffer)))

{-|
Populate the buffer by fetching the data from socket.
-}
fetchFromSocket :: Int -> Do ()
fetchFromSocket amount =
  primitiveDo def
  where
    def socket buffer =
      do
        traceEventIO ("START fetchFromSocket " <> show amount)
        socketEither <- E.put buffer actualAmount $ \ptr -> do
          either <- F.receiveToPtr socket ptr actualAmount
          case either of
            Left x -> return (Left (TransportError x), 0)
            Right x -> return (Right (), x)
        traceEventIO ("STOP fetchFromSocket " <> show amount)
        return socketEither
    actualAmount =
      max amount (shiftL 1 13)

{-|
Ensure that there is a certain amount of bytes available in the buffer,
blocking until that.

Initiates the socket fetching if need be.
-}
arrangeData :: Int -> Do ()
arrangeData amount =
  do
    availableAmount <- primitiveDo $ \socket buffer -> Right <$> E.getOccupiedSpace buffer
    if availableAmount >= amount
      then return ()
      else fetchFromSocket (amount - availableAmount)

peek :: C.Peek peeked -> Do peeked
peek peek =
  case C.run peek of
    (amount, ptrIO) ->
      do
        arrangeData amount
        primitiveDo $ \_ buffer -> E.take buffer $ \ptr _ -> do
          result <- ptrIO ptr
          return (Right result, amount)

getMessageHeader :: (J.MessageType -> Int -> result) -> Do result
getMessageHeader cont =
  peek peeker
  where
    peeker =
      cont <$> (J.MessageType <$> C.word8) <*> (fromIntegral <$> C.beWord32)

getMessageBytes :: Int -> Do ByteString
getMessageBytes amount =
  peek (C.bytes amount)

getMessage :: (J.MessageType -> ByteString -> result) -> Do result
getMessage cont =
  join $ getMessageHeader $ \messageType messageLength ->
  do
    bytes <- getMessageBytes messageLength
    return (cont messageType bytes)
