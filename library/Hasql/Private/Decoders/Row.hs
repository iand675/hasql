module Hasql.Private.Decoders.Row where

import Hasql.Private.Prelude
import Hasql.Private.Errors
import Data.Vector (generateM)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified PostgreSQL.Binary.Decoding as A
import qualified Hasql.Private.Decoders.Value as Value


newtype Row a =
  Row (ReaderT Env (ExceptT RowError IO) a)
  deriving (Functor, Applicative, Monad)

-- Vectors here are lazy intentionally
data Env =
  Env !LibPQ.Result !LibPQ.Row !LibPQ.Column !Bool !(IORef LibPQ.Column) (Vector ByteString) (Vector LibPQ.Oid)

-- * Functions
-------------------------

{-# INLINE run #-}
run :: Row a -> (LibPQ.Result, LibPQ.Row, LibPQ.Column, Bool) -> IO (Either RowError a)
run (Row impl) (result, row, columnsAmount@(LibPQ.Col columnsInt), integerDatetimes) =
  do
    columnRef <- newIORef 0
    columnNames <- unsafeInterleaveIO $
      generateM (fromIntegral columnsInt) (fmap (fromMaybe "") . LibPQ.fname result . LibPQ.toColumn)
    columnOids <- unsafeInterleaveIO $
      generateM (fromIntegral columnsInt) (LibPQ.ftype result . LibPQ.toColumn)
    runExceptT (runReaderT impl (Env result row columnsAmount integerDatetimes columnRef columnNames columnOids))

{-# INLINE error #-}
error :: RowError -> Row a
error = Row . ReaderT . const . ExceptT . pure . Left

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE value #-}
value :: Value.Value a -> Row (Maybe a)
value valueDec =
  {-# SCC "value" #-}
  Row $ ReaderT $ \(Env result row columnsAmount integerDatetimes columnRef _ _) -> ExceptT $ do
    col <- readIORef columnRef
    writeIORef columnRef (succ col)
    if col < columnsAmount
      then do
        valueMaybe <- {-# SCC "getvalue'" #-} LibPQ.getvalue' result row col
        pure $
          case valueMaybe of
            Nothing ->
              Right Nothing
            Just value ->
              fmap Just $ mapLeft ValueError $
              {-# SCC "decode" #-} A.valueParser (Value.run valueDec integerDatetimes) value
      else pure (Left EndOfInput)

-- |
-- Next value, decoded using the provided value decoder.
{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> Row a
nonNullValue valueDec =
  {-# SCC "nonNullValue" #-}
  value valueDec >>= maybe (error UnexpectedNull) pure

-- ** Column metadata

-- |
-- The index of the next unconsumed column. Not recommended
-- for general use, but may be useful in tandem with 'columnNames'.
{-# INLINE columnPosition #-}
columnPosition :: Row Int
columnPosition = Row $ do
  (Env _ _ _ _ columnRef _ _) <- ask
  (LibPQ.Col c) <- liftIO $ readIORef columnRef
  pure $ fromIntegral c

{-# INLINE columnNames #-}
columnNames :: Row (Vector ByteString)
columnNames = Row $ do
  (Env _ _ _ _ _ columnNames _) <- ask
  pure $! columnNames

{-# INLINE columnOids #-}
columnOids :: Row (Vector LibPQ.Oid)
columnOids = Row $ do
  (Env _ _ _ _ _ _ columnOids) <- ask
  pure $! columnOids
