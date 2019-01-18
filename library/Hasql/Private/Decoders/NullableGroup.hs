module Hasql.Private.Decoders.NullableGroup where

import Hasql.Private.Prelude
import Hasql.Private.Errors
import qualified Hasql.Private.Decoders.Value as Value
import qualified Hasql.Private.Decoders.Row as Row

data GroupNullity
  = GroupWasNotNull
  | GroupWasNull

newtype NullableGroup a =
  NullableGroup (ReaderT (Row.Env, IORef GroupNullity) (ExceptT RowError IO) a)
  deriving (Functor, Applicative)

{-# INLINE run #-}
run :: NullableGroup a -> Row.Row (Maybe a)
run (NullableGroup m) = do
  (s, eVal) <- Row.Row $ do
    env <- ask
    liftIO $ do
      groupStatus <- newIORef GroupWasNotNull
      eVal <- runExceptT $ runReaderT m (env, groupStatus)
      s <- readIORef groupStatus
      return (s, eVal)
  case s of
    GroupWasNotNull -> case eVal of
      Left e -> Row.error e
      Right ok -> return $ Just ok
    GroupWasNull -> return Nothing

{-# INLINE runRow #-}
runRow :: Row.Row a -> NullableGroup a
runRow (Row.Row m) = NullableGroup $ withReaderT fst m

{-# INLINE value #-}
value :: Value.Value a -> NullableGroup (Maybe a)
value valueDec = runRow $ Row.value valueDec

{-# INLINE nonNullValue #-}
nonNullValue :: Value.Value a -> NullableGroup a
nonNullValue valueDec = NullableGroup $ do
  let (NullableGroup m) = runRow $ Row.value valueDec
  mr <- m
  case mr of
    Nothing -> do
      (_, ref) <- ask
      liftIO $ writeIORef ref GroupWasNull
      pure ($bug "NullableGroup result should not be used if group was null")
    Just r -> pure r

{-# INLINE columnPosition #-}
columnPosition :: NullableGroup Int
columnPosition = runRow Row.columnPosition

{-# INLINE columnNames #-}
columnNames :: NullableGroup (Vector ByteString)
columnNames = runRow Row.columnNames
