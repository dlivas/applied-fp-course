{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level08.AppM
  ( AppM(..)
  , App
  , Env(..)
  , liftEither
  , runApp
  , defaultEnvLoggingFn
  )
where

import           Control.Monad                  ( join )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Control.Monad.Reader           ( MonadReader(..)
                                                , asks
                                                )

import           Data.Text                      ( Text )

import           Level08.Types                  ( Conf
                                                , FirstAppDB
                                                )
import           Level08.Types.Error            ( Error )

data Env = Env
  { envLoggingFn :: Text -> App ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

defaultEnvLoggingFn :: Text -> App ()
defaultEnvLoggingFn t = AppM $ \_ -> do
  print t
  return $ Right ()

newtype AppM e a = AppM
  { runAppM :: Env -> IO (Either e a)
  }
  deriving Functor

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure = AppM . pure . pure . pure -- AppM . const . return . Right

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  AppM fIO <*> AppM aIO =
    AppM $
      \e -> do
        f <- fIO e
        a <- aIO e
        return $ f <*> a

instance Monad (AppM e) where
  return :: a -> AppM e a
  return = pure

  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  AppM aIO >>= f =
    AppM $
      \env -> do
        aE <- aIO env
        runAppM (either throwError f aE) env

instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError =
    AppM . pure . pure . Left -- AppM . const . return . Left

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError (AppM aIO) errorHandler =
    AppM $
      \env -> do
        aE <- aIO env
        runAppM (either errorHandler pure aE) env

instance MonadReader Env (AppM e) where
  -- Return the current Env from the AppM.
  ask :: AppM e Env
  ask =
    AppM $ pure . pure -- AppM $ return . Right
  
  -- Run a (AppM e) inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM e a -> AppM e a
  local f (AppM aIO) =
    AppM $ aIO . f

  -- reader :: (Env -> a) -> AppM e a

instance MonadIO (AppM e) where
  liftIO :: IO a -> AppM e a
  liftIO = AppM . const . fmap Right

-- | This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
liftEither :: Either e a -> AppM e a
liftEither = either throwError pure

-- Move on to ``src/Level07/DB.hs`` after this
