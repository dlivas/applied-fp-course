{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level07.AppM
  ( AppM (..)
  , App
  , Env (..)
  , liftEither
  , runApp
  , defaultEnvLoggingFn
  ) where

import           Control.Monad          (join)
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader           ( MonadReader(..)
                                                , asks
                                                )

import           Data.Text              (Text)

import           Level07.Types          (Conf, FirstAppDB)
import           Level07.Types.Error    (Error)

-- | First, let's clean up our (Conf,FirstAppDB) with an application Env type.
-- We will add a general purpose logging function as well. Remember that
-- functions are values, we're able to pass them around and place them on
-- records like any other type.
data Env = Env

  -- | We will add a function to take some 'Text' input and print it to the
  -- console as a crude form of logging. Construct a function that matches this
  -- type so you can include it when you create the 'Env'.
  { envLoggingFn :: Text -> App ()

  -- We're able to nest records to keep things neat and tidy.
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

defaultEnvLoggingFn :: Text -> App ()
defaultEnvLoggingFn t =
  AppM $
    \_ -> do
      print t
      return  (Right  ())

-- | It would be nice to remove the need to pass around our Env to every
-- function that needs it. Wouldn't it be great to have our functions run where
-- we could simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM e a = AppM
  { runAppM :: Env -> IO (Either e a)
  }
  -- | Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor
  -- | We could do this for the rest of these instances, but that would turn
  -- into "magic" what is otherwise straight-forward implementations. You are
  -- here to learn after all.

type App = AppM Error

runApp :: App a -> Env -> IO (Either Error a)
runApp = runAppM

instance Applicative (AppM e) where
  pure :: a -> AppM e a
  pure =
    AppM . pure . pure . pure
    -- AppM . const . return . Right

  (<*>) :: AppM e (a -> b) -> AppM e a -> AppM e b
  AppM f' <*> AppM a' = 
    AppM $
      \e -> do
        f <- f' e
        a <- a' e
        return $ f <*> a

instance Monad (AppM e) where
  return :: a -> AppM e a
  return =
    pure

  -- | When it comes to running functions in (AppM e) as a Monad, this will take
  -- care of passing the Env from one function to the next whilst preserving the
  -- error handling behaviour.
  (>>=) :: AppM e a -> (a -> AppM e b) -> AppM e b
  AppM aIO >>= f =
    AppM $
      \env -> do
        aE <- aIO env
        runAppM
          (either throwError f aE)
          env

instance MonadError e (AppM e) where
  throwError :: e -> AppM e a
  throwError =
    AppM . const . return . Left

  catchError :: AppM e a -> (e -> AppM e a) -> AppM e a
  catchError (AppM aIO) errorHandler =
    AppM $
      \env -> do
        aE <- aIO env
        runAppM
          (either errorHandler pure aE)
          env
    
instance MonadReader Env (AppM e) where
  -- Return the current Env from the AppM.
  ask :: AppM e Env
  ask =
    AppM $ pure . pure
    -- AppM $ return . Right
    -- AppM $ \env -> (runAppM . pure) env env 
    -- AppM $ join (runAppM . pure)
    -- AppM $ runAppM . pure <*> id

  -- Run a (AppM e) inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM e a -> AppM e a
  local f (AppM aIO) =
    AppM $ aIO . f

  -- This will run a function on the current Env and return the result.
  -- reader :: (Env -> a) -> AppM e a
  -- reader =
    -- AppM $ return . Right . f
    ------- actual implemenntation
    ------- (http://hackage.haskell.org/package/mtl-2.2.2/docs/src/Control.Monad.Reader.Class.html#ask)
    ------- follows:
    -- do
    --   env <- ask
    --   return $ f env
    ------- OR
    -- f <$> ask
    ------- OR
  -- reader = 
  --   asks 

instance MonadIO (AppM e) where
  -- Take a type of 'IO a' and lift it into our (AppM e).
  liftIO :: IO a -> AppM e a
  liftIO =
    AppM . const . (Right <$>)

-- | This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither :: Either e a -> AppM e a
liftEither =
  either throwError pure

-- Move on to ``src/Level07/DB.hs`` after this
