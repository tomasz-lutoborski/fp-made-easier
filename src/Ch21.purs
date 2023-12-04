module Ch21 where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow, catchError, throwError)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.State.Class (class MonadState, get, put)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, tell)
import Control.Monad.Writer.Trans (WriterT, runWriterT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console as Console

-- WriterT
-- newtype WriterT w m a = WriterT (m (Tuple a w))

-- runWriterT :: ∀ w m a. WriterT w m a -> m (Tuple a w)
-- runWriterT (WriterT mx) = mx

-- instance functorWriterT :: Functor m => Functor (WriterT w m) where
--   map f (WriterT mx) = WriterT $ mx <#> \(Tuple x w) -> Tuple (f x) w

-- instance applyWriterT :: (Semigroup w, Monad m) => Apply (WriterT w m) where
--   apply (WriterT mf) (WriterT mx) = WriterT do
--     Tuple f w1 <- mf
--     Tuple x w2 <- mx
--     pure $ Tuple (f x) (w1 <> w2)

-- instance applicativeWriterT :: (Monoid w, Monad m) => Applicative (WriterT w m) where
--   pure x = WriterT $ pure $ Tuple x mempty

-- instance bindWriterT :: (Semigroup w, Monad m) => Bind (WriterT w m) where
--   bind (WriterT mx) f = WriterT do
--     Tuple x w1 <- mx
--     Tuple y w2 <- runWriterT $ f x
--     pure $ Tuple y $ w1 <> w2

-- instance monadWriterT :: (Monoid w, Monad m) => Monad (WriterT w m)

-- ReaderT
-- newtype ReaderT :: forall k. Type -> (k -> Type) -> k -> Type
-- newtype ReaderT r m a = ReaderT (r -> m a)

-- runReaderT :: ∀ r m a. ReaderT r m a -> r -> m a
-- runReaderT (ReaderT f) = f

-- instance functorReaderT :: Functor m => Functor (ReaderT r m) where
--   map f (ReaderT mg) = ReaderT \r -> f <$> mg r

-- instance applyReaderT :: Apply m => Apply (ReaderT r m) where
--   apply (ReaderT fmf) (ReaderT fmx) = ReaderT \r -> fmf r <*> fmx r

-- instance applicativeReaderT :: Applicative m => Applicative (ReaderT r m) where
--   pure = ReaderT <<< const <<< pure

-- instance bindReaderT :: (Apply m, Bind m) => Bind (ReaderT r m) where
--   bind (ReaderT fmx) f = ReaderT \r -> fmx r >>= \x -> runReaderT (f x) r

-- instance monadReaderT :: Monad m => Monad (ReaderT r m)

-- StateT
newtype StateT s m a = StateT (s -> m (Tuple a s))

runStateT :: forall s m a. StateT s m a -> (s -> m (Tuple a s))
runStateT (StateT f) = f

instance functorStateT :: Functor m => Functor (StateT s m) where
  map f (StateT mg) =
    StateT \s -> mg s <#> \(Tuple x s') -> Tuple (f x) s'

instance applyStateT :: Monad m => Apply (StateT s m) where
  apply (StateT fmf) (StateT fmx) = StateT \s -> do
    Tuple f s1 <- fmf s
    Tuple x s2 <- fmx s1
    pure $ Tuple (f x) s2

instance applicativeStateT :: Monad m => Applicative (StateT s m) where
  pure x = StateT \s -> pure $ Tuple x s

instance bindStateT :: Monad m => Bind (StateT s m) where
  bind (StateT fmx) f = StateT \s -> do
    Tuple x s1 <- fmx s
    runStateT (f x) s1

instance monadStateT :: Monad m => Monad (StateT s m)

instance monadTransStateT :: MonadTrans (StateT s) where
  lift mx = StateT \s -> mx <#> \x -> Tuple x s

instance monadStateStateT :: Monad m => MonadState s (StateT s m) where
  state f = StateT $ pure <<< f

instance monadAskStateT :: MonadAsk r m => MonadAsk r (StateT s m) where
  ask = lift ask

instance monadThrowStateT :: MonadThrow e m => MonadThrow e (StateT s m) where
  throwError = lift <<< throwError

instance monadTellStateT :: MonadTell w m => MonadTell w (StateT s m) where
  tell = lift <<< tell

instance monadErrorStateT :: MonadError e m => MonadError e (StateT s m) where
  catchError :: ∀ a. StateT s m a -> (e -> StateT s m a) -> StateT s m a
  catchError (StateT fmx) f = StateT \s ->
    catchError (fmx s) \e -> runStateT (f e) s

type AppStack e w s a = ExceptT e (WriterT w (StateT s Effect)) a

type AppM = AppStack String String Int Unit

type StackResult = Tuple (Tuple (Either String Unit) String) Int

runApp :: Int -> AppM -> Effect AppResult
runApp st = (results <$> _) <<< flip runStateT st <<< runWriterT <<< runExceptT

type AppEffects =
  { log :: String
  , state :: Int
  , result :: Maybe Unit
  }

type AppResult = Tuple (Maybe String) AppEffects

results :: StackResult -> AppResult
results (Tuple (Tuple (Left err) l) s) = Tuple (Just err) { log: l, state: s, result: Nothing }
results (Tuple (Tuple (Right res) l) s) = Tuple Nothing { log: l, state: s, result: Just res }

log :: ∀ m. MonadTell String m => String -> m Unit
log s = tell $ s <> "\n"

app :: AppM
app = do
  tell "Starting app"
  n <- get
  when (n == 0) $ void $ throwError "state cannot be 0"
  put $ n + 1
  tell "Incremented state"
  pure unit

