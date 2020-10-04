-- | State and Reader operations specialized for working with total and partial
-- labels.

{-# LANGUAGE TypeOperators  #-}

module Label.Monadic
(
-- * 'MonadState' label operations.
  getsL
, putsL
, modifyL
, (%=)
, localSt

-- * 'MonadReader' label operations.
, asksL
, localL

-- * 'MonadWriter' label operations.
, tellsL

-- * 'MonadState' partial label operations.
, getsP
, putsP
, modifyP

-- * 'MonadReader' partial label operations.
, asksP
, localP
)
where

import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer.Strict (MonadWriter)
import Label.Simple ((:->), (:~>))

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.State as State
import qualified Control.Monad.Writer.Strict as Writer
import qualified Label.Core as Core
import qualified Label.Partial as Partial
import qualified Label.Total as Total

-- | Get a value out of the state, pointed to by the specified label.

getsL :: MonadState f m => (f :-> o) -> m o
getsL = State.gets . Total.get

-- | Set a value somewhere in the state, pointed to by the specified label.

putsL :: MonadState f m => f :-> o -> o -> m ()
putsL l v = modifyL l (const v)

-- | Modify a value with a function somewhere in the state, pointed to by the
-- specified label.

modifyL :: MonadState f m => f :-> o -> (o -> o) -> m ()
modifyL l f = State.modify (Total.modify l f)

infix %=

(%=) :: MonadState f m => f :-> o -> (o -> o) -> m ()
(%=) = modifyL

-- | Similar to localL but inside the state monad.

localSt :: MonadState f m => f :-> o -> (o -> o) -> m a -> m a
localSt l f a =
  do o <- getsL l
     modifyL l f
     r <- a
     putsL l o
     return r

-- | Fetch a value pointed to by a label out of a reader environment.

asksL :: MonadReader f m => (f :-> o) -> m o
asksL = Reader.asks . Total.get

-- | Execute a computation in a modified environment. The label is used to
-- point out the part to modify.

localL :: MonadReader f m => (f :-> o) -> (o -> o) -> m a -> m a
localL l f = Reader.local (Total.modify l f)

-- | Write a part of a monadic value to the writer log, by setting the part
-- pointed to by the label on an empty value.

tellsL :: MonadWriter f m => (f :-> o) -> o -> m ()
tellsL l v = Writer.tell (Total.set l v mempty)

-------------------------------------------------------------------------------

-- | Get a possible value out of the state, pointed to by the specified partial
-- label.

getsP :: MonadState f m => (f :~> o) -> m (Maybe o)
getsP = State.gets . Partial.getP

-- | Fetch a possible value pointed to by a partial label out of a reader
-- environment.

asksP :: MonadReader f m => (f :~> o) -> m (Maybe o)
asksP = Reader.asks . Partial.getP

-- | Execute a computation in a modified environment. The partial label is used
-- to point out the part to modify.

localP :: MonadReader f m => f :~> o -> (o -> o) -> m a -> m a
localP l f = Reader.local (Partial.modifyP l f)

-- | Possibly set a value somewhere in the state, pointed to by the specified
-- partial label.

putsP :: MonadState f m => f :~> o -> o -> m ()
putsP l v = modifyP l (const v)

-- | Possibly modify a value with a function somewhere in the state, pointed to
-- by the specified partial label.

modifyP :: MonadState f m => f :~> o -> (o -> o) -> m ()
modifyP l m = State.modify (Partial.modifyP l m)
