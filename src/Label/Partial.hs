-- | Partial polymorphic labels for getters that might fail.

{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

module Label.Partial
( Partial
, (:~>)
, makeP
, getP
, modifyP
, setP

, traverseP
, forP
, writeP

, orP
, joinP
, total
)
where

import Control.Applicative ((<|>))
import Control.Monad.Identity
import Data.Maybe (fromMaybe)
import Label.Core (Label)

import qualified Label.Core  as Core
import qualified Label.Total as Total

{-# INLINE makeP     #-}
{-# INLINE getP      #-}
{-# INLINE modifyP   #-}
{-# INLINE setP      #-}
{-# INLINE traverseP #-}
{-# INLINE forP      #-}
{-# INLINE writeP    #-}
{-# INLINE joinP     #-}
{-# INLINE total     #-}

-- | Partial label.

type Partial m f a = Label Maybe m f a

-- | Simple partial label.

type f :~> a = Label Maybe Identity f a

-- | Create a partial label from a getter that can fail and a total
-- modification function.

makeP :: (f -> Maybe o) -> ((o -> m i) -> f -> m g) -> Partial m (f -> g) (o -> i)
makeP = Core.make

-- | Getter for a partial label.

getP :: (f -> g) :~> (o -> i) -> f -> Maybe o
getP = Core.get

-- | Modifier for a partial label.

modifyP :: (f -> g) :~> (o -> i) -> (o -> i) -> f -> g
modifyP l m = runIdentity . Core.modify l (Identity . m)

-- | Setter for a partial label.

setP :: (f -> g) :~> (o -> i) -> i -> f -> g
setP l v = runIdentity . Core.set l (Identity v)

-------------------------------------------------------------------------------

traverseP :: Label Maybe n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
traverseP = Core.modify

-- | Flipped version of traverse function.

forP :: Label Maybe n (f -> g) (o -> i) -> f -> (o -> n i) -> n g
forP l = flip (Core.modify l)

-- | Setter within some context.

writeP :: Label Maybe n (f -> g) (o -> i) -> n i -> f -> n g
writeP l v = traverseP l (const v)

-------------------------------------------------------------------------------

-- | Combine two partial labels into a single one, working the left most
-- available field.

orP :: Monad m => Partial m (f -> f) (o -> o) -> Partial m (f -> f) (o -> o) -> Partial m (f -> f) (o -> o)
orP q r = Core.make (liftM2 (<|>) (Core.get q) (Core.get r))
                    (\f -> Core.modify r f <=< Core.modify q f)

-- | Embed a partial label pointing to a `Maybe` field in a partial label by
-- joining the `Maybe.

joinP :: (f -> g) :~> (Maybe o -> Maybe i) -> (f -> g) :~> (o -> i)
joinP l = makeP (join . getP l) (\m -> Identity . modifyP l (fmap (runIdentity . m)))
            
-- | Given a default value for the Nothing case, embed a partial label in a
-- total label.

total :: o -> (f -> g) :~> (o -> i) -> (f -> g) Total.:-> (o -> i)
total d l = Total.make (fromMaybe d . getP l) (modifyP l)
