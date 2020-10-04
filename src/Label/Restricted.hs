-- | Restricted polymorphic labels with getters and modifiers that can both fail.

{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}

module Label.Restricted
( (:~>)
, makeR
, getR
, modifyR
, setR
)
where

import Label.Core (Label)

import qualified Label.Core as Core

{-# INLINE makeR   #-}
{-# INLINE getR    #-}
{-# INLINE modifyR #-}
{-# INLINE setR    #-}

-- | Restricted partial label type.

type f :~> a = Label Maybe Maybe f a

-- | Create a restricted label from a getter that can fail and a modification
-- function that can fail.

makeR :: (f -> Maybe o) -> ((o -> Maybe i) -> f -> Maybe g) -> (f -> g) :~> (o -> i)
makeR = Core.make

-- | Getter for a restricted label.

getR :: (f -> g) :~> (o -> i) -> f -> Maybe o
getR = Core.get

-- | Modifier for a restricted label.

modifyR :: (f -> g) :~> (o -> i) -> (o -> i) -> f -> Maybe g
modifyR l m = Core.modify l (Just . m)

-- | Setter for a restricted label.

setR :: ((f -> g) :~> (o -> i)) -> i -> f -> Maybe g
setR l = modifyR l . const
