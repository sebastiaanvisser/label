-- | Traversing labels can be done directly with any label that has no
-- resitrictions on the modifier context.

module Label.Traversing
( traverse
, traverseI
, for
, forI
, write
, lifted
, embed
)
where

import Control.Monad
import Control.Monad.Identity (Identity, runIdentity)
import Label.Core
import Prelude hiding (traverse)

{-# INLINE traverse  #-}
{-# INLINE traverseI #-}
{-# INLINE for       #-}
{-# INLINE forI      #-}
{-# INLINE write     #-}

-- | Modifier within some context.

traverse :: Label m n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
traverse = modify

-- | Modifier within some context, with getter component fixed.

traverseI :: Label Identity n (f -> g) (o -> i) -> (o -> n i) -> f -> n g
traverseI = modify

-- | Flipped version of traverse function.

for :: Label m n (f -> g) (o -> i) -> f -> (o -> n i) -> n g
for l = flip (modify l)

-- | Flipped version of traverse function, with getter component fixed.

forI :: Label Identity n (f -> g) (o -> i) -> f -> (o -> n i) -> n g
forI l = flip (modify l)

-- | Setter within some context.

write :: Label m n (f -> g) (o -> i) -> n i -> f -> n g
write l v = traverse l (const v)

-- | Lifted label composition.
--
-- For example, when specialized to simple labels and lists:
--
-- > :: (f :-> [o])
-- > -> (o :-> [a])
-- > -> (f :-> [a])

lifted :: (Monad m, Monad n, Monad r, Traversable r)
       => Label m n (f -> g) (r o -> r p)
       -> Label m n (o -> p) (r i -> r j)
       -> Label m n (f -> g) (r i -> r j)
lifted a b = make (liftM join . mapM (get b) <=< get a)
                  (\m -> modify a (mapM (modify b m)))

-- | Embedding effects.

embed :: (Monad n, Monad m, Traversable m)
      => Label Identity n (f -> g) (m o -> m b)
      -> Label m        n (f -> g) (  o ->   b)
embed l = make (runIdentity . get l)
               (\f -> modify l (mapM f))
