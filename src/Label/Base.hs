-- | Labels for data types in the base package. The label types are kept
-- abstract to be fully reusable in custom contexts. Build to be imported
-- qualified.

{-# LANGUAGE
    KindSignatures
  , NoMonomorphismRestriction
  , TemplateHaskell
  , TypeOperators #-}

module Label.Base
(
-- * Labels for lists.
  headL
, tailL
, listL
, atL
, lastL
, reverseL

-- * Labels for non-empty lists.
, neHeadL
, neTailL

-- * Labels for Either.
, leftL
, rightL

-- * Label for Maybe.
, justL

-- * Labels for 2-tuples.
, fstL
, sndL
, swapL
, pairL

-- * Labels for 3-tuples.
, fst3L
, snd3L
, trd3L
, tripleL

-- * Read/Show isomorphism.
, readShowL
)
where

import Control.Applicative
import Control.Category
import Data.List.NonEmpty (NonEmpty)
import Label.Core (Iso (..), isom, Label, make)
import Label.Derive (getLabel)
import Label.Mono (Mono)
import Prelude hiding (fst, snd, head, tail, last, reverse, (.), id)

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import qualified Data.Tuple as Tuple

-- | Label pointing to the head of a list's cons cell. (Partial and monomorphic)

headL :: (Alternative m, Applicative n)
      => Mono (Label m n) [a] a

-- | Label pointing to the tail of a list's cons cell. (Partial and monomorphic)

tailL :: (Alternative m, Applicative n)
      => Mono (Label m n) [a] [a]

(headL, tailL) = $(getLabel ''[])

-- | Pointwise label for all items in a list.

listL :: Applicative m => Label [] m ([o] -> [i]) (o -> i)
listL = make id traverse

-- | Partial label for indexed access into a list.

atL :: (Alternative m, Applicative n) => Int -> Label m n ([a] -> [a]) (a -> a)
atL i = make (\ls   -> if length ls > i
                       then pure (ls !! i)
                       else empty)
             (\f ls -> if length ls > i
                       then (take i ls ++) <$> ((: drop (i + 1) ls) <$> f (ls !! i))
                       else pure ls)

-- | Label pointing to the last element of a list. (Partial and monomorphic)

lastL :: (Monad m, Monad n, Alternative m) => Label m n ([a] -> [a]) (a -> a)
lastL = headL . isom reverseL reverseL

-- | Label accessing the reverse of a list. (Total and monomorphic)

reverseL :: (Applicative m, Applicative n) => Iso m n [a] [a]
reverseL = Iso (pure . List.reverse) (pure . List.reverse)

-- | Label pointing to the head of a non-empty list's cons cell. (Total and monomorphic)

neHeadL :: (Applicative n, Applicative m) => Mono (Label m n) (NonEmpty a) a

-- | Label pointing to the tail of a non-empty list's cons cell. (Total and monomorphic)

neTailL :: (Applicative n, Applicative m) => Mono (Label m n) (NonEmpty a) [a]

(neHeadL, neTailL) = $(getLabel ''NonEmpty)

-- | Label pointing to the left value in an Either. (Partial and polymorphic)

leftL :: (Applicative n, Alternative m)
      => Label m n (Either a b -> Either c b) (a -> c)

-- | Label pointing to the right value in an Either. (Partial and polymorphic)

rightL :: (Applicative n, Alternative m)
       => Label m n (Either a b -> Either a c) (b -> c)

(leftL, rightL) = $(getLabel ''Either)

-- | Label pointing to the value in a Maybe. (Partial and polymorphic)

justL :: (Applicative n, Alternative m)
      => Label m n (Maybe a -> Maybe b) (a -> b)
justL = $(getLabel ''Maybe)

-- | Label pointing to the first component of a 2-tuple. (Total and polymorphic)

fstL :: (Applicative n, Applicative m)
     => Label m n ((a, b) -> (c, b)) (a -> c)

-- | Label pointing to the second component of a 2-tuple. (Total and polymorphic)

sndL :: (Applicative n, Applicative m)
     => Label m n ((a, b) -> (a, c)) (b -> c)

(fstL, sndL) = $(getLabel ''(,))

-- | Polymorphic label that swaps the components of a tuple. (Total and polymorphic)

swapL :: Applicative m => Iso m m (a, b) (b, a)
swapL = Iso (pure . Tuple.swap) (pure . Tuple.swap)

-- | Pointwise access to the two items in a pair.

pairL :: Applicative m
      => Label [] m ((o, o) -> (a, a)) (o -> a)
pairL = make (\(a, b) -> [a, b]) (\m (a, b) -> (,) <$> m a <*> m b)

-- | Label pointing to the first component of a 3-tuple. (Total and polymorphic)

fst3L :: (Applicative m, Applicative n)
      => Label m n ((a, b, c) -> (d, b, c)) (a -> d)

-- | Label pointing to the second component of a 3-tuple. (Total and polymorphic)

snd3L :: (Applicative m, Applicative n)
      => Label m n ((a, b, c) -> (a, d, c)) (b -> d)

-- | Label pointing to the third component of a 3-tuple. (Total and polymorphic)

trd3L :: (Applicative m, Applicative n)
      => Label m n ((a, b, c) -> (a, b, d)) (c -> d)

(fst3L, snd3L, trd3L) = $(getLabel ''(,,))

-- | Pointwise access to the three items in a triple.

tripleL :: Applicative m
        => Label [] m ((o, o, o) -> (a, a, a)) (o -> a)
tripleL = make (\(a, b, c) -> [a, b, c]) (\m (a, b, c) -> (,,) <$> m a <*> m b <*> m c)

-- | Partial isomorphism for readable and showable values. Can easily be lifted
-- into a label by using `iso`.

readShowL :: (Alternative m, Applicative n, Read a, Show a) => Iso m n String a
readShowL = Iso r s
  where r v = case readsPrec 0 v of
                (w, _):_ -> pure w
                []       -> empty
        s = pure . show

