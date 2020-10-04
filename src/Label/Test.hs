{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleInstances
  , GADTs
  , KindSignatures
  , LambdaCase
  , MultiParamTypeClasses
  , TemplateHaskell
  , TypeOperators
  , RankNTypes
  , FlexibleContexts
  #-}

module Label.Test (tests) where

import Control.Applicative
import Control.Category
import Control.Monad.Except
import Data.Tuple (swap)
import Label.Simple
import Prelude hiding ((.), id)
import Test.HUnit

import Control.Monad.Reader (runReader)
import Control.Monad.State (evalState)

import Label.Core ((>-), view, iso, inv, Iso(Iso))
import Label.Core (Label)
import Label.Derive (defaultNaming, mkLabelsWith, mkLabels, label)
import Label.Mono (Mono)
import Label.Partial (getP, modifyP, setP)
import Label.Restricted (getR, modifyR, setR)
import Label.Traversing (embed)

import qualified Label.Base       as Base
import qualified Label.Core       as Core
import qualified Label.Monadic    as Monadic
import qualified Label.Partial    as Partial
import qualified Label.Restricted as Restricted
import qualified Label.Simple     as Simple
import qualified Label.Total      as Total

-- No named fields at all.

data NoRecord = NoRecord Integer Bool
  deriving (Eq, Ord, Show)

-- Newtype, obviously one field only.

newtype Newtype a = Newtype { _unNewtype :: [a] }
  deriving (Eq, Ord, Show)

mkLabels [''NoRecord, ''Newtype]

-- Identity view on newtype field.

newtypeId :: Newtype Bool Simple.:-> Newtype Bool
newtypeId = Core.view (id <$> id >- id)

-- The expected most general label type.

unNewtypeExpected
  :: (Applicative n, Applicative m)
  => Core.Label m n (Newtype a -> Newtype b) ([a] -> [b])
unNewtypeExpected = unNewtype

-- A simple single constuctor record type.

data Record = Record
  { label_recA :: Integer
  , label_recB :: Maybe (Newtype Bool)
  , label_recC :: Newtype Bool
  , label_recD :: Either Integer Bool
  } deriving (Eq, Ord, Show)

mkLabelsWith (drop 6) False False ''Record

-- We provide the types ourselves.

recD :: (Applicative m, Applicative n) => Mono (Label m n) Record (Either Integer Bool)
recC :: (Applicative m, Applicative n) => Mono (Label m n) Record (Newtype Bool)
recB :: (Applicative m, Applicative n) => Mono (Label m n) Record (Maybe (Newtype Bool))
recA :: (Applicative m, Applicative n) => Mono (Label m n) Record (Integer)

-- Manual version of recA.

manual_recA :: (Applicative n, Applicative m) => Label m n (Record -> Record) (Integer -> Integer)
manual_recA = Core.make
  (\(Record a _ _ _) -> pure a)
  (\m (Record a b c d) -> Record <$> m a <*> pure b <*> pure c <*> pure d)

-- We could make those labels simple.

recA_simple = recA; recA_simple :: Record Simple.:-> Integer
recB_simple = recB; recB_simple :: Record Simple.:-> Maybe (Newtype Bool)
recC_simple = recC; recC_simple :: Record Simple.:-> Newtype Bool
recD_simple = recD; recD_simple :: Record Simple.:-> Either Integer Bool

-- Multi constructor record type.

label [d|

  data Multi
    = First  { multiA :: Record
             , multiB :: Double
             , multiC :: Either String Float
             }
    | Second { multiB :: Double }
    deriving (Eq, Ord, Show)

  |]

-- multiA and multiC are partial, but multiB is still total.

multiA_partial = multiA; multiA_partial :: Multi Simple.:~> Record
multiB_total   = multiB; multiB_total   :: Multi Simple.:-> Double
multiC_partial = multiC; multiC_partial :: Multi Simple.:~> Either String Float

-- Manual version of multiA.

manual_multiA :: (Applicative n, Alternative m) => Mono (Label m n) Multi Record
manual_multiA = Core.make
  ( \case First a _ _ -> pure a
          Second _    -> empty
  )

  (\ m -> \case First a b c -> First <$> m a <*> pure b <*> pure c
                Second a    -> pure (Second a)
  )

-- Highly polymorphic direction datatype, with some constraints.

data Direction i north east south west
  = North   { _dir :: i, _north   :: north }
  | East    { _dir :: i, _east    :: east }
  | South   { _dir :: i, _south   :: south }
  | West    { _dir :: i, _west    :: west }
  | Lateral { _dir :: i, _lateral :: (east, west) }
  deriving (Eq, Ord, Show)

mkLabelsWith defaultNaming True True ''Direction

-- East and west will be mono, because of their use in lateral.

__dir     = dir     ; __dir     :: (Applicative m, Applicative o) => Label m o        (Direction i n e s w -> Direction a n e s w) (i -> a)
__north   = north   ; __north   :: (Alternative m, Applicative o) => Label m o        (Direction i n e s w -> Direction i a e s w) (n -> a)
__east    = east    ; __east    :: (Alternative m, Applicative o) => Mono (Label m o) (Direction i n e s w) e
__south   = south   ; __south   :: (Alternative m, Applicative o) => Label m o        (Direction i n e s w -> Direction i n e a w) (s -> a)
__west    = west    ; __west    :: (Alternative m, Applicative o) => Mono (Label m o) (Direction i n e s w) w
__lateral = lateral ; __lateral :: (Alternative m, Applicative o) => Mono (Label m o) (Direction i n e s w) (e, w)

-- Manual version of west.

manual_west :: (Alternative m, Applicative o)
            => Label m o (Direction i n e s q -> Direction i n e s w) (q -> w)
manual_west = Core.make
  ( \case North   _ _      -> empty
          East    _ _      -> empty
          South   _ _      -> empty
          West    _ w      -> pure w
          Lateral _ (_, w) -> pure w
  )
  (\m -> \case North   i n      -> pure (North i n)
               East    i e      -> pure (East  i e)
               South   i s      -> pure (South i s)
               West    i w      -> West i <$> m w
               Lateral i (e, w) -> Lateral i <$> ((,) <$> pure e <*> m w)
  )

-- Used to test applicative view.

label [d|

  data View = View
    { viewA :: Maybe (Newtype Bool)
    , viewB :: Either Integer Bool
    , viewC :: Newtype Bool
    } deriving (Eq, Ord, Show)

  |]

-- GADT with type indices constraining constructors.

data Gadt a where
  C1 :: { ga :: Integer, gb :: Bool       } -> Gadt (Int, Bool)
  C2 :: { gc :: Integer, gd :: Maybe Bool } -> Gadt Bool
  C3 :: { ge :: a,       gf :: b          } -> Gadt (a, b)
  C4 :: { gg :: a                         } -> Gadt [a]
  C5 :: {                gd :: Maybe Bool } -> Gadt Bool
  C6 :: { gh :: [a]                       } -> Gadt (a, a, a)
  C7 :: { ga :: Integer, gb :: Bool       } -> Gadt (Int, Bool)
  C8 :: Integer -> Bool                     -> Gadt (Int, Bool)

{-

mkLabel ''Gadt

-- Even with multiple constuctors some fields are total, because of the
-- constraints used in the GADT constuctors.

_Ga = lGa; _Ga :: Gadt (Int, Bool)                   Simple.:~>     Integer
_Gb = lGb; _Gb :: Gadt (Int, Bool)                   Simple.:~>     Bool
_Gc = lGc; _Gc :: Gadt Bool                          Simple.:~>     Integer
_Gd = lGd; _Gd :: Gadt Bool                          Simple.:->     (Maybe Bool)
_Ge = lGe; _Ge :: (Gadt (a, b) -> Gadt (c, b))       Restricted.:~> (a -> c)
_Gf = lGf; _Gf :: (Gadt (a, b) -> Gadt (a, c))       Restricted.:~> (b -> c)
_Gg = lGg; _Gg :: (Gadt [a] -> Gadt [b])             Total.:->      (a -> b)
_Gh = lGh; _Gh :: (Gadt (a, a, a) -> Gadt (b, b, b)) Total.:->      ([a] -> [b])

-}


manual_ge :: (Alternative m, Alternative n)
          => Label m n (Gadt (a, b) -> Gadt (c, b)) (a -> c)
manual_ge = Core.make
  (\case C3 a _ -> pure a
         _      -> empty
  )
  (\ m f -> case f of
      C3 a b -> C3 <$> m a <*> pure b
      C1 _ _ -> empty
      C7 _ _ -> empty
      C8 _ _ -> empty
  )

-- Test data type with large number (> 26) of fields.

label [d|

  data C = C { c_a :: (),  c_b :: (),  c_c  :: (),  c_d  :: (),  c_e  :: (),  c_f  :: ()
             , c_g :: (),  c_h :: (),  c_i  :: (),  c_j  :: (),  c_k  :: (),  c_l  :: ()
             , c_m :: (),  c_n :: (),  c_o  :: (),  c_p  :: (),  c_q  :: (),  c_r  :: ()
             , c_s :: (),  c_t :: (),  c_u  :: (),  c_v  :: (),  c_w  :: (),  c_x  :: ()
             , c_y :: (),  c_z :: (),  c_a0 :: (),  c_b0 :: (),  c_c0 :: (),  c_d0 :: ()
             }

  |]

-- More complicated label views.

label [d|

  data View2 a
    = Con1 { field1 :: Bool
           , field2 :: (a, a)
           }
    | Con2 { field3 :: [a]
           , field1 :: Bool
           }
    deriving (Eq, Show)

  |]

-- Most obvious view might not work as expected because field1 is total and will never fail.

view1 :: (MonadPlus m, Alternative m) => Mono (Label m m) (View2 a) (Bool, Either (a, a) [a])
view1 = view $ (,) <$> Base.fstL >- field1
                   <*> Base.sndL >- view ( Right <$> Base.rightL >- field3
                                       <|> Left  <$> Base.leftL  >- field2
                                         )

field3_explicitlyPartial
  :: (Alternative n, Alternative m)
  => Core.Label m n (View2 a -> View2 b) ([a] -> [b])

field3_explicitlyPartial = Core.make
  ( \case Con1 {}  -> empty
          Con2 a _ -> pure a
  )
  ( \m -> \case Con1 _ _ -> empty
                Con2 a b -> Con2 <$> m a <*> pure b
  )

view2 :: (MonadPlus m, Alternative m) => Mono (Label m m) (View2 a) (Bool, Either (a, a) [a])
view2 = view $ (,) <$> Base.fstL >- field1
                   <*> Base.sndL >- view sub

  where sub = Right <$> Base.rightL >- field3_explicitlyPartial
          <|> Left  <$> Base.leftL  >- field2

-------------------------------------------------------------------------------

tests :: Test
tests = TestList
  [ mono
  , totalMono
  , partialMono
  , totalPoly
  , partialPoly
  , compositionTotal
  , compositionPartial
  , convertingPartial
  , applicativeTotal
  , applicativePartial
  , bijections
  , monadic
  , base
  ]

mono :: Test
mono = TestList
  [ eq "get manual_recA_m" (get manual_recA r0) 0
  , eq "set manual_recA_m" (set manual_recA 1 r0) r1
  , eq "mod manual_recA_m" (modify manual_recA (+ 1) r0) r1
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("tot mono - " ++ x)
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 1 Nothing (Newtype []) (Left 1)

totalMono :: Test
totalMono = TestList
  [ eq "get recA" (Total.get recA r0) 0
  , eq "set recA" (Total.set recA 1 r0) r1
  , eq "mod recA" (Total.modify recA (+ 1) r0) r1
  , eq "get manual_recA" (Total.get manual_recA r0) 0 
  , eq "set manual_recA" (Total.set manual_recA 1 r0) r1
  , eq "mod manual_recA" (Total.modify manual_recA (+ 1) r0) r1
  , eq "get multiB" (Total.get multiB f0) 0
  , eq "set multiB" (Total.set multiB 1 f0) first1
  , eq "mod multiB" (Total.modify multiB (+ 1) f0) first1
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("tot mono - " ++ x)
          f0 = First r0 0.0 (Right 1.0)
          first1 = First r0 1.0 (Right 1.0)
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 1 Nothing (Newtype []) (Left 1)

partialMono :: Test
partialMono = TestList
  [ eq0 "get multiA"        (getP multiA f0) (Just r0)
  , eq0 "set multiA"        (setP multiA r1 f0) f1
  , eq0 "mod multiA"        (modifyP multiA (Total.modify recA (+ 1)) f0) (f1)
  , eq0 "get manual_multiA" (getP manual_multiA f0) (Just r0)
  , eq0 "set manual_multiA" (setP manual_multiA r1 f0) f1
  , eq0 "mod manual_multiA" (modifyP manual_multiA (Total.modify recA (+ 1)) f0) f1
  , eq1 "get multiA"        (getP multiA s0) Nothing
  , eq1 "set multiA"        (setP multiA r1 s0) s0
  , eq1 "mod multiA"        (modifyP multiA (Total.modify recA (+ 1)) s0) s0
  , eq1 "get manual_multiA" (getP manual_multiA s0) Nothing
  , eq1 "set manual_multiA" (setP manual_multiA r1 s0) s0
  , eq1 "mod manual_multiA" (modifyP manual_multiA (Total.modify recA (+ 1)) s0) s0
  ] where eq0, eq1 :: (Eq a, Show a) => String -> a -> a -> Test
          eq0 x = equality ("partial mono - " ++ x)
          eq1 x = equality ("partial mono fail - " ++ x)
          s0 = Second 0.0
          f0 = First r0 0.0 (Right 1.0)
          f1 = First r1 0.0 (Right 1.0)
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 1 Nothing (Newtype []) (Left 1)

totalPoly :: Test
totalPoly = TestList
  [ eq "get dir" (Total.get dir n0) (0 :: Integer)
  , eq "set dir" (Total.set dir False n0) n1
  , eq "mod dir" (Total.modify dir (> 1) n0) n1
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("tot mono - " ++ x)
          n0 :: Direction Integer () () () ()
          n0 = North 0 ()
          n1 :: Direction Bool () () () ()
          n1 = North False ()

partialPoly :: Test
partialPoly = TestList
  [ eq0 "get north" (getP north n0) (Just ())
  , eq0 "set north" (setP north False n0) n1
  , eq0 "mod north" (modifyP north (> ()) n0) n1
  , eq1 "get north" (getP north w0) Nothing
  , eq1 "set north" (setP north False w0) (West 0 ())
  , eq1 "mod north" (modifyP north (> ()) w0) (West 0 ())
  ] where eq0, eq1 :: (Eq a, Show a) => String -> a -> a -> Test
          eq0 x = equality ("partial poly - " ++ x)
          eq1 x = equality ("partial poly fail - " ++ x)
          n0 :: Direction Integer () () () ()
          n0 = North 0 ()
          n1 :: Direction Integer Bool () () ()
          n1 = North 0 False
          w0 :: Direction Integer () () () ()
          w0 = West 0 ()

compositionTotal :: Test
compositionTotal = TestList
  [ eq0 "get id"          (Total.get id a) a
  , eq0 "set id"          (Total.set id b a) b
  , eq0 "mod id"          (Total.modify id (const b) a) b
  , eq0 "get comp tot"    (Total.get comp a) [True]
  , eq0 "set comp tot"    (Total.set comp [False] a) b
  , eq0 "mod comp tot"    (Total.modify comp (map not) a) b
  , eq0 "get id comp tot" (Total.get idc a) [True]
  , eq0 "set id comp tot" (Total.set idc [False] a) b
  , eq0 "mod id comp tot" (Total.modify idc (map not) a) b
  , eq0 "get comp tot id" (Total.get cid a) [True]
  , eq0 "set comp tot id" (Total.set cid [False] a) b
  , eq0 "mod comp tot id" (Total.modify cid (map not) a) b
  ] where eq0 :: (Eq a, Show a) => String -> a -> a -> Test
          eq0 x = equality ("composition partial mono - " ++ x)
          a = Record 0 Nothing (Newtype [True ]) (Left 1)
          b = Record 0 Nothing (Newtype [False]) (Left 1)
          comp :: Record Simple.:-> [Bool]
          comp = unNewtype . recC
          cid = comp . id
          idc = id . comp

compositionPartial :: Test
compositionPartial = TestList
  [ eq0 "getP id"           (getP id f0) (Just f0)
  , eq0 "setP id"           (setP id f1 f0) f1
  , eq0 "modP id"           (modifyP id (const f1) f0) f1
  , eq0 "getP comp part"    (getP comp f0) (Just 0)
  , eq0 "setP comp part"    (setP comp 1 f0) f1
  , eq0 "modP comp part"    (modifyP comp (+ 1) f0) f1
  , eq0 "getP id comp part" (getP idc f0) (Just 0)
  , eq0 "setP id comp part" (setP idc 1 f0) f1
  , eq0 "modP id comp part" (modifyP (idc) (+ 1) f0) f1
  , eq0 "getP comp part id" (getP cid f0) (Just 0)
  , eq0 "setP comp part id" (setP cid 1 f0) f1
  , eq0 "modP comp part id" (modifyP cid (+ 1) f0) f1
  ] where eq0 :: (Eq a, Show a) => String -> a -> a -> Test
          eq0 x = equality ("composition partial mono - " ++ x)
          comp :: Multi :~> Integer
          comp = recA . multiA
          cid = comp . id
          idc = id . comp
          f0 = First r0 0.0 (Right 1.0)
          f1 = First r1 0.0 (Right 1.0)
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 1 Nothing (Newtype []) (Left 1)

convertingPartial :: Test
convertingPartial = TestList
  [ eq0 "get r0" (getP lab r0) Nothing
  , eq0 "get r1" (getP lab r1) (Just (Newtype [False]))
  , eq0 "set r0" (setP lab (Newtype [False]) r0) r0
  , eq0 "set r1" (setP lab (Newtype [True]) r1) r2
  , eq0 "mod r0" (modifyP (unNewtype . lab) (map not) r0) r0
  , eq0 "mod r1" (modifyP (unNewtype . lab) (map not) r1) r2
  ] where eq0 :: (Eq a, Show a) => String -> a -> a -> Test
          eq0 x = equality ("converting partial mono - " ++ x)
          lab :: Record :~> Newtype Bool
          lab = embed recB_simple
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 0 (Just (Newtype [False])) (Newtype []) (Left 1)
          r2 = Record 0 (Just (Newtype [True])) (Newtype []) (Left 1)

applicativeTotal :: Test
applicativeTotal = TestList
  [ eq "get vA"        (Total.get (viewA . recordView) r0) Nothing
  , eq "get vB"        (Total.get (viewB . recordView) r0) (Left 1)
  , eq "get vC"        (Total.get (viewC . recordView) r0) nt0
  , eq "set vA"        (Total.set (viewA . recordView) (Just nt0) r2) r3
  , eq "modify vA"     (Total.modify (viewA . recordView) (fmap (const nt0)) r2) r3
  , eq "get newtypeId" (Total.get newtypeId nt0) nt0
  , eq "set newtypeId" (Total.set newtypeId nt1 nt0) nt1
  , eq "mod newtypeId" (Total.modify newtypeId (const nt2) nt0) nt2
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("applicative tot mono - " ++ x)
          recordView :: Record Simple.:-> View
          recordView = Core.view $
            View <$> viewA >- recB
                 <*> viewB >- recD
                 <*> viewC >- recC
          r0 = Record 0 Nothing nt0 (Left 1)
          r2 = Record 0 (Just nt1) nt0 (Left 1)
          r3 = Record 0 (Just nt0) nt0 (Left 1)
          nt0 = Newtype []
          nt1 = Newtype [True]
          nt2 = Newtype [False]

applicativePartial :: Test
applicativePartial = TestList
  [ eq "get app part" (getR (Base.leftL . Base.sndL . view1) c1) (Just ('a', 'z'))
  , eq "get app part" (getR (Base.leftL . Base.sndL . view1) c2) Nothing
  , eq "get app part" (getR (Base.rightL . Base.sndL . view1) c1) Nothing
  , eq "get app part" (getR (Base.rightL . Base.sndL . view1) c2) (Just "abc")
  , eq "mod app part" (modifyR (Base.fstL . view1) not c1) (Just (Con1 True ('a', 'z')))
  , eq "mod app part" (modifyR (Base.fstL . view1) not c2) (Just (Con2 "abc" False))
  , eq "set app part" (setR (Base.fstL . view1) True c1) (Just (Con1 True ('a', 'z')))
  , eq "set app part" (setR (Base.fstL . view1) False c2) (Just (Con2 "abc" False))
  , eq "mod app part" (modifyR (Base.leftL . Base.sndL . view1) swap c1) (Just (Con1 False ('a', 'z')))
  , eq "mod app part" (modifyR (Base.leftL . Base.sndL . view2) swap c1) (Just (Con1 False ('z', 'a')))
  , eq "mod app part" (modifyR (Base.leftL . Base.sndL . view1) swap c2) (Just (Con2 "abc" True))
  , eq "mod app part" (modifyR (Base.rightL . Base.sndL . view1) reverse c1) (Just (Con1 False ('a','z')))
  , eq "mod app part" (modifyR (Base.rightL . Base.sndL . view1) reverse c2) (Just (Con2 "cba" True))
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("applicative partial mono - " ++ x)
          c1 :: View2 Char
          c1 = Con1 False ('a', 'z')
          c2 :: View2 Char
          c2 = Con2 "abc" True

bijections :: Test
bijections = TestList
  [ eq "get md"    (get (iso md md . recA) r0) 0
  , eq "set md"    (set (iso md md . recA) 1 r0) r10
  , eq "mod md"    (modify (iso md md . recA) (+ 1) r0) r10
  , eq "get as"    (get (iso (inv as) (inv as) . recA) r0) (-10)
  , eq "set as"    (set (iso (inv as) (inv as) . recA) 1 r0) r11
  , eq "mod as"    (modify (iso (inv as) (inv as) . recA) (+ 1) r0) r1
  , eq "get id md" (get (iso (id . md) (id . md) . recA) r0) 0
  , eq "set id md" (set (iso (id . md) (id . md) . recA) 1 r0) r10
  , eq "mod id md" (modify (iso (id . md) (id . md) . recA) (+ 1) r0) r10
  , eq "get id md" (get (iso (md . id) (md . id) . recA) r0) 0
  , eq "set id md" (set (iso (md . id) (md . id) . recA) 1 r0) r10
  , eq "mod id md" (modify (iso (md . id) (md . id) . recA) (+ 1) r0) r10
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("isomorphisms mono - " ++ x)
          md :: (Applicative n, Applicative m) => Iso m n Integer Double
          md = Iso (\i -> pure (fromInteger i / 10)) (\i -> pure (round (i * 10)))
          as :: (Applicative n, Applicative m) => Iso m n Double Integer
          as = Iso (\i -> pure (round (i + 10))) (\i -> pure (fromInteger i - 10))
          r10 = Record 10 Nothing (Newtype []) (Left 1)
          r11 = Record 11 Nothing (Newtype []) (Left 1)
          r0 = Record 0 Nothing (Newtype []) (Left 1)
          r1 = Record 1 Nothing (Newtype []) (Left 1)

monadic :: Test
monadic = TestList
  [ eq "asks id tot"           (runReader (Monadic.asksL id) r0) r0
  , eq "asks recC tot"         (runReader (Monadic.asksL recC) r0) nt0
  , eq "gets id tot"           (evalState (Monadic.getsL id) r0) r0
  , eq "gets recC tot"         (evalState (Monadic.getsL recC) r0) nt0
  , eq "local recA tot"        (runReader (Monadic.localL recA (+1) $ Monadic.asksL id) r0) r1
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("tot monadic - " ++ x)
          r0 = Record 0 Nothing nt0 (Left 1)
          r1 = Record 1 Nothing nt0 (Left 1)
          nt0 = Newtype []

base :: Test
base = TestList
  [ eq "get head"  (getP Base.headL [1, 2, 3]) (Just (1::Int))
  , eq "get head"  (getP Base.headL ([] :: [Int])) Nothing
  , eq "get tail"  (getP Base.tailL [1, 2, 3]) (Just [2, 3 ::Int])
  , eq "get tail"  (getP Base.tailL ([] :: [Int])) Nothing
  , eq "get at"    (getP (Base.atL 1) "hallo") (Just 'a')
  , eq "get at"    (getP (Base.atL 5) "hallo") Nothing
  , eq "get left"  (getP Base.leftL (Left 'a')) (Just 'a')
  , eq "get left"  (getP Base.leftL (Right 'a' :: Either () Char)) Nothing
  , eq "get right" (getP Base.rightL (Right 'a')) (Just 'a')
  , eq "get right" (getP Base.rightL (Left 'a' :: Either Char ())) Nothing
  , eq "get just"  (getP Base.justL (Just 'a')) (Just 'a')
  , eq "get just"  (getP Base.justL (Nothing :: Maybe Char)) Nothing
  , eq "get fst"   (Total.get (Base.fstL . Core.iso Base.swapL Base.swapL) ('a', ())) ()
  , eq "get snd"   (Total.get (Base.sndL . Core.iso Base.swapL Base.swapL) ((), 'b')) ()
  , eq "get fst3"  (Total.get Base.fst3L ('a', (), ())) 'a'
  , eq "get snd3"  (Total.get Base.snd3L ((), 'b', ())) 'b'
  , eq "get trd3"  (Total.get Base.trd3L ((), (), 'c')) 'c'
  , eq "mod head"  (modifyP Base.headL (*2) [1, 2, 3]) [2, 2, 3::Int]
  , eq "mod head"  (modifyP Base.headL (*2) ([]::[Int])) []
  , eq "mod tail"  (modifyP Base.tailL reverse [1, 2, 3]) [1, 3, 2::Int]
  , eq "mod tail"  (modifyP Base.tailL reverse ([]::[Int])) []
  , eq "mod at"    (modifyP (Base.atL 1) (const '1') "hallo") "h1llo"
  , eq "mod at"    (modifyP (Base.atL 5) (const '1') "hallo") "hallo"
  , eq "mod left"  (modifyP Base.leftL (=='a') (Left 'a')) (Left True :: Either Bool ())
  , eq "mod left"  (modifyP Base.leftL (=='a') (Right ())) (Right ())
  , eq "mod right" (modifyP Base.rightL (=='c') (Right 'b')) (Right False :: Either () Bool)
  , eq "mod right" (modifyP Base.rightL (=='c') (Left ())) (Left ())
  , eq "mod just"  (modifyP Base.justL (=='a') (Just 'a')) (Just True)
  , eq "mod just"  (modifyP Base.justL (=='a') Nothing) Nothing
  , eq "mod fst"   (Total.modify (Base.fstL . Core.iso Base.swapL Base.swapL) (== 'a') ((), 'a')) ((), True)
  , eq "mod snd"   (Total.modify (Base.sndL . Core.iso Base.swapL Base.swapL) (== 'a') ('a', ())) (True, ())
  , eq "mod fst3"  (Total.modify Base.fst3L (== 'a') ('a', (), ())) (True, (), ())
  , eq "mod snd3"  (Total.modify Base.snd3L (== 'a') ((), 'b', ())) ((), False, ())
  , eq "mod trd3"  (Total.modify Base.trd3L (== 'a') ((), (), 'c')) ((), (), False)
  ] where eq :: (Eq a, Show a) => String -> a -> a -> Test
          eq x = equality ("base - " ++ x)

equality :: (Eq a, Show a) => String -> a -> a -> Test
equality d a b = TestCase (assertEqual d b a)
