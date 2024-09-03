{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Unrestricted where

import Data.Coerce
import GHC.Exts
import UnsafeL

newtype Ur a = Ur { unur :: a }

{-# INLINE ur #-}
ur :: a -> Ur a
ur = Ur

{-# INLINE ($-) #-}
($-) :: (a %p -> b) %1 -> Ur a %1 -> b
($-) f = unsafeMultiplicity (\(Ur x) -> f x)

{-# INLINE (&-) #-}
(&-) :: Ur a %1 -> (a %p -> b) %1 -> b
(&-) x f = f $- x

{-# INLINE (=&) #-}
(=&) :: forall (rep :: RuntimeRep) a b (c :: TYPE rep) (p :: Multiplicity) (q :: Multiplicity). 
    (# Ur a , b #) %q -> (a %p -> b %q -> c) %1 -> c
(=&) (# ura , b #) f = (f $- ura) b

{-# INLINE (=&#) #-}
(=&#) :: forall (rep :: RuntimeRep) a (b :: TYPE UnliftedRep) (c :: TYPE rep) (p :: Multiplicity) (q :: Multiplicity). 
    (# Ur a , b #) %q -> (a %p -> b %q -> c) %1 -> c
(=&#) (# ura , b #) f = (f $- ura) b

