{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
module UnsafeL where
import GHC.Exts
import Unsafe.Coerce(unsafeEqualityProof, UnsafeEquality(UnsafeRefl))

{-# INLINE unsafeLinearCoerce #-}
unsafeLinearCoerce :: a %1 -> b
unsafeLinearCoerce x = c unsafeEqualityProof x where
    c :: UnsafeEquality a b -> a %1 -> b
    c p x = case p of UnsafeRefl -> x

{-# INLINE unsafeMultiplicity #-}
unsafeMultiplicity :: forall (ra :: RuntimeRep) (rb :: RuntimeRep) (a :: TYPE ra) (b :: TYPE rb) 
    (p :: Multiplicity) (q :: Multiplicity). (a %p -> b) %1-> (a %q -> b)
unsafeMultiplicity = unsafeLinearCoerce

