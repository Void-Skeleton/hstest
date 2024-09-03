{-# LANGUAGE LinearTypes #-}
module PreludeL where
import GHC.Exts

(-$) :: forall {rep} a (b :: TYPE rep) p q. (a %p -> b) %q -> a %p -> b
(-$) f x = f x

(-&) :: forall {rep} a (b :: TYPE rep) p q. a %p -> (a %p -> b) %q -> b
(-&) x f = f x

(-&#) :: forall {rep} (a :: TYPE UnliftedRep) (b :: TYPE rep) p q. a %p -> (a %p -> b) %q -> b
(-&#) x f = f x

forget :: forall {rep} a (b :: TYPE rep) p. (a %p -> b) -> a -> b
forget f x = f x
