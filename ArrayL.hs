{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant case" #-}
module ArrayL where

import GHC.Exts
import UnsafeL
import Unrestricted

newtype MArray a = MArray (MutableArray# RealWorld a)

readM :: Int# -> MArray a %1 -> (# Ur a, MArray a #)
readM i# = unsafeMultiplicity (go i#) where
    go :: Int# -> MArray a -> (# Ur a, MArray a #)
    go i# (MArray mut#) = case runRW# (readArray# mut# i#) of
        (# _ , ret #) -> (# Ur ret , MArray mut# #)

writeM :: Int# -> a -> MArray a %1 -> MArray a
writeM i# x = unsafeMultiplicity (go i# x) where
    go :: Int# -> a -> MArray a -> MArray a
    go i# x (MArray mut#) = case runRW# (writeArray# mut# i# x) of _ -> MArray mut#

runMArray :: Int# -> a -> (MArray a %1 -> Ur b) %1 -> Ur b
runMArray size# defval f = f marray where
    marray = case runRW# (newArray# size# defval) of (# _ , mut# #) -> MArray mut#

initMArray :: [a] -> MArray a %1 -> MArray a
initMArray arr = unsafeMultiplicity (initMArray' 0# arr)

initMArray' :: Int# -> [a] -> MArray a %1 -> MArray a
initMArray' i# [] marray = marray
initMArray' i# (a:as) marray = initMArray' (i# +# 1#) as (writeM i# a marray)

lengthM :: MArray a -> (# Int# , MArray a #)
lengthM (MArray mut#) = (# sizeofMutableArray# mut# , MArray mut# #)

dumpM :: MArray a %1 -> Ur [a]
dumpM = unsafeMultiplicity go' where
    go' :: MArray a -> Ur [a]
    go' arr@(MArray mut#) = ur (go 0# (sizeofMutableArray# mut#) arr)
    go :: Int# -> Int# -> MArray a -> [a]
    go i# len# (MArray mut#) = case i# ==# len# of
        1# -> []
        0# -> case runRW# (readArray# mut# i#) of
            (# _, ret #) -> ret : go (i# +# 1#) len# (MArray mut#)
