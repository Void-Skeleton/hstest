{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
import ArrayL
import GHC.Exts
import PreludeL
import Unrestricted
import System.Random
import System.TimeIt

quicksortM :: Ord a => [a] -> [a]
quicksortM ls = unur $ let size@(I# size#) = length ls in
    runMArray size# undefined (\arr ->
        arr -&# initMArray ls
        -&# quicksortM' 0# size#
        -&# dumpM)


quicksortM' :: (Ord a) => Int# -> Int# -> MArray a %1 -> MArray a
quicksortM' from# to# arr = case to# -# from# <=# 1# of
    1# -> arr
    0# -> arr -&# readM (to# -# 1#)
          =&# \pivot arr -> pivotM from# (to# -# 1#) from# pivot arr
          =&# \(I# pos#) arr -> swapM pos# (to# -# 1#) arr
          -&# quicksortM' from# pos#
          -&# quicksortM' (pos# +# 1#) to#

pivotM :: (Ord a) => Int# -> Int# -> Int# -> a -> MArray a %1 -> (# Ur Int, MArray a #)
pivotM curr# to# acc# pivot arr = case curr# ==# to# of
    1# -> (# ur (I# acc#) , arr #)
    0# ->
        arr -&# readM curr#
        =&# \val arr ->
            if val <= pivot
            then pivotM (curr# +# 1#) to# (acc# +# 1#) pivot (swapM curr# acc# arr)
            else pivotM (curr# +# 1#) to# acc# pivot arr

swapM :: Int# -> Int# -> MArray a %1 -> MArray a
swapM i# j# arr =
    arr
    -&# readM i#
    =&# \vi arr -> readM j# arr
    =&# writeM i#
    -&# writeM j# vi

isSorted :: Ord a => [a] -> Bool
isSorted [x] = True
isSorted (a:b:xs) = a <= b && isSorted (b:xs)
isSorted [] = undefined

main :: IO ()
main = do
    let size = 10 ^ 7
    gen <- newStdGen
    let ls :: [Int] = take size $ randoms gen
    print $ sum ls {- This yeets all the laziness out of the list ls -}
    timeIt $ do
        print "A"
        print $ isSorted (quicksortM ls) {- Once again, yeets all laziness -}
        print "B"