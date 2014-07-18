{-# LANGUAGE ParallelListComp #-}
module Factorial
    ( main
    , f1
    , f2
    ) where
    
import Prelude
import Control.Applicative
import Criterion.Main
import Data.Array.Repa

f1 :: Integer -> Maybe Integer
f1 n | n < 0 = Nothing
     | otherwise = Just $ (drop (fromIntegral n) f1l) !! 1
                 
f1l :: [Integer]
f1l = 1 : 1 : [x*y | x <- [2..] | y <- (drop 1 f1l)]

f2 :: Integer -> Integer
f2 0 = 1
f2 n = n * f2 (n-1)

main :: IO ()
main = defaultMain [
       bgroup "f1" [ bench "1000" $ nf f1 1000
                   , bench "10000" $ nf f1 10000
                   , bench "90000" $ nf f1 90000
                   , bench "100000" $ nf f1 100000
                 --  , bench "200000" $ nf f1 200000
                   ]
       bgroup "f2" [ bench "1000" $ nf f2 1000
                   , bench "10000" $ nf f2 10000
                   , bench "100000" $ nf f2 100000
                 --  , bench "200000" $ nf f2 200000
                   ]
                   ]
{-
factorial :: Integer -> Maybe Integer
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial n | n < 0     = Nothing
            | otherwise = Just $ initFactorial

initFactorial :: Integer -> Integer
initFactorial = undefined-}
