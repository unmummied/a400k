module Build where

import Text.Printf (printf)

import A400k

cityUpto :: Int -> IO ()
cityUpto n = mapM_ putStrLn $ zipWith build [1..n] a400k

build :: Int -> Int -> String
build n h = unwords [n', h', replicate h '-']
  where
    n' = printf "%4d" n
    h' = printf "(%2d)" h
