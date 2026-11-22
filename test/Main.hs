module Main (main) where

import Data.Foldable

import A400k
import System.Exit

limit = 1000

main :: IO ()
main = do
    bFile <- readFile "data/b400000.txt"

    let expected = take limit . map (read . (!! 1) . words) $ lines bFile
        actual   = take limit a400k

    let test = find (\(_, e, a) -> e /= a) $ zip3 [1 ..] expected actual
    case test of
        Nothing  -> exitSuccess
        Just nae -> do
            putStrLn $ "Mismatch found (n, expected a(n), actual a(n)) = " ++ show nae
            exitFailure
