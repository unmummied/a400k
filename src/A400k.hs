module A400k (a400k) where

a400k :: [Int]
a400k = map nth [1 ..]

nth :: Int -> Int
nth n = head $ filter (\h -> countSees h prevs == h) [0 ..]
  where prevs = take (n - 1) a400k

countSees :: Int -> [Int] -> Int
countSees hr hls = length . filter hrSees $ map (`drop` hls) [0 .. n - 1]
  where
    hrSees (hl : hms) = (hl `isSeenFrom` hr) hms
    n                 = length hls

-- | Test whether the right building sees the left building.
--
-- The left and right buildings \(L\) and \(R\) are located at \(x_L\) and \(x_R\) and have heights of \(h_L\) and \(h_R\), respectively.
-- The \(i\)-th intermediate building \(M_i\) is located at \(x_{M_i}\) and has a height of \(h_{M_i}\).
--
-- \(R\) sees \(L\) if every \(M_i\) has its top \((x_{M_i}, h_{M_i})\) on or below the straight line joining \((x_L, h_L)\) and \((x_R, h_R)\).
isSeenFrom :: Int -> Int -> [Int] -> Bool
(hl `isSeenFrom` hr) hms = and $ zipWith onOrBelow [1 ..] hms
  where
    onOrBelow diff hm = (hm - hl) * width <= (hr - hl) * diff
    width             = 1 + length hms
