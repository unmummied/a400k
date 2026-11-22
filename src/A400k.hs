module A400k (a400k) where

-- | OEIS A400000, The city-building sequence.
a400k :: [Int]
a400k = map heightOf [1 ..]

-- | The height \(h\) of the shortest buildings which can see exactly \(h\) left buildings.
heightOf :: Int -> Int
heightOf n | not (1 <= n) = undefined
           | otherwise    = head $ filter (\h -> countVisibles h lefts == h) [0 ..]
  where lefts = take (n - 1) a400k

-- | Returns the number of buildings, among the left buildings, visible from the right building.
countVisibles :: Int -> [Int] -> Int
countVisibles _  []             = 0
countVisibles hr hls@(_ : mids) = length $ filter ((hr `sees`) . (`drop` hls)) [0 .. n]
  where n = length mids

-- | Test whether the right building sees the left building.
--
-- The left and right buildings \(L\) and \(R\) are located at \(x_L\) and \(x_R\) and have heights of \(h_L\) and \(h_R\), respectively.
-- The \(i\)-th intermediate building \(M_i\) is located at \(x_{M_i}\) and has a height of \(h_{M_i}\).
--
-- \(R\) sees \(L\) if every \(M_i\) has its top \((x_{M_i}, h_{M_i})\) on or below the straight line joining \((x_L, h_L)\) and \((x_R, h_R)\).
sees :: Int -> [Int] -> Bool
right `sees` lefts@(left : mids) = and $ zipWith onOrBelow [1 ..] mids
  where
    onOrBelow dist mid = (mid - left) * width <= (right - left) * dist
    width              = length lefts
