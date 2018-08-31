module Diamond (diamond) where

import Data.Char (ord, chr, isAsciiUpper)

diamond :: Char -> Maybe [String]
diamond c
  | c >= 'A' && c <= 'Z' = Just $ prettyDiamond <$> diamondD (D c)
  | otherwise = Nothing

newtype D = D Char

instance Enum D where
  toEnum n
   | n > 0 && n <= 26 = D $ chr (64 + n)
   | otherwise = D ' '
  fromEnum (D c)
    | isAsciiUpper c = (ord c) - 64
    | otherwise      = 0

instance Show D where
  show (D c) = show c

prettyDiamond :: [D] -> String
prettyDiamond ((D d):ds) = d : prettyDiamond ds
prettyDiamond [] = []

diamondD :: D -> [[D]]
diamondD d = (fmap . fmap) toEnum (diamondN (fromEnum d))

stairN :: Int -> [[Int]]
stairN 0 = []
stairN n = rightPad <$> zipWith (++) spacers (pure <$> [1..n])
 where
  rightPad s = s ++ (replicate (n - (length s)) 0)
  spacers = reverse $ (\x -> replicate x 0) <$> [0..(n-1)]

pyramidN :: Int -> [[Int]]
pyramidN n = mirror <$> stairN n

diamondN :: Int -> [[Int]]
diamondN n = mirror $ pyramidN n

mirror :: [a] -> [a]
mirror [] = []
mirror xs = xs ++ (reverse . init) xs

putStrLns :: [String] -> IO ()
putStrLns (x:xs) = putStrLn x >> putStrLns xs
putStrLns [] = return ()
