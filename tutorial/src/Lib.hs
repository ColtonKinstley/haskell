module Lib
  ( someFunc
  , is_elem
  , cknub
  , cknub2
  , isAsc
  , hasPath
  , tickets
  , isTriangle
  , getSum
  , findSmallestInteger
  , printerError
  , helper
  , productFib
  , sumFracts
  , sumFracts2
  , reduce
  , euclid
  , sumHelper
  , gimme
  , rowSumOddNumbers
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

is_elem :: (Eq a) => a -> [a] -> Bool
is_elem x list | null list      = False
               | x == head list = True
               | otherwise      = is_elem x (tail list)

cknub :: (Eq a) => [a] -> [a]
cknub list = aux list []
 where
  aux list deduped
    | null list                         = deduped
    | is_elem (head list) deduped       = aux (tail list) deduped
    | not (is_elem (head list) deduped) = aux (tail list) (head list : deduped)


cknub2 :: (Eq a) => [a] -> [a]
cknub2 [] = []
cknub2 (x : xs) | is_elem x xs = xs
                | otherwise    = x : cknub2 xs

isAsc :: [Int] -> Bool
-- isAsc [] = True
isAsc (x : xs) | null xs     = True
               | x > head xs = False
               | otherwise   = isAsc xs

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = False
hasPath edges start end
  | start == end
  = True
  | not (null [ x | x <- edges, x == (start, end) ])
  = True
  | otherwise
  = not (null current_edges)
    && (  hasPath [ x | x <- edges, x /= x' ] (snd x') end
       || hasPath [ x | x <- edges, x /= x' ] start    end
       )
 where
  current_edges = [ x | x <- edges, fst x == start ]
  x'            = head current_edges

ck_reverse :: [Int] -> [Int]
ck_reverse = go []
 where
  go acc []       = acc
  go acc (x : xs) = go (x : acc) xs

type Money = Int
data CanHe = NO | YES deriving (Show,Eq)

tickets :: [Money] -> CanHe
tickets = go (0, 0)
 where
  go acc []        = YES
  go acc (25 : xs) = go (fst acc + 1, snd acc) xs
  go acc (x : xs) | fst acc < 1              = NO
                  | x == 50                  = go (fst acc - 1, snd acc + 1) xs
                  | x == 100 && snd acc > 0  = go (fst acc - 1, snd acc - 1) xs
                  | fst acc < 3              = NO
                  | x == 100 && snd acc <= 0 = go (fst acc - 3, snd acc) xs

isTriangle :: Int -> Int -> Int -> Bool
isTriangle a b c = (sum l' > maximum l || length l' < 2) && minimum l > 0
 where
  l  = a : b : c : []
  l' = [ x | x <- l, x < maximum l ]

getSum :: Int -> Int -> Int
getSum a b | a == b         = a
           | b < a          = getSum b a
           | b <= 0         = (-1) * getSum ((-1) * b) ((-1) * a)
           | a < 0 && b > 0 = (getSum 0 b) + getSum a 0
           | a >= 0         = tri b - tri (a - 1)
  where tri c = div (c * (c + 1)) 2

findSmallestInteger :: [Int] -> Int
findSmallestInteger (x : []) = x
findSmallestInteger (x : y : xs) | x <= y = findSmallestInteger (x : xs)
                                 | x > y  = findSmallestInteger (y : xs)

printerError :: [Char] -> [Char]
printerError = helper 0 0

helper :: Int -> Int -> [Char] -> [Char]
helper n d [] = show n ++ "/" ++ show d
helper n d (s : ss) | elem s allowed       = helper (n + 1) (d + 1) ss
                    | not (elem s allowed) = helper n (d + 1) ss
  where allowed = ['a' .. 'm']

productFib :: Integer -> (Integer, Integer, Bool)
productFib = f 1 1
 where
  f a b n | a * b < n  = f b (a + b) n
          | a * b == n = (a, b, True)
          | a * b > n  = (a, b, False)


sumFracts :: [(Integer, Integer)] -> Maybe String
sumFracts []           = Nothing
sumFracts (x     : []) = Just $ reduce x
sumFracts (x : y : xs) = sumFracts $ (sumHelper x y) : xs

reduce :: (Integer, Integer) -> String
reduce (x, y) | y `div` gcd == 1 = show (x `div` gcd)
              | otherwise = show (x `div` gcd) ++ " " ++ show (y `div` gcd)
  where gcd = euclid x y

euclid :: Integer -> Integer -> Integer
euclid a b | a < b = euclid b a
euclid a b | snd (divMod a b) == 0 = b
           | otherwise             = euclid b $ snd $ divMod a b

sumHelper :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
sumHelper x y = (a * d + c * b, b * d)
 where
  a = fst x
  b = snd x
  c = fst y
  d = snd y

sumFracts2 :: [(Integer, Integer)] -> (Integer, Integer)
sumFracts2 [] = (0, 1)
sumFracts2 l  = reduce unreduced $ gcd unreduced where
  gcd (a, b) = if r == 0 then b else gcd (b, r) where
      r = snd $ divMod a b
  unreduced = foldl (\(a, b) (c, d) -> (d * a + b * c, d * b)) (0, 1) l
  reduce (a, b) d = (div a d, div b d)

gimme :: Ord a => (a, a, a) -> Int
gimme = go 0
    where go n (a, b, c) 
            | (b <= a && a <= c) || (c <= a && a <= b) = n
            | otherwise = go (n+1) (b, c, a)


rowSumOddNumbers :: Integer -> Integer
rowSumOddNumbers n = (div (n*(n+1)) 2)^2 - (div (n*(n-1)) 2)^2

-- isPangram :: String -> Bool
-- isPangram str = go "abcdef"
