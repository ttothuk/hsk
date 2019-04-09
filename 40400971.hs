
-- QUESTION 1: Sets

-- 1 / A

complement :: (Eq a) => [a] -> [a] -> Maybe [a]
complement xs ys
  | filter (\x -> notElem x ys) xs /= [] = Nothing
  | otherwise = Just $ filter (\y -> notElem y xs) ys
  
-- 1 / B

toMultiset :: (Eq a) => [a] -> [(a, Int)]
toMultiset [] = []
toMultiset (x:xs) = (x, (+) 1 $ length $ filter (==x) xs) : toMultiset (filter (/=x) xs)

uniqueList :: (Eq a) => [a] -> [a]
uniqueList [] = []
uniqueList (x:xs) 
 | elem x xs == True = uniqueList xs
 | otherwise = x : uniqueList xs

-- 1 / C
mIntersect :: (Eq a) => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
mIntersect xs ys = [(n,min x y)|(n,x) <- xs, (m,y) <- ys, n==m]

myIntersection' :: (Eq a) => [a] -> [a] -> [a]
myIntersection' [] ys = []
myIntersection' (x:xs) ys 
        | elem x ys = x : myIntersection' xs ys 
        | otherwise = myIntersection' xs ys

myIntersection :: (Eq a) => [a] -> [a] -> [a]
myIntersection xs ys = uniqueList (myIntersection' xs ys)

-- TEST SET FOR Q1
{-
Your functions should have the following behaviour:
complement [1,2,3] [1..5] = Just [4,5]
complement [1,2,3] [2..5] = Nothing
toMultiset [1,1,1,2,4,1,2] = [(1,4),(2,2),(4,1)]
toMultiset "from each according to his ability, to each according to his needs" = [('f',1),('m',1),('b',1),('l',1),('y',1),(',',1),('a',5),('c',6),('r',3),('g',2),('t',4),('o',6),('h',4),('i',6),(' ',11),('n',3),('e',4),('d',3),('s',3)]
mIntersect [(1,6),(2,3)] [(1,2),(2,5),(3,1)] = [(1,2),(2,3)]
mIntersect [(1,2),(4,1)] [(1,1),(4,2)] = [(1,1),(4,1)]
 
THE ORDER OF ELEMENTS IN THE RESULTS OF mUnion IS NOT IMPORTANT.
-}

-- QUESTION 2: Functions and relations

transClosure :: (Eq a) => [(a,a)] -> [(a,a)]
transClosure xs = uniq (xs ++ [(a, c) | (a, b) <- xs, (b', c) <- xs, b == b'])
    where uniq [] = []
          uniq (x:xs) = x : uniq (filter (/=x) xs)

-- TEST SET FOR Q2
{-
Your functions should have the following behaviour:
transClosure [(1,2),(2,3)] = [(1,2),(2,3),(1,3)]
transClosure [(1,1),(3,5), (5,3)] = [(1,1),(3,5),(5,3),(3,3),(5,5)]

DO NOT WORRY ABOUT THE ORDER IN WHICH PAIRS APPEAR IN YOUR LIST
-}

-- QUESTION 3: Combinatorics
--https://rosettacode.org/wiki/Combinations#Haskell
missing2 :: [Int] -> [[Int]]
missing2 xs = subsets (length xs - 2) (quickSort xs)
  where
    quickSort [] = []
    quickSort (p : xs) = quickSort (filter (< p) xs) ++ [p] ++ quickSort (filter (>= p) xs)

subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs


-- TEST SET FOR Q3
{-
Your functions should have the following behaviour:
missing2 [1,2,3] = [[1],[2],[3]]
missing2 [2,6,9,12] = [[9,12],[6,12],[6,9],[2,12],[2,9],[2,6]]
NOTE THAT THE SMALLER LISTS ARE SORTED. THE ORDERING OF THE LISTS IN THE BIG LIST DOES NOT MATTER.
-}

-- QUESTION 4: Primes
-- 4 / A

--https://wiki.haskell.org/99_questions/Solutions/31
isPrime :: Int -> Bool  
isPrime n | n < 4 = n /= 1 
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor.sqrt $ fromIntegral n

--https://wiki.haskell.org/Prime_numbers
nextPrimes :: Int -> [Int]
nextPrimes n
    | isPrime n == False = take 3 (filter isPrime [n..])
    | otherwise = tail (take 4 (filter isPrime [n..]))


-- 4 / B
  
primeFactorisation :: Int -> [Int]
primeFactorisation n = primeFactorisationHelper 2  n
  
  
primeFactorisationHelper :: Int -> Int -> [Int]
primeFactorisationHelper _ 1 = [] 
primeFactorisationHelper d n 
    | d * d > n = [n]
    | n `mod` d == 0 = d : primeFactorisationHelper d (n `div` d)
    | otherwise = primeFactorisationHelper (d + 1) n

-- TEST SET FOR Q4
{-
Your functions should have the following behaviour:
nextPrimes 75 = [79,83,89]
nextPrimes 64 = [67,71,73]
primeFactorisation 75 = [3,5,5]
primeFactorisation 64 = [2,2,2,2,2,2]
-}


-- QUESTION 5: RSA

--5 / A
--https://wiki.haskell.org/99_questions/Solutions/34

eTotient :: Int -> Int
eTotient n = totient n

totient 1 = 1
totient a = length $ filter (coprime a) [1..a-1]
 where coprime a b = gcd a b == 1

factors :: Int -> [Int]
factors n = xs ++ [div n x | x <- reverse xs, x /= div n x] ++ [n]
    where xs = [x | x <- [2..floor(sqrt(fromIntegral n))], mod n x == 0]



-- 5 /B

encode :: Int -> Int -> Int -> Int -> Maybe Int
encode p q m e 
 | isNotPrime p || isNotPrime q || isNotCoprime e phi = Nothing
 | otherwise = Just (mod (m ^ e) n)
 where phi = eTotient n 
       n = p * q
       isNotPrime x = primeFactorisation x /= [x]
       isNotCoprime x y = [z | z <- (factors x), elem z (factors y)] /= []


-- TEST SET FOR Q5
{-
Your functions should have the following behaviour:
eTotient 54 = 18
eTotient 73 = 72
encode 37 23 29 5 = Just 347
encode 99 18 108 45 = Nothing
encode 37 17 23 48 = Nothing
-}