import Data.List -- for nub

-- gets the list of prime factors of n.
primeFact :: Integer -> [Integer]
primeFact 1 = []
primeFact n
    | null factors  = [n]
    | otherwise     = factors ++ primeFact(div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2..n-1]

-- taken from Higemaru on stackoverflow, https://stackoverflow.com/questions/19554984/haskell-count-occurrences-function
-- gets the number of instances of x in the second argument list.
instanceCt :: Eq a => a -> [a] -> Int
instanceCt x = length.filter (x==)

-- gets the different valid p's for the Sylow p-subgroups of a group of order n.
sylowPs :: Integer -> [Integer]
sylowPs n
    | n < 2 = []
    | otherwise = nub $ primeFact n

-- gets the possible numbers of Sylow p-subgroups of a group of order n.
-- used to answer a question about whether or not simple groups of order n are possible - if only one Sylow p-subgroup exists for some p, then there exists a normal subgroup with order p^a, where a is the power of p in the prime decomposition of n.
sylowSubgpCt :: Integer -> Integer -> [Integer]
sylowSubgpCt p n
    | n < 2 = error "n should be at least 2."
    | not $ elem p $ pfs = error "p not a prime factor of n"
    | otherwise = [x | x<-[0..m], mod x p == 1, mod m x == 0]
    where m = div n (p^(instanceCt p pfs))
          pfs = primeFact n
