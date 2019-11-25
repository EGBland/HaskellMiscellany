primeFact :: Int -> [Int]
primeFact 1 = []
primeFact n
    | null factors  = [n]
    | otherwise     = factors ++ primeFact(div n (head factors))
    where factors = take 1 $ filter (\x -> (mod n x) == 0) [2..n-1]

-- taken from Higemaru on stackoverflow, https://stackoverflow.com/questions/19554984/haskell-count-occurrences-function
instanceCt :: Eq a => a -> [a] -> Int
instanceCt x = length.filter (x==)

sylowSubgpCt :: Int -> Int -> [Int]
sylowSubgpCt p n
    | n < 2 = error "n should be at least 2."
    | not $ elem p $ pfs = error "p not a prime factor of n"
    | otherwise = [x | x<-[0..m], mod x p == 1, mod m x == 0]
    where m = div n (p^(instanceCt p pfs))
          pfs = primeFact n
