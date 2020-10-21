data Number = Number {
    d1  :: [Int],
    d2  :: [Int],
    d3  :: [Int],
    d4  :: [Int],
    d5  :: [Int],
    d6  :: [Int],
    d7  :: [Int],
    d8  :: [Int],
    d9  :: [Int],
    d10 :: [Int]
    }

instance Show Number where
  show num = map (\x -> if x `elem` (d1 num) then (head $ show x) else ' ') [0..9]
-- some 3-tuple stuff
fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

trd3 :: (a,b,c) -> c
trd3 (_,_,z) = z


constraint1 :: Number -> Number
constraint1 = id

constraint2 :: Number -> Number
constraint2 num = let d'2 = filter (\x -> x `mod` 2 == 0) (d2 num)
                      in num {d2 = d'2}

constraint3 :: Number -> Number
constraint3 num = let dcandidates = zip3 (d1 num) (d2 num) (d3 num)
                      daccepted   = filter (\(x,y,z) -> (x+y+z) `mod` 3 == 0) dcandidates
                      d'1         = map fst3 daccepted
                      d'2         = map snd3 daccepted
                      d'3         = map trd3 daccepted
                      in num {d1 = d'1, d2 = d'2, d3 = d'3}

constraint4 :: Number -> Number
constraint4 num = let dcandidates = zip (d3 num) (d4 num)
                      daccepted   = filter (\(x,y) -> (10*x + y) `mod` 4 == 0) dcandidates
                      d'3         = map fst daccepted
                      d'4         = map snd daccepted
                      in num {d3 = d'3, d4 = d'4}

constraint5 :: Number -> Number
constraint5 num = let d'5 = filter (\x -> x == 5 || x == 0) (d5 num)
                      in num {d5 = d'5}

constraint8 :: Number -> Number
constraint8 num = let dcandidates = zip3 (d6 num) (d7 num) (d8 num)
                      daccepted   = filter (\(x,y,z) -> (100*x + 10*y + z) `mod` 8 == 0) dcandidates
                      d'6         = map fst3 daccepted
                      d'7         = map snd3 daccepted
                      d'8         = map trd3 daccepted
                      in num {d6 = d'6, d7 = d'7, d8 = d'8}

constraint10 :: Number -> Number
constraint10 num = num {d10 = [0]}

main = do
  let num = Number {
    d1  = [0..9],
    d2  = [0..9],
    d3  = [0..9],
    d4  = [0..9],
    d5  = [0..9],
    d6  = [0..9],
    d7  = [0..9],
    d8  = [0..9],
    d9  = [0..9],
    d10 = [0..9]
    }

  putStrLn $ show num