import Control.Monad

oddEvenSplit :: [a] -> ([a],[a])
oddEvenSplit [] = ([],[])
oddEvenSplit [x] = ([x],[])
oddEvenSplit (x1:x2:xs) = let (ro,re) = oddEvenSplit xs in (x1:ro,x2:re)

read' :: (Read a) => Char -> a
read' x = read [x]

checksum :: String -> Bool
checksum x = let (digo,dige) = oddEvenSplit $ reverse x
                 numo        = map read' digo :: [Int]
                 nume        = map read' dige :: [Int]
                 a           = sum numo
                 nume2       = map (show . (*2)) nume
                 b           = sum $ map read' $ concat nume2
                 in (a + b) `mod` 10 == 0

main = do
    n <- readLn :: IO Int
    replicateM_ n (getLine >>= (\line -> if checksum line then putStrLn "Yes" else putStrLn "No"))