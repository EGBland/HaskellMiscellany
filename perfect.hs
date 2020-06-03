fac :: Int -> [Int]
fac n = [a | a <- [1..n-1],  n `mod` a == 0]

mynums = [(a,a - (sum $ fac a)) | a <- [1..10000]]

main = mapM_ print (filter (\x -> snd x == 0) mynums)
