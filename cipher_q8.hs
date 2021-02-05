import Data.Char (ord, chr)

toCipher :: Char -> Int
toCipher = (flip (-) $ 97) . ord

fromCipher :: Int -> Char
fromCipher = chr . (+97)

main = (putStrLn . show . (zip ['a'..'z']) $ map (fromCipher . (`mod`26) . (+11) . (^2) . toCipher) $ ['a'..'z'])
    >> (putStrLn . show . (zip [0..25]) $ map ((`mod`26) . (+11) . (^2)) $ [0..25])