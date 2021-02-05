import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, sortBy, maximumBy)
import Data.Ord (comparing)
import Data.Char (ord, chr)
import Text.Printf (printf)
import System.IO (hPutStrLn, stderr)

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

letterFreqString = "ETAOINSHRDLCUMWFGYPBVKJXQZ"

-- join string into trigrams
trigrams :: String -> [String]
trigrams (x:y:z:xs) = [x,y,z]:(trigrams ([y,z]++xs))
trigrams _ = []

-- make a map of frequencies of elements in a list
freqMap :: (Ord a) => [a] -> Map a Int
freqMap = foldl (\acc x -> Map.insertWith (+) x 1 acc) Map.empty 

-- get positions of trigrams in a string
triPos :: String -> String -> [Int]
triPos str tri = (map snd) . (filter $ (==tri) . fst) $ zip (trigrams str) [1..]

-- measure distances between elements of a list
listdist :: (Num a) => [a] -> [a]
listdist (x:y:xs) = (y-x):listdist (y:xs)
listdist _ = []

-- perform frequency analysis
freqAnal :: String -> [(Char,Double)]
freqAnal str = let freqs = Map.assocs . freqMap $ str
                   in map (\(x,n) -> (x,fromIntegral n / fromIntegral (length str))) freqs

-- split strings into mod n substrings for frequency analysis
freqAnalMod :: String -> Int -> [String]
freqAnalMod str n = [(map fst) . (filter $ (==t) . (`mod`n) . snd) $ zip str [0..] | t <- [0..n-1]]

-- find the mode of a set
mode :: (Ord a) => [a] -> a
mode = fst . last . (sortBy $ comparing snd) . Map.assocs . freqMap

-- something that treats negative mods correctly
mathmod :: Int -> Int -> Int
x `mathmod` n
    | y < 0 = y+n
    | otherwise = y
    where y = x `mod` n

-- get the element at the p/n'th position in a list
percentile :: Int -> [a] -> a
percentile p xs = xs!!(p*(length xs)`div`100)

-- decipher a vigenere cipher
vigenereDecipher :: String -> String -> String
vigenereDecipher message key = let ciphermap = zip message (cycle key)
                                  in map (chr . \(x,y) -> (ord x - ord y) `mathmod` 26 + 65) ciphermap

main = readFile "ciphertext.txt" >>= (\line -> do
    -- get the most frequent trigram
    let tris = trigrams line
    let toptri = fst . head . reverse . (sortBy $ comparing snd) . Map.assocs . freqMap $ tris
    putErrLn $ printf "The most frequently occurring trigram is %s" toptri
    -- get the most likely key length
    let dists = listdist $ triPos line toptri
    let maxgcd = percentile 5 $ sort dists
    putErrLn $ printf "The 5th percentile trigram distance is %d" maxgcd
    let bestgcd = maximumBy (comparing snd) [(n,((*100) . length . filter ((==0).(`mod`n)) $ dists)`div`(length dists)) | n <- [2..maxgcd]]
    putErrLn $ printf "The most likely key length is %d, matching %d%% of distances" (fst bestgcd) (snd bestgcd)
    -- do frequency analysis on every nth character with n in {0,...,bestgcd-1}
    let analysis = freqAnalMod line (fst bestgcd)
    -- determine the most likely ciphertext->plaintext letter mapping from the string of frequent letters
    let likelyciphers = map ((zip letterFreqString) . reverse . (map fst) . (sortBy $ comparing snd) . freqAnal) analysis
    -- determine the 'shift' of each letter
    let shifts = map (map (\(x,y) -> ord y - ord x)) likelyciphers
    -- the most likely shift is the one that appears most often
    let likelykey = map (chr . (+65) . (`mathmod`26) . mode) shifts
    putErrLn $ printf "Best guess key is %s" likelykey
    putErrLn "Writing deciphered text to stdout"
    putStrLn $ vigenereDecipher line likelykey
    )