import Data.Map (Map)
import qualified Data.Map as Map

letterFreqString = "ETAOINSHRDLCUMWFGYPBVKJXQZ"
myFreqString     = "TXGYHVEANJCKISDPWRUBFZOLMQ"
theFreqMap = Map.fromList (zip myFreqString letterFreqString) :: Map Char Char

freqMap :: Char -> Char
freqMap x
    | x == ' ' = ' '
    | x == ',' = ','
    | otherwise = Map.findWithDefault '?' x theFreqMap

myCiphertext = "VX SGA XET BTAX YP XVITA, VX SGA XET SYNAX YP XVITA"

main = putStrLn $ map freqMap myCiphertext