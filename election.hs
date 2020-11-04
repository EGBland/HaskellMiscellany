import Control.Monad
import Data.List
import Data.Map (Map, insertWith, findWithDefault)
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord (comparing)
import System.Random
import Text.Printf

type Elector = (String,Int)
type State = String

theStates :: [Elector]
theStates = [
	("Alabama",9),
	("Alaska",3),
	("Arizona",11),
	("Arkansas",6),
	("California",55),
	("Colorado",9),
	("Connecticut",7),
	("D.C.",3),
	("Delaware",3),
	("Florida",29),
	("Georgia",16),
	("Hawaii",4),
	("Idaho",4),
	("Illinois",20),
	("Indiana",11),
	("Iowa",6),
	("Kansas",6),
	("Kentucky",8),
	("Louisiana",8),
	("MaineDrumpf",1),
	("MaineBooboo",3),
	("Maryland",10),
	("Massachusetts",11),
	("Michigan",16),
	("Minnesota",10),
	("Mississippi",6),
	("Missouri",10),
	("Montana",3),
	("NebraskaDrumpf",4),
	("NebraskaBooboo",1),
	("Nevada",6),
	("New Hampshire",4),
	("New Jersey",14),
	("New Mexico",5),
	("New York",29),
	("North Carolina",15),
	("North Dakota",3),
	("Ohio",18),
	("Oklahoma",7),
	("Oregon",7),
	("Pennsylvania",20),
	("Rhode Island",4),
	("South Carolina",9),
	("South Dakota",3),
	("Tennessee",11),
	("Texas",38),
	("Utah",6),
	("Vermont",3),
	("Virginia",13),
	("Washington",12),
	("West Virginia",5),
	("Wisconsin",10),
	("Wyoming",3)
	]

powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = [x:ps | ps <- powerSet xs] ++ powerSet xs

pathTo :: [Elector] -> Int -> [[State]]
pathTo states votes = map (map fst) $ filter (\s -> (sum $ map snd s) >= votes) (powerSet states)

lookupJust :: Eq a => [(a,b)] -> a -> b
lookupJust xs k = fromJust (lookup k xs)

remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove k xs = filter (\(key,_) -> key /= k) xs

percent :: Int -> Int -> Int
percent n d = (n*100) `div` d

randomFrom :: [a] -> Int -> a
randomFrom xs n = xs!!(n `mod` (length xs))

main = do
	let drumpf = [
		"Alabama",
		"Arkansas",
		"Idaho",
		"Kentucky",
		"Louisiana",
		"Mississippi",
		"NebraskaDrumpf",
		"North Dakota",
		"Oklahoma",
		"South Dakota",
		"Tennessee",
		"West Virginia",
		"Wyoming",

		"Indiana",
		"Kansas",
		"Missouri",
		"Montana",
		"South Carolina",
		"Utah",

		"Florida",
		"Iowa",
		"Ohio",
		"Texas",

		"MaineDrumpf"
		]

	let booboo = [
		"California",
		"Connecticut",
		"Delaware",
		"D.C.",
		"Hawaii",
		"Illinois",
		"Maryland",
		"Massachusetts",
		"NebraskaBooboo",
		"New Jersey",
		"New Mexico",
		"New York",
		"Oregon",
		"Rhode Island",
		"Vermont",
		"Washington",

		"Colorado",
		"Minnesota",
		"New Hampshire",
		"Virginia",
		"Wisconsin",

		"MaineBooboo"
		]

	let competitiveStates = foldr remove theStates (drumpf ++ booboo)
	let competitiveStateNames = map fst competitiveStates

	let drumpfScore = sum $ map (lookupJust theStates) drumpf
	let boobooScore = sum $ map (lookupJust theStates) booboo
	
	let drumpfPaths = pathTo competitiveStates (270-drumpfScore)
	let boobooPaths = pathTo competitiveStates (270-boobooScore)

	let drumpfFreq = foldl (\accmap state -> insertWith (+) state 1 accmap) Map.empty (concat drumpfPaths)
	let boobooFreq = foldl (\accmap state -> insertWith (+) state 1 accmap) Map.empty (concat boobooPaths)

	let stateStrings = [(printf (if length x >= 8 then "%s\t%d%%\t%d%%" else "%s\t\t%d%%\t%d%%") x (percent (findWithDefault 0 x drumpfFreq) (length drumpfPaths)) (percent (findWithDefault 0 x boobooFreq) (length boobooPaths)))::String | x <- competitiveStateNames]

	putStrLn "State\t\tDrumpf\tBooboo"
	sequence_ [putStrLn x | x <- stateStrings]

	putStrLn $ printf "Drumpf has %d electoral votes." drumpfScore
	putStrLn $ printf "Booboo has %d electoral votes." boobooScore

	putStrLn $ printf "Drumpf paths: %d" (length drumpfPaths)
	putStrLn $ printf "Booboo paths: %d" (length boobooPaths)

	g <- newStdGen
	putStrLn "Example paths:"
	putStrLn $ printf "Drumpf: %s" (show $ randomFrom drumpfPaths (head $ randoms g :: Int))
	putStrLn $ printf "Booboo: %s" (show $ randomFrom boobooPaths (head $ randoms g :: Int))

	putStrLn "Shortest paths:"
	putStrLn $ printf "Drumpf: %s" (show $ minimumBy (comparing length) drumpfPaths)
	putStrLn $ printf "Booboo: %s" (show $ minimumBy (comparing length) boobooPaths)
	--putStrLn $ show $ randomFrom drumpfPaths (head $ randoms g :: Int)