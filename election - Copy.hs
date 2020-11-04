import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
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
	("Maine",4),
	("Maryland",10),
	("Massachusetts",11),
	("Michigan",16),
	("Minnesota",10),
	("Mississippi",6),
	("Missouri",10),
	("Montana",3),
	("Nebraska",5),
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

main = do
	let drumpf = [
		"Alabama",
		"Arkansas",
		"Idaho",
		"Kentucky",
		"Louisiana",
		"Mississippi",
		"Nebraska",
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

		"Ohio"
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
		"New Jersey",
		"New Mexico",
		"New York",
		"Oregon",
		"Rhode Island",
		"Vermont",
		"Washington",

		"Colorado",
		"New Hampshire",
		"Virginia"
		]

	let competitiveStates = foldr remove theStates (drumpf ++ booboo)
	let competitiveStateNames = map fst competitiveStates

	let drumpfScore = sum $ map (lookupJust theStates) drumpf
	let boobooScore = sum $ map (lookupJust theStates) booboo
	
	let drumpfPaths = pathTo competitiveStates (270-drumpfScore)
	let boobooPaths = pathTo competitiveStates (270-boobooScore)

	putStrLn $ printf "Drumpf paths: %d" (length drumpfPaths)
	putStrLn $ printf "Booboo paths: %d" (length boobooPaths)