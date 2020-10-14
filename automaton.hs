import Data.Maybe

data Automaton a b = Automaton
    { alphabet        :: [a],
      states          :: [b],
      q0              ::  b,
      finalStates     :: [b],
      delta           :: b -> a -> b
    }

checkWord :: (Eq b) => Automaton a b -> [a] -> Bool
checkWord dfa x = foldl (delta dfa) (q0 dfa) x `elem` finalStates dfa

checkWords :: (Eq b) => Automaton a b -> [[a]] -> [([a],Bool)]
checkWords dfa words = foldr (\word results -> (word, checkWord dfa word):results) [] words

lookup3 :: (Eq b, Eq a) => [(b,a,b)] -> b -> a -> Maybe b
lookup3 [] _ _ = Nothing
lookup3 ((x,y,z):arcs) q l
    | x == q && y == l = Just z
    | otherwise        = lookup3 arcs q l

lookup3Just :: (Eq b, Eq a) => [(b,a,b)] -> b -> a -> b
lookup3Just arcs q l = fromJust $ lookup3 arcs q l


main = do
    let theDelta = [("q0",'a',"q0"),("q0",'b',"q1"),("q1",'a',"q0"),("q1",'b',"q1")]
    let dfa = Automaton {alphabet="ab", states=["q0","q1"], q0="q0", finalStates=["q1"], delta=(lookup3Just theDelta)}
    putStrLn $ show $ checkWords dfa ["a","b","ab","abbab","aaabbbbabababbaaa"]
