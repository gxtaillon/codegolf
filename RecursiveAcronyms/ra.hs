import Data.Char
import Data.List

f :: String -> [String] -> [(Bool, String)]
f "" w = map ((,) False) w
f _ [] = []
f (a:as) ((c:cs):w) | toLower a == toLower c = (True, c:cs) : f as w
                    | otherwise = (False, c:cs) : f (a:as) w
                   
g :: String -> Maybe [String]
g s = if (length $ filter (fst) d) == (length v)
          then Just $ map (snd) $ snd $ partition (fst) d 
          else Nothing
  where w = words s
        v = head w
        d = f v w
