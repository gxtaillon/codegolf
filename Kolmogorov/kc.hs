import Data.Bits
import Data.List
import Data.Numbers.Primes
import qualified Data.Text as T
import Debug.Trace

-- Because upgrading is a pain in the ***.
traceShowId :: (Show a) => a -> a
traceShowId a = trace (show a) a


factorLists = map(primeFactors)[0..127]
factors = nub$concat$factorLists

encRatio t = fromIntegral (T.length $ encode t) / fromIntegral (T.length t)

-- Behavior is undefined for input characters greater than 0x7f
encode :: T.Text -> T.Text
encode t = T.pack $ map (toEnum) $ pck rdyFactors
    where rawFactors = map (primeFactors.fromEnum) $ T.unpack t
          idxFactors = map (map(\x->case elemIndex x factors of Just i -> i;_->1)) rawFactors
          rdyFactors = concatMap (\x->[length x]++x) idxFactors
          
          
pck :: [Int] -> [Int]
pck (a:r) = pcks r a
pck [] = []
pcks :: [Int] -> Int -> [Int]
pcks (a:r) c = if s < 0x10ffff then pcks r (s+a) else c : pcks r a -- max value of unicode char
    where s = shift c 5 -- 128 chars with a total of 32 factors that can be stored on *5* bits.
pcks [] c = [c]


uck :: [Int] -> [Int]
uck = ucks.reverse
ucks (a:r) = if o /= a then ucks ((shift a (-5)):r) ++ [o] else ucks r ++ [o]
    where o = a .&. 0x1f
ucks [] = []


toIdxFactors :: [Int] -> [[Int]]
toIdxFactors (a:r) = [fst tup] ++ toIdxFactors (snd tup)
    where tup = splitAt a r
toIdxFactors [] = []

decode :: T.Text -> T.Text
decode t = T.pack $ map (toEnum.product.map((!!)factors)) idxFactors
    where rdyFactors = uck $ map (fromEnum) $ T.unpack t
          idxFactors = toIdxFactors rdyFactors










