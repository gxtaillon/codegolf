import Data.Bits
import Data.List
import Data.Numbers.Primes
import qualified Data.Text as T

factorLists = map(primeFactors)[0..127]
factors = nub$concat$factorLists

encRatio t = fromIntegral (T.length $ encode t) / fromIntegral (T.length t)

-- Behavior is undefined for input characters greater than 0x7f
encode :: T.Text -> T.Text
encode t = T.pack $ map (toEnum) $ pck $ rdyFactors
    where rawFactors = map (primeFactors.fromEnum) $ T.unpack t
          idxFactors = map (map(\x->case find(==x)factors of Just i -> i;_->1)) rawFactors
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


{-
decode :: Text -> Text
decode t = pack $ fs $ sf
    where pckdFactors = map (fromEnum) $ unpack t
          rdyFactors = 
          idxFactors = map ((!!)factors) $ uck pckdFactors










-}
