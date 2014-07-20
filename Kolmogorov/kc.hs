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
          rdyFactors = concatMap (\x@(a:_)->do 
                            let l = length x;
                                w = take l $ repeat a
                            if w==x 
                                then [l`setBit`4,a] -- If the factors are all the same, store only one, set bit to tell how to decode.
                                else if init w == init x
                                    then [l`setBit`3,a,last x]
                                    else [l]++x) $ idxFactors 

pck :: [Int] -> [Int]
pck (a:r) = pcks r a
pck [] = []
pcks :: [Int] -> Int -> [Int]
pcks (a:r) c = if s <= 0x10ffff then pcks r (s+a) else c : pcks r a -- max value of unicode char
    where s = shift c 5 -- 128 chars with a total of 32 factors that can be stored on *5* bits.
pcks [] c = [c]


uck :: [Int] -> [Int]
uck = ucks.reverse
ucks (a:r) = if o /= a then ucks ((shift a (-5)):r) ++ [o] else ucks r ++ [o]
    where o = a .&. 0x1f
ucks [] = []



toIdxFactors :: [Int] -> [[Int]]
toIdxFactors (a:r) = [fst tup] ++ toIdxFactors (snd tup)
    where len = a`clearBit`4`clearBit`3
          tup = if a`testBit`4 
                    then (take len $ repeat $ head r, tail r)
                    else if a`testBit`3
                        then ((take (len-1) $ repeat $ head r) ++ [r!!1], drop 2 r)
                        else splitAt len r
toIdxFactors [] = []

decode :: T.Text -> T.Text
decode t = T.pack $ map (toEnum.product.map((!!)factors)) idxFactors
    where rdyFactors = uck $ map (fromEnum) $ T.unpack t
          idxFactors = toIdxFactors rdyFactors


decodeFile f = do 
    t <- readFile f
    let txt = T.pack t
        dec = decode txt
        inf = "Input lenght: "++(show $ T.length txt)++"\nDecoded length: "++(show $ T.length dec)
    writeFile (f++".decoded") $ T.unpack dec
    writeFile (f++".decoded.stats") inf
encodeFile f = do 
    t <- readFile f
    let txt = T.pack t
        enc = encode txt
        inf = "Input lenght: "++(show $ T.length txt)++"\nEncoded length: "++(show $ T.length enc)
    writeFile (f++".encoded") $ T.unpack enc
    writeFile (f++".encoded.stats") inf

cleanFile f = do
    t <- readFile f
    writeFile (f++".clean") $ filter ((>)0x7f.fromEnum) t
    

edTest f = do
    t <- readFile f
    let txt = T.pack t
        enc = encode txt
        dec = decode enc
        tst = txt == dec
    putStrLn $ (show $ T.length txt) ++ "," ++ (show $ T.length enc) ++ "," ++ (show $ T.length dec)++","++(show tst)
    putStrLn $ if not tst then T.unpack txt ++ "\n---NEXT---\n" ++ T.unpack dec else ""    




