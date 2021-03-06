import Data.Bits
import Data.List
import Data.Numbers.Primes
import qualified Data.Text as T
a=nub$concat$map(primeFactors)[0..127]
d(a:r)c=let s=shift c 5in if s<=0x10ffffthen d r(s+a)else c:d r a
d[]c=[c]
f(a:r)=let o={-# SCC "f-and" #-}a.&.0x1fin{-# SCC "f-appendend" #-}(if o/=a then {-# SCC "f-shift" #-} f((shiftR a 5):r)else {-# SCC "f-f-next" #-} f r)++[o]
f[]=[]
g=T.pack.map(toEnum).(\(a:r)->d r a).concatMap j.map(map(\x->head$elemIndices x a)).map(primeFactors.fromEnum).T.unpack
h=T.pack.map(toEnum.product.map((!!)a)).i.f.reverse.map(fromEnum).T.unpack
i(a:r)=let z=a`clearBit`4;x=if a`testBit`4then(take z$repeat$head r,tail r)else splitAt z r in[fst x]++i(snd x)
i[]=[]
j x@(a:_)=let l=length x in if(take l(repeat a))==x then[l`setBit`4,a]else[l]++x
j[]=[0]

edTest f = do
    t <- readFile f
    let txt = T.pack t
        enc = g txt
        dec = h enc
        tst = txt == dec
    putStrLn $ (show $ T.length txt) ++ "," ++ (show $ T.length enc) ++ "," ++ (show $ T.length dec)++","++(show tst)
    putStrLn $ if not tst then T.unpack txt ++ "\n---NEXT---\n" ++ T.unpack dec else ""

main = getLine >>= edTest
