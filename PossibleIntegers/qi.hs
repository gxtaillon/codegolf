import Control.Monad
import Data.Function (on)
import Data.List (nub,sort,sortBy)
r(l:h:_)f=filter(>=0)$sort$nub$map(floor.f n)$replicateM(k-1)[(+),(-),(*),(/),(**)]
 where n=fromIntegral l;k=fromIntegral h
main=interact(\s->show$concat[r(map read$words s)$foldl(\m op->m`op`n),r(map read$words s)$foldr(\m op->m`op`n)])


