import Control.Monad
import Data.Function (on)
import Data.List (nub,sort,sortBy)
r(l:h:_)={-filter(>=0)$sort$nub$-}map(floor.foldl(\m(_,op)->m`op`n)n.sortBy(compare`on`fst))$replicateM(k-1)[(3,(+)),(3,(-)),(1,(*)),(1,(/)),(0,(**))]
 where n=fromIntegral l;k=fromIntegral h
main=interact(\s->show$r$map read$words s)

