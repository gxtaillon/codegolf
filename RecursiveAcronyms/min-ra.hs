import Data.Char
import Data.List
f""w=map((,)False)w
f _[]=[]
f(a:as)(cs@(c:_):w) 
 |toLower a==toLower c=(True,cs):f as w
 |True=(False,cs):f(a:as)w
g s=if(length$filter(fst)d)==length v
  then Just$map(snd)$snd$partition(fst)d 
  else Nothing
 where 
  w=words s
  v=head w
  d=f v w
