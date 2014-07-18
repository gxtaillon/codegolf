import Text.Parsec
import Text.Parsec.String
n='N'
s='S'
e='E'
w='W'
d(x,y,c)'M'|c==n=(x,y+1,c)|c==s=(x,y-1,c)|c==e=(x+1,y,c)|c==w=(x-1,y,c)
d(x,y,c)e=(x,y,i c e)
i 'N''R'=e
i 'N''L'=w
i 'S''R'=w
i 'S''L'=e
i 'E''R'=s
i 'E''L'=n
i 'W''R'=n
i 'W''L'=s
f=many digit
g=char ','
o=oneOf
main=interact(\s->show$parse(do x<-f;g;y<-f;g;c<-o"NSEW";newline;b<-many$o"MRL";return$foldl(\x c->d x c)(read x,read y,c)b)""s)
