-- Rules for 1,!,(c,C),(b,B) and accents are not going to happen.
-- Your example contains an unspecified rule: ( -> c,C
-- Your grammar is wrong. '8' cannot translate into (b,B), -ate- and '8'. Same goes for '!'
import System.Environment
import Text.Parsec
import Text.Parsec.String
s=string
r=return
t=try
-- n=noneOf
o=oneOf
(>|)=(<|>)
a p l u=b[p]l u
b (p:q) l u=e(foldl(>|)(s p)$map(s)q)l u
c p l u=e(o p)l u
d p q=t$s p>>r q
e p l u=t$do{p;r l}>|do{s". ";p;r$". "++u} -- >|do{q<-n" ";p;r$[q]++l}
f p q=t$do{between(t$s" ")(t$s" ")(o p);r q}
pp::Parser String
pp=do{s<-many$ -- (37 items + (21 * 5))*10/lengths
  c"$5""s""S"
  >|c"@4^""a""A"
  >|c"3""e""E"
  >|c"7+""t""T"
  >|c"#""h""H"
  >|d"teh""the"
  >|d"'d""ed"
  >|d"pwnd""pwned"
  >|d"pwnt""pwned"
  >|c"kK""ok""OK"
  >|d"kk""OK"
  >|d"n00b""newbie"
  >|f"yY""why"
  >|d"4""for"
  >|d"txt""text"
  >|d"dafuq""what the f**k"
  >|b["\\/\\/","vv","VV"]"w""W"
  >|a"/\\""a""A"
  >|d"d00d""dude"
  >|c"0""o""O"
  >|a"\\/""v""V"
  >|c"8""b""B"
  >|a"|_|""u""U"
  >|a"|-|""h""H"
  >|c"Я""r""R"
  >|b["j00","joo"]"you""you"
  >|d"tomoz""tomorrow"
  >|a"|<""k""K"
  >|b["[)","|)"]"d""D"
  >|d"<3""love"
  >|a"><""x""X"
  >|c"1!""i""I"
  >|d"10100111001""leet"
  >|c"2""too""to"
  >|d"ur""your"
  >|d"UR""you're"
  >|f"uU""you"
  >|c"xX""ks""cks"
  >|d"&""and"
  >|do{c<-anyChar;return [c]};
 return$concat s}
main=getArgs>>=putStrLn.show.(parse pp"").concat
