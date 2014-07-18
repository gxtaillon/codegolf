
import Text.ParserCombinators.Parsec

--file = sepBy content comment
comment = (try commentLine <|> commentBlock <?> "comment") >> return "// Do the needful"
commentLine = string "//" >> manyTill anyChar eol
commentBlock = string "/*" >> manyTill anyChar (string "*/")
commentStart = string "//" <|> string "/*" <?> "comment start"
content = manyTill anyChar (try commentStart <|> eol)
line = comment <|> content
file = manyTill line eof

f = case anyChar of
        



eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"
{-
main = do
    c <- getContents
    case parse file "" c of
        Left e -> print e
        Right r -> map-}
