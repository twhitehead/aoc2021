import AOC
import qualified Data.Attoparsec.Text as P

data Command = CmdFwd Integer | CmdDwn Integer | CmdUp Integer deriving Show

main = runSimpleApp $ do
  input <- readFileUtf8 "2.txt"
  let Right commands = let command = CmdFwd <$ P.string "forward" <|> CmdDwn <$ P.string "down" <|> CmdUp <$ P.string "up"
                       in P.parseOnly ( ( many $ P.skipSpace *> command <* P.skipSpace <*> P.decimal ) <* P.skipSpace <* P.endOfInput ) input
      evaluate (x,y) (CmdFwd amount) = (x+amount,y)
      evaluate (x,y) (CmdUp  amount) = (x,y-amount)
      evaluate (x,y) (CmdDwn amount) = (x,y+amount)
      (x,y) = foldl' evaluate (0,0) commands
  logInfo $ "The result is " <> display x <> "*" <> display y <> " = " <> display (x*y)
