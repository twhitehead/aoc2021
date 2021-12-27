import AOC
import qualified Data.Attoparsec.Text as P

data Command = CmdFwd Integer | CmdDwn Integer | CmdUp Integer deriving Show

main = runSimpleApp $ do
  input <- readFileUtf8 "2.txt"
  let Right commands = let command = CmdFwd <$ P.string "forward" <|> CmdDwn <$ P.string "down" <|> CmdUp <$ P.string "up"
                       in P.parseOnly ( ( many $ P.skipSpace *> command <* P.skipSpace <*> P.decimal ) <* P.skipSpace <* P.endOfInput ) input
      evaluate (x,y,aim) (CmdFwd amount) = (x+amount,y+aim*amount,aim       )
      evaluate (x,y,aim) (CmdUp  amount) = (x       ,y           ,aim-amount)
      evaluate (x,y,aim) (CmdDwn amount) = (x       ,y           ,aim+amount)
      (x,y,_) = foldl' evaluate (0,0,0) commands
  logInfo $ "The result is " <> display x <> "*" <> display y <> " = " <> display (x*y)
