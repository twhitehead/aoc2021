data Command = CmdFwd Integer | CmdDwn Integer | CmdUp Integer deriving Show

main = runSimpleApp $ do
  input <- readFileUtf8 "2.txt"
  let Right commands = let command = CmdFwd <$ string "forward" <|> CmdDwn <$ string "down" <|> CmdUp <$ string "up"
                       in parseOnly ( ( many $ skipSpace *> command <* skipSpace <*> decimal ) <* skipSpace <* endOfInput ) input
      evaluate (x,y) (CmdFwd amount) = (x+amount,y)
      evaluate (x,y) (CmdUp  amount) = (x,y-amount)
      evaluate (x,y) (CmdDwn amount) = (x,y+amount)
      (x,y) = foldl' evaluate (0,0) commands
  logInfo $ "The result is " <> display x <> "*" <> display y <> " = " <> display (x*y)
