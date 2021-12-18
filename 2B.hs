data Command = CmdFwd Integer | CmdDwn Integer | CmdUp Integer deriving Show

main = runSimpleApp $ do
  input <- readFileUtf8 "2.txt"
  let Right commands = let command = CmdFwd <$ string "forward" <|> CmdDwn <$ string "down" <|> CmdUp <$ string "up"
                       in parseOnly ( ( many $ skipSpace *> command <* skipSpace <*> decimal ) <* skipSpace <* endOfInput ) input
      evaluate (x,y,aim) (CmdFwd amount) = (x+amount,y+aim*amount,aim       )
      evaluate (x,y,aim) (CmdUp  amount) = (x       ,y           ,aim-amount)
      evaluate (x,y,aim) (CmdDwn amount) = (x       ,y           ,aim+amount)
      (x,y,_) = foldl' evaluate (0,0,0) commands
  logInfo $ "The result is " <> display x <> "*" <> display y <> " = " <> display (x*y)
