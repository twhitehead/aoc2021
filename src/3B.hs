import AOC
import qualified Data.Attoparsec.Text as P

import RIO.List.Partial ((!!))

main = runSimpleApp $ do
  input <- readFileUtf8 "3.txt"
  let Right digits = let binary = P.skipSpace *> some (False <$ P.char '0' <|> True <$ P.char '1')
                     in P.parseOnly ( many binary <* P.skipSpace <* P.endOfInput ) input
      reduce _      [result] _     = result
      reduce choice options  depth =
        let options1 = filter (!! depth) options
            options2 = filter (not . (!! depth)) options
        in reduce choice (if length options1 `choice` length options2 then options1 else options2) (depth+1)
      decimal = foldl' (\left next -> left*2 + if next then 1 else 0) (0::Integer)
      gamma = decimal $ reduce (>=) digits 0
      epsilon = decimal $ reduce (<) digits 0
  logInfo $ "The result is " <> display gamma <> "*" <> display epsilon <> " = " <> display (gamma*epsilon)
