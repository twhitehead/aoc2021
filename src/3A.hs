import AOC
import qualified Data.Attoparsec.Text as P

main = runSimpleApp $ do
  input <- readFileUtf8 "3.txt"
  let Right digits = let binary = P.skipSpace *> some (-1 <$ P.char '0' <|> 1 <$ P.char '1')
                     in P.parseOnly ( many binary <* P.skipSpace <* P.endOfInput ) input
      sums = foldl' (\sum next -> zipWith (+) sum next) (let zs = 0:zs in zs) digits
      reduce isOne = foldl' (\left next -> left*2 + if isOne next then 1 else 0) (0::Word) sums
      gamma = reduce (>0)
      epsilon = reduce (<0)
  logInfo $ "The result is " <> display gamma <> "*" <> display epsilon <> " = " <> display (gamma*epsilon)
