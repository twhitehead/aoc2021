import AOC
import qualified RIO.Map as M
import qualified Data.Attoparsec.Text as P

main = runSimpleApp $ do
  input <- readFileUtf8 "14.txt"
  let
    seed :: Char
    seeds :: [Char]
    rules :: [((Char,Char),Char)]
    Right (seed:seeds,rules) = let chain = P.skipSpace *> some P.letter
                                   rule = (,) <$ P.skipSpace <*> ((,) <$> P.letter <*> P.letter)
                                              <* P.skipSpace <* P.string "->" <* P.skipSpace <*> P.letter
                               in P.parseOnly ( (,) <$> chain <*> some rule <* P.skipSpace <* P.endOfInput ) input

    rulesMap :: Map (Char,Char) [(Char,Char)]
    rulesMap = M.fromList $ map (\((left,right),middle) -> ((left,right),[(left,middle),(middle,right)])) rules

    initial :: Map (Char,Char) Integer
    initial = M.fromListWith (+) $ zipWith (,) (zipWith (,) (seed:seeds) seeds) (repeat 1)

    pairStep :: ((Char,Char),Integer) -> [((Char,Char),Integer)]
    pairStep (pair,count) = rulesMap M.!? pair & maybe [(pair,count)] ( map (\pair -> (pair,count)) )

    systemStep :: Map (Char,Char) Integer -> Map (Char,Char) Integer
    systemStep = M.fromListWith (+) . foldMap pairStep . M.toList

    final :: Map (Char,Char) Integer
    final = appEndo (mconcat $ replicate 10 (Endo systemStep)) initial

    counts :: Map Char Integer
    counts = M.toList final & map (\((_,right),count) -> (right,count))  -- Each letter occurs on a right
                            & ((seed,1):)                                -- except the first left
                            & M.fromListWith (+)

    max :: Integer
    min :: Integer
    Just (Min min,Max max) = M.toList counts & foldMap ( \(_,count) -> Just (Min count, Max count) )
  logInfo $ "The result is " <> display max <> " - " <> display min <> " = " <> display (max - min)
