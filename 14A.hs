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

    transform :: ((Char,Char),Integer) -> [((Char,Char),Integer)]
    transform (pair,count) = rulesMap M.!? pair & maybe [(pair,count)] (map (\pair -> (pair,count)))

    initial :: Map (Char,Char) Integer
    initial = M.fromListWith (+) $ zipWith (,) (zipWith (,) (seed:seeds) seeds) (repeat 1)

    systemStep :: Map (Char,Char) Integer -> Map (Char,Char) Integer
    systemStep = M.fromListWith (+) . foldMap transform . M.toList

    final :: Map (Char,Char) Integer
    final = appEndo (mconcat $ replicate 10 (Endo systemStep)) initial

    counts :: Map Char Integer
    counts = M.toList final & map (\((_,right),count) -> (right,count))  -- Each letter occurs it two pairs
                            & ((seed,1):)                                -- Except the first and last one
                            & M.fromListWith (+)

    max :: Integer
    min :: Integer
    Just (Min min,Max max) = M.toList counts & foldMap ( \(_,count) -> Just (Min count, Max count) )
  logInfo $ "The result is " <> display max <> " - " <> display min <> " = " <> display (max - min)
