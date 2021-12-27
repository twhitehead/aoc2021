import AOC
import qualified Data.Attoparsec.Text as P

main = runSimpleApp $ do
  input <- readFileUtf8 "1.txt"
  let Right depths0@(_:depths1@(_:depths2)) = let depth = P.skipSpace *> P.decimal
                                              in P.parseOnly (some depth <* P.skipSpace <* P.endOfInput) input
      smoothed0@(_:smoothed1) = zipWith (+) (zipWith (+) depths0 depths1) depths2
      changes = zipWith (-) smoothed1 smoothed0
      increases = length $ filter (>0) changes 
  logInfo $ "The number of depth increases is " <> display increases
