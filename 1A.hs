main = runSimpleApp $ do
  input <- readFileUtf8 "1.txt"
  let Right depths0@(_:depths1) = parseOnly (many (skipSpace *> decimal) <* skipSpace <* endOfInput) input
      changes = zipWith (-) depths1 depths0
      increases = length $ filter (>0) changes 
  logInfo $ "The number of depth increases is " <> display increases
