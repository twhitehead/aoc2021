import AOC
import qualified RIO.Map as M
import qualified Data.Attoparsec.Text as P

data Draw a = Draw a | Never deriving (Eq, Ord)

main = runSimpleApp $ do
  input <- readFileUtf8 "4.txt"
  let
    draws :: [Integer]       -- List of draws in order
    boards :: [[[Integer]]]  -- List of boards, which are a list of rows, which are a list of values
    Right (draws,boards) = let order = (:) <$ P.skipSpace <*> P.decimal <*> many (P.skipSpace *> P.char ',' *> P.skipSpace *> P.decimal)
                               line = some (P.skipWhile isHorizontalSpace *> P.decimal)
                               board = (:) <$ P.skipSpace <*> line <*> many (P.skipWhile isHorizontalSpace *> P.endOfLine *> line)
                           in P.parseOnly ( (,) <$> order <*> some board <* P.skipSpace <* P.endOfInput ) input

    valueToDraw :: Integer -> Draw Integer  -- Reverse map from a value to its draw index
    valueToDraw = let map = M.fromList $ zip draws [0..] in maybe Never Draw . (map M.!?)

    done :: Integer          -- The index of the first draw producing a winner
    value :: Integer         -- The value of the first draw producing a winner
    score :: Integer         -- The sum of entries on the winning board not drawn by winning time
    Just (Max (Arg (Draw done) (value,score))) = boards
      & foldMap (\(board :: [[Integer]]) -> do
          let
            annotated :: [[Arg (Draw Integer) Integer]]             -- Boards annotated with draw indices
            annotated = board
              & (fmap . fmap) (\value -> Arg (valueToDraw value) value)
          
          Min (Arg done value) <- annotated <> transpose annotated  -- All possible ways to complete (row + cols)
            & foldMap ( fmap (Min . getMax) . foldMap (Just . Max) )

          Sum score <- annotated                                    -- Score is sum of entries not drawn
            & (foldMap . foldMap) (\(Arg draw value) -> if draw > done then Just . Sum $ value else Nothing )
                                
          Just . Max $ Arg done (value, score)
        )
  logInfo $ "The result is " <> display score <> "*" <> display value <> " = " <> display (score*value) <> " (draw " <> display done <> ")"
