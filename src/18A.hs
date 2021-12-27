import AOC
import RIO.Seq as S (Seq(..))
import qualified RIO.Seq as S
import qualified Data.Attoparsec.Text as P


mapCons :: (a -> a) -> Seq a -> Seq a
mapCons f (x :<| xs) = f x :<| xs
mapCons _ Empty      = Empty

mapSnoc :: (a -> a) -> Seq a -> Seq a
mapSnoc f (xs :|> x) = xs :|> f x
mapSnoc _ Empty      = Empty


main :: IO ()
main = runSimpleApp $ do
  input <- readFileUtf8 "18.txt"
  let Right (number:numbers) = parse input
      result = magnitude $ foldl' (((.).(.)) simplify add) number numbers
  logInfo $ "The magnitude of the sum is " <> display result


-- | Parse a sequence of snailfish numbers
--
-- [[[[4,3],4],4],[7,[[8,4],9]]] -> [(4,4),(-1,3),(-1,4),(-1,4),(1,7),(2,8),(-1,4),(-3,9)]
--
-- >>> parse "[[[[4,3],4],4],[7,[[8,4],9]]]\n[1,1]"
-- Right [fromList [(4,4),(-1,3),(-1,4),(-1,4),(1,7),(2,8),(-1,4),(-3,9)],fromList [(1,1),(-1,1)]]
parse :: Text -> Either String [Seq (Int,Integer)]
parse input =
  let lBraces =          length <$> some (P.skipSpace *> P.char '[')
      rBraces = negate . length <$> some (P.skipSpace *> P.char ']')
      element =          (,) <$> lBraces <*  P.skipSpace <*> P.decimal
                <|> flip (,) <$  P.skipSpace <*> P.decimal <*> rBraces
      number = S.fromList <$> ( (:) <$> element <*> many (P.skipSpace *> P.char ',' *> element) )
  in P.parseOnly ( some number <* P.skipSpace <* P.endOfInput ) input


-- | Explode a snailfish number (single step)
--
-- [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] -> [[[[0,7],4],[7,[[8,4],9]]],[1,1]]
--                                       -> [[[[0,7],4],[15,[0,13]]],[1,1]]
-- [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]] -> [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
--
-- >>> explode [(5,4),(-1,3),(-1,4),(-1,4),(1,7),(2,8),(-1,4),(-3,9),(1,1),(-2,1)]
-- Just (fromList [(4,0),(-1,7),(-1,4),(1,7),(2,8),(-1,4),(-3,9),(1,1),(-2,1)])
-- >>> explode [(4,0),(-1,7),(-1,4),(1,7),(2,8),(-1,4),(-3,9),(1,1),(-2,1)]
-- Just (fromList [(4,0),(-1,7),(-1,4),(1,15),(1,0),(-3,13),(1,1),(-2,1)])
-- >>> explode [(4,0),(-1,7),(-1,4),(1,15),(1,0),(-3,13),(1,1),(-2,1)]
-- Nothing
--
-- >>> explode [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,0),(1,6),(-4,7),(1,1),(-2,1)]
-- Just (fromList [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,6),(-3,0),(1,8),(-2,1)])
-- >>> explode [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,6),(-3,0),(1,8),(-2,1)]
-- Nothing
explode :: Ord a => Num a => Num b => Seq (a,b) -> Maybe (Seq (a,b))
explode s =
  let walk :: Ord a => Num a => Num b => Seq (a,b) -> Seq (a,b) -> a -> Maybe (Seq (a,b))
      walk  (ls :|> (ln0,lx0)) ((rn0,rx0) :<| rs) mn | mn > 4 && ln0 > 0 && rn0 < 0  -- Explode (return changes)
        = Just $ ((mapSnoc . fmap) (+lx0) ls) <> ((ln0+rn0,0) :<| (mapCons . fmap) (rx0+) rs)
      walk ls (r0@(rn0,rx0) :<| rs) mn = walk (ls :|> r0) rs (mn+rn0)                -- Forward
      walk ls Empty                 0 = Nothing                                      -- Done (return no changes)
  in walk Empty s 0


-- | Split a snailfish number (single step)
--
-- [[[[0,7],4],[15,[0,13]]],[1,1]] -> [[[[0,7],4],[[7,8],[0,13]]],[1,1]]
--                                 -> [[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]
--
-- >>> split [(4,0),(-1,7),(-1,4),(1,15),(1,0),(-3,13),(1,1),(-2,1)]
-- Just (fromList [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,0),(-3,13),(1,1),(-2,1)])
-- >>> split [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,0),(-3,13),(1,1),(-2,1)]
-- Just (fromList [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,0),(1,6),(-4,7),(1,1),(-2,1)])
-- >>> split [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,0),(1,6),(-4,7),(1,1),(-2,1)]
-- Nothing
split :: Ord a => Num a => Integral b => Seq (a,b) -> Maybe (Seq (a,b))
split s =
  let walk :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> Maybe (Seq (a,b))
      walk ls ((rn0,rx0) :<| rs) | rx0 >= 10
        = let (q,r) = quotRem rx0 2                                  -- Split (return changes)
          in Just $ (ls :|> (max rn0 0 + 1,q)) <> ((min rn0 0 - 1,q+r) :<| rs)
      walk ls (r0        :<| rs) = walk (ls :|> r0) rs               -- Forward
      walk ls Empty              = Nothing                           -- Done (return no changes)
  in walk Empty s


-- | Simplifying a snailfish number
--
-- [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] -> [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
--
-- >>> simplify [(5,4),(-1,3),(-1,4),(-1,4),(1,7),(2,8),(-1,4),(-3,9),(1,1),(-2,1)]
-- fromList [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,6),(-3,0),(1,8),(-2,1)]
simplify :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b)
simplify s = maybe (maybe s simplify $ split s) simplify $ explode s


-- | Add two snailfish numbers
--
-- [[[[4,3],4],4],[7,[[8,4],9]]] + [1,1] -> [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]
--                                       -> [[[[0,7],4],[[7,8],[6,0]]],[8,1]]
--
-- >>> add [(4,4),(-1,3),(-1,4),(-1,4),(1,7),(2,8),(-1,4),(-3,9)] [(1,1),(-1,1)]
-- fromList [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,6),(-3,0),(1,8),(-2,1)]
add :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> Seq (a,b)
add ((ln0,lx0) :<| ls) (rs :|> (rn0,rx0)) = simplify $ ((ln0+1,lx0) :<| ls) <> (rs :|> (rn0-1,rx0))


-- | Magnitude
--
-- [[[[0,7],4],[[7,8],[6,0]]],[8,1]] -> 1384
--
-- >>> magnitude [(4,0),(-1,7),(-1,4),(2,7),(-1,8),(1,6),(-3,0),(1,8),(-2,1)]
-- 1384
magnitude :: Ord a => Num a => Integral b => Seq (a,b) -> b
magnitude s =
  let walk  :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> b
      walk (ls    :|> (nl,xl)) ((nr,xr) :<| rs) | nl > 0 && nr < 0 = walk ls ((nl+nr,3*xl+2*xr) :<| rs)
      walk  ls                 (r0      :<| rs)                    = walk (ls :|> r0) rs
      walk (Empty :|> (0 ,x))  Empty                               = x
  in walk Empty s
