import RIO.Seq as S (Seq(..))
import qualified RIO.Seq as S
import qualified Data.Attoparsec.Text as P

mapCons f (x :<| xs) = f x :<| xs
mapCons _ Empty      = Empty

mapSnoc f (xs :|> x) = xs :|> f x
mapSnoc _ Empty      = Empty

main :: IO ()
main = runSimpleApp $ do
  input <- readFileUtf8 "18.txt"
  let numbers :: [Seq (Int,Integer)]
      Right numbers = let lBraces =          length <$> some (P.skipSpace *> P.char '[')
                          rBraces = negate . length <$> some (P.skipSpace *> P.char ']')
                          element =          (,) <$> lBraces <*  P.skipSpace <*> P.decimal
                                    <|> flip (,) <$  P.skipSpace <*> P.decimal <*> rBraces
                          number = S.fromList <$> ( (:) <$> element <*> many (P.skipSpace *> P.char ',' *> element) )
                      in P.parseOnly ( some number <* P.skipSpace <* P.endOfInput ) input

      add :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> Seq (a,b)
      add ((ln0,lx0) :<| ls) (rs :|> (rn0,rx0)) = simplify $ ((ln0+1,lx0) :<| ls) <> (rs :|> (rn0-1,rx0))

      simplify :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b)
      simplify s = maybe (maybe s simplify $ split s) simplify $ explode s

      explode :: Ord a => Num a => Num b => Seq (a,b) -> Maybe (Seq (a,b))
      explode s =
        let walk :: Ord a => Num a => Num b => Seq (a,b) -> Seq (a,b) -> a -> Maybe (Seq (a,b))
            walk  (ls :|> (ln0,lx0)) ((rn0,rx0) :<| rs) mn | mn > 4 && ln0 > 0 && rn0 < 0  -- Explode (return changes)
              = Just $ ((mapSnoc . fmap) (+lx0) ls) <> ((ln0+rn0,0) :<| (mapCons . fmap) (rx0+) rs)
            walk ls (r0@(rn0,rx0) :<| rs) mn = walk (ls :|> r0) rs (mn+rn0)                -- Forward
            walk ls Empty                 0 = Nothing                                      -- Done (return no changes)
        in walk Empty s 0

      split :: Ord a => Num a => Integral b => Seq (a,b) -> Maybe (Seq (a,b))
      split s =
        let walk :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> Maybe (Seq (a,b))
            walk ls ((rn0,rx0) :<| rs) | rx0 >= 10
              = let (q,r) = quotRem rx0 2                                  -- Split (return changes)
                in Just $ (ls :|> (max rn0 0 + 1,q)) <> ((min rn0 0 - 1,q+r) :<| rs)
            walk ls (r0        :<| rs) = walk (ls :|> r0) rs               -- Forward
            walk ls Empty              = Nothing                           -- Done (return no changes)
        in walk Empty s

      magnitude :: Ord a => Num a => Integral b => Seq (a,b) -> b
      magnitude s =
        let walk  :: Ord a => Num a => Integral b => Seq (a,b) -> Seq (a,b) -> b
            walk (ls    :|> (nl,xl)) ((nr,xr) :<| rs) | nl > 0 && nr < 0 = walk ls ((nl+nr,3*xl+2*xr) :<| rs)
            walk  ls                 (r0      :<| rs)                    = walk (ls :|> r0) rs
            walk (Empty :|> (0 ,x))  Empty                               = x
        in walk Empty s

      Just (Max result) = foldMap (Just . Max) $ magnitude <$> ( add <$> numbers <*> numbers )
  logInfo $ "The maximum sum is " <> display result
