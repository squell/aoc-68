import Control.Monad.State -- only used for parsing
import Data.List

parse :: String -> Snail
parse = evalState snail
  where 
  snail = do pair <- sub
             if pair then
               (:~:) <$> snail <* comma <*> snail <* bus
             else
               Value <$> digit
  sub   = state$ \(c  :ss) -> (c=='[', if c=='[' then ss else c:ss)
  comma = state$ \(',':ss) -> ((),ss)
  bus   = state$ \(']':ss) -> ((),ss)
  digit = state$ \(c:ss)   -> (read [c], ss)

data Snail = Value Int | Snail :~: Snail
  deriving Eq

instance Show Snail where
  show (Value n) = show n
  show (x :~: y) = concat ["[", show x, ",", show y, "]"]

main :: IO ()
main = do
  homework <- map parse . filter (not . null) . lines <$> getContents
  putStr "total magnitude: "
  print $ magnitude (foldl1' add homework)
  putStr "max magnitude: "
  print $ maximum [ magnitude $ add x y | x <- homework, y <- homework ]

add :: Snail -> Snail -> Snail
add x y = repeat (repeat explode . split) (x :~: y)
  where
  repeat :: (Eq a) => (a->a) -> (a->a)
  repeat f x = let y' = f x in if y' == x then x else repeat f y'

magnitude :: Snail -> Int
magnitude (Value x) = x
magnitude (x :~: y) = 3*magnitude x + 2*magnitude y

split :: Snail -> Snail
split = snd . go
  where
  go (Value x) | x >= 10 = (True, let half = x `div` 2 in Value half :~: Value(x-half))
  go (x :~: y)           = case (go x, go y) of
                             ((True, x'), _     ) -> (True, x' :~: y)
                             ((_   , x'), (b,y')) -> (b,    x' :~: y')
  go x                   = (False, x)

data Status = Seeking | Boom (Int,Int) | Idle

explode :: Snail -> Snail
explode snail = fst $ go (Just 0) snail (0,0)
  where
  go :: Maybe Int -> Snail -> (Int,Int) -> (Snail, Status)

  go n (Value x :~: Value y) _ | n >= Just 4  -- kapoof!
    = (Value 0, Boom (x,y))

  go n (Value x) (l,r)
    = (Value (l+x+r), if n /= Nothing then Seeking else Idle)

  go n (left :~: right) (a,z)
    = (left' :~: right', case (s1,s2) of (Boom(val,_), _           ) -> Boom(val,0)
                                         (_          , Boom (_,val)) -> Boom(0,val)
                                         _ -> s2)
    where (left' , s1) = go  (fmap(+1) n) left  (a, case s2 of Boom(val,_) -> val; _ -> 0)
          (right', s2) = go' (fmap(+1) n) right (case s1 of Boom(_,val) -> val; _ -> 0, z)

          go' = case s1 of Idle   -> \_ n _ -> (n, Idle)
                           Boom _ -> \_ -> go Nothing
                           _      -> go

