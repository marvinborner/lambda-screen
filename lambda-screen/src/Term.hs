module Term
  ( parse
  , render
  , Term(..)
  ) where

import           Screen

data Term = Abs Term | App Term Term | Idx Int
  deriving Show
data HTerm = HAbs (HTerm -> HTerm) | HApp HTerm HTerm | HVar Int
data NTerm = NAbs Int NTerm | NApp NTerm NTerm | NVar Int
  deriving Show

(!?) :: [a] -> Int -> Maybe a
(!?) []       _ = Nothing
(!?) (x : _ ) 0 = Just x
(!?) (_ : xs) i = xs !? (i - 1)

app :: HTerm -> HTerm -> HTerm
app (HAbs f) = f
app f        = HApp f

higher :: Term -> HTerm
higher = go []
 where
  go env (Idx x) = case env !? x of
    Just v -> v
    _      -> HVar x
  go env (Abs t  ) = HAbs $ \x -> go (x : env) t
  go env (App a b) = app (go env a) (go env b)

lower :: HTerm -> NTerm
lower = go 0
 where
  go _ (HVar v  ) = NVar v
  go d (HAbs t  ) = NAbs d $ go (d + 1) (t (HVar d))
  go d (HApp a b) = NApp (go d a) (go d b)

render :: Term -> Image
render = go . lower . higher
 where
  go (NAbs x (NApp (NApp (NApp (NApp (NVar y) tl) tr) bl) br)) | x == y =
    Screen (go tl) (go tr) (go bl) (go br)
  go (NAbs w (NAbs b (NVar x))) | x == b = Pixel Black
  go (NAbs w (NAbs b (NVar x))) | x == w = Pixel White
  go t = error $ "not reducing to screen/pixel " <> show t

fromBinary :: String -> Term
fromBinary = fst . go
 where
  go inp = case inp of
    '0' : '0' : rst -> let (e, es) = go rst in (Abs e, es)
    '0' : '1' : rst ->
      let (a, rst1) = go rst
          (b, rst2) = go rst1
      in  (App a b, rst2)
    '1' : _ : rst -> binaryBruijn rst
    _             -> error $ "invalid " ++ inp
   where
    binaryBruijn rst =
      let idx = length (takeWhile (== '1') inp) - 1
      in  case rst of
            "" -> (Idx idx, "")
            _  -> (Idx idx, drop idx rst)

parse :: String -> Term
parse = fromBinary
