module Main where

main :: IO ()
main = do
    print $ head $ solutions [1, 3, 7, 10, 25, 50] 765
    print $ subs [1,2]
    print $ interleave 1 [2,3]
    print $ perms [1,2,3]
    print $ choices [1,2,3]
    print $ split [1,2,3]

data Op = Add | Sub | Mul | Div deriving Show

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


valid :: Op -> Int -> Int -> Bool
valid Add x y = x >= y
valid Sub x y = x > y
valid Mul x y = x >= y
valid Div x y = x `mod` y == 0


data Expr = Val Int | App Op Expr Expr 
    deriving Show

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                 where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls,rs) <- split xs]

exprs                         :: [Int] -> [Expr]
exprs []                      =  []
exprs [n]                     =  [Val n]
exprs ns                      =  [e | (ls,rs) <- split ns
                                    , l       <- exprs ls
                                    , r       <- exprs rs
                                    , e       <- combine l r]

combine                       :: Expr -> Expr -> [Expr]
combine l r                   =  [App o l r | o <- ops]


ops                           :: [Op]
ops                           =  [Add,Sub,Mul,Div]

solutions                     :: [Int] -> Int -> [Expr]
solutions ns n                =  [e | ns' <- choices ns
                                    , e   <- exprs ns'
                                    , eval e == [n]]
