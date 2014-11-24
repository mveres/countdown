module Main where

main :: IO ()
main = do
    putStrLn "Welcome to the Countdown problem!"


data Op = Add | Sub | Mul | Div

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y