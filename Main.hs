module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let n1 = read $ args !! 0
    let n2 = read $ args !! 1
    let sm = n1 + n2
    putStrLn ("Hello, " ++ (show sm))