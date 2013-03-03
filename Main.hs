module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\"\n\r\t"
                  return $ case x of
                           '\\' -> x
                           '"'  -> x
                           'n'  -> '\n'
                           'r'  -> '\r'
                           't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\\\"" <|> escapedChars)
                 char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first: rest
               return $ Atom atom

parseNumber :: Parser LispVal
parseNumber = (many1 digit) >>= (return . read) >>= (return . Number)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (args !! 0))