module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

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

parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do try (string "#d")
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do try (string "#x")
              x <- many1 hexDigit
              (return . Number . fst . head . readHex) x

parseOct :: Parser LispVal
parseOct = do try (string "#o")
              x <- many1 octDigit
              (return . Number . fst . head . readOct) x

binStr2Number :: Integer -> String -> Integer
binStr2Number accum "" = accum
binStr2Number accum (x:xs) = binStr2Number (accum * 2 + (if x == '0' then 0 else 1)) xs

parseBin :: Parser LispVal
parseBin = do try (string "#b")
              x <- many1 (oneOf "10")
              (return . Number . binStr2Number) x

parseNumber :: Parser LispVal
parseNumber = do num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
                 return num

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