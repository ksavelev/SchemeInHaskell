module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad()
import Numeric

-- data
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
escapedChars = do _ <- char '\\'
                  x <- oneOf "\\\"\n\r\t"
                  return $ case x of
                           '\\' -> x
                           '"'  -> x
                           'n'  -> '\n'
                           'r'  -> '\r'
                           't'  -> '\t'
                           _    -> x

parseString :: Parser LispVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\\\"" <|> escapedChars)
                 _ <- char '"'
                 return $ String x

parseBool :: Parser LispVal
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          _ -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first: rest
               return $ Atom atom

parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit
                   (return . Number . read) x

parseDigital2 :: Parser LispVal
parseDigital2 = do _ <- try (string "#d")
                   x <- many1 digit
                   (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do _ <- try (string "#x")
              x <- many1 hexDigit
              (return . Number . fst . head . readHex) x

parseOct :: Parser LispVal
parseOct = do _ <- try (string "#o")
              x <- many1 octDigit
              (return . Number . fst . head . readOct) x

bin2Num :: String -> Integer
bin2Num = binStr2Number 0

binStr2Number :: Integer -> String -> Integer
binStr2Number accum "" = accum
binStr2Number accum (x:xs) = binStr2Number (accum * 2 + (if x == '0' then 0 else 1)) xs

parseBin :: Parser LispVal
parseBin = do _ <- try (string "#b")
              x <- many1 (oneOf "10")
              (return . Number . bin2Num) x

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _ -> "Found value"

main :: IO ()
main = do
    args <- getArgs
    putStrLn ((readExpr . head) args)