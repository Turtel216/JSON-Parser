module JParser where

import Control.Applicative
import Data.Char

data JsonValue = JsonNull
                | JsonBool Bool 
                | JsonNumber Integer -- TODO Implement Floating point numbers
                | JsonString String
                | JsonArray [JsonValue]
                | JsonObject [(String, JsonValue)]
                deriving (Show, Eq)

-- TODO add proper error reporting
newtype Parser a = Parser 
  { runParser :: String -> Maybe (String, a) 
  }

instance Functor Parser where 
  fmap f (Parser p) = Parser $ \input -> do 
    (input', x) <- p input 
    Just (input', f x)

instance Applicative Parser where 
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do 
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where 
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue 
jsonNull = (\_ -> JsonNull) <$> stringP "null"


-- parses single character
charP :: Char -> Parser Char 
charP x = Parser f 
  where
    f (y:ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

-- parses list of characters
stringP :: String -> Parser String
stringP input = sequenceA $ map charP input

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true"  = JsonBool True
        f "false" = JsonBool False
        -- This should never happen
        f _       = undefined

-- A version of span from stdlib but for the parser type
spanP :: (Char -> Bool) -> Parser String
spanP f = 
  Parser $ \input -> 
    let (token, rest) = span f input
      in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = 
  Parser $ \input -> do
    (input', xs) <- p input 
    if null xs 
      then Nothing
      else Just (input', xs)

-- Seperates input by giving seperator
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

whiteSpace :: Parser String
whiteSpace = spanP isSpace

-- TODO add escape support
stringLiteral :: Parser String 
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

jsonString :: Parser JsonValue 
jsonString = JsonString <$> stringLiteral 

jsonNumber :: Parser JsonValue 
jsonNumber = f <$> notNull (spanP isDigit)
  where f ds = JsonNumber $ read ds

jsonArray :: Parser JsonValue 
jsonArray = JsonArray <$> (charP '[' *> whiteSpace *> elements <* whiteSpace <* charP ']')
  where elements = sepBy sep jsonValue
        sep = whiteSpace *> charP ',' <* whiteSpace

jsonObject :: Parser JsonValue 
jsonObject = JsonObject <$> (charP '{'
    *> whiteSpace *>
    sepBy (whiteSpace *> charP ',' <* whiteSpace) pair
    <* whiteSpace <*
    charP '}')
  where pair = (\key _ value-> (key, value)) <$> stringLiteral <*> (whiteSpace *> charP ':' *> whiteSpace) <*> jsonValue

jsonValue :: Parser JsonValue 
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject
