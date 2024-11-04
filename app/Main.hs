module Main where

data JsonValue = JsonNull
                | JsonBool Bool 
                | JsonNumber Integer -- Implement Floating point numbers
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

jsonNull :: Parser JsonValue 
jsonNull = undefined


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

jsonValue :: Parser JsonValue 
jsonValue = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
