module Main where

import Control.Applicative
import JParser
import Test.HUnit

-- Helper function to run parser and get result
parse :: Parser a -> String -> Maybe a
parse parser input = case runParser parser input of
  Just ("", result) -> Just result
  _ -> Nothing

-- Basic Parser Tests
testCharP :: Test
testCharP =
  TestList
    [ "charP success" ~: Just 'a' ~=? parse (charP 'a') "a",
      "charP failure" ~: Nothing ~=? parse (charP 'a') "b",
      "charP empty" ~: Nothing ~=? parse (charP 'a') ""
    ]

testStringP :: Test
testStringP =
  TestList
    [ "stringP success" ~: Just "hello" ~=? parse (stringP "hello") "hello",
      "stringP failure" ~: Nothing ~=? parse (stringP "hello") "world",
      "stringP partial" ~: Nothing ~=? parse (stringP "hello") "hel"
    ]

-- JSON Null Tests
testJsonNull :: Test
testJsonNull =
  TestList
    [ "parse null" ~: Just JsonNull ~=? parse jsonNull "null",
      "reject invalid null" ~: Nothing ~=? parse jsonNull "nul",
      "reject different word" ~: Nothing ~=? parse jsonNull "NULL"
    ]

-- JSON Boolean Tests
testJsonBool :: Test
testJsonBool =
  TestList
    [ "parse true" ~: Just (JsonBool True) ~=? parse jsonBool "true",
      "parse false" ~: Just (JsonBool False) ~=? parse jsonBool "false",
      "reject invalid bool" ~: Nothing ~=? parse jsonBool "TRUE",
      "reject partial bool" ~: Nothing ~=? parse jsonBool "tru"
    ]

-- JSON String Tests
testJsonString :: Test
testJsonString =
  TestList
    [ "parse empty string" ~: Just (JsonString "") ~=? parse jsonString "\"\"",
      "parse simple string" ~: Just (JsonString "hello") ~=? parse jsonString "\"hello\"",
      "reject unquoted string" ~: Nothing ~=? parse jsonString "hello",
      "reject unclosed string" ~: Nothing ~=? parse jsonString "\"hello"
    ]

-- JSON Number Tests
testJsonNumber :: Test
testJsonNumber =
  TestList
    [ "parse single digit" ~: Just (JsonNumber 5) ~=? parse jsonNumber "5",
      "parse multiple digits" ~: Just (JsonNumber 123) ~=? parse jsonNumber "123",
      "reject invalid number" ~: Nothing ~=? parse jsonNumber "12a",
      "reject empty" ~: Nothing ~=? parse jsonNumber ""
    ]

-- JSON Array Tests
testJsonArray :: Test
testJsonArray =
  TestList
    [ "parse empty array" ~: Just (JsonArray []) ~=? parse jsonArray "[]",
      "parse single element"
        ~: Just (JsonArray [JsonNumber 1])
        ~=? parse jsonArray "[1]",
      "parse multiple elements"
        ~: Just (JsonArray [JsonNumber 1, JsonNumber 2])
        ~=? parse jsonArray "[1,2]",
      "parse nested array"
        ~: Just (JsonArray [JsonArray [JsonNumber 1]])
        ~=? parse jsonArray "[[1]]",
      "parse with whitespace"
        ~: Just (JsonArray [JsonNumber 1, JsonNumber 2])
        ~=? parse jsonArray "[ 1 , 2 ]"
    ]

-- JSON Object Tests
testJsonObject :: Test
testJsonObject =
  TestList
    [ "parse empty object" ~: Just (JsonObject []) ~=? parse jsonObject "{}",
      "parse single pair"
        ~: Just (JsonObject [("key", JsonNumber 1)])
        ~=? parse jsonObject "{\"key\":1}",
      "parse multiple pairs"
        ~: Just (JsonObject [("k1", JsonNumber 1), ("k2", JsonNumber 2)])
        ~=? parse jsonObject "{\"k1\":1,\"k2\":2}",
      "parse nested object"
        ~: Just (JsonObject [("obj", JsonObject [("key", JsonNumber 1)])])
        ~=? parse jsonObject "{\"obj\":{\"key\":1}}",
      "parse with whitespace"
        ~: Just (JsonObject [("key", JsonNumber 1)])
        ~=? parse jsonObject "{ \"key\" : 1 }"
    ]

-- Complex JSON Value Tests
testJsonValue :: Test
testJsonValue =
  TestList
    [ "parse complex nested structure"
        ~: Just
          ( JsonObject
              [ ("name", JsonString "John"),
                ("age", JsonNumber 30),
                ("isStudent", JsonBool False),
                ("grades", JsonArray [JsonNumber 85, JsonNumber 92]),
                ( "address",
                  JsonObject
                    [ ("street", JsonString "123 Main St"),
                      ("city", JsonString "Anytown")
                    ]
                )
              ]
          )
        ~=? parse jsonValue "{\"name\":\"John\",\"age\":30,\"isStudent\":false,\"grades\":[85,92],\"address\":{\"street\":\"123 Main St\",\"city\":\"Anytown\"}}"
    ]

-- White Space Tests
testWhiteSpace :: Test
testWhiteSpace =
  TestList
    [ "parse spaces" ~: Just "   " ~=? parse whiteSpace "   ",
      "parse mixed whitespace" ~: Just "\t \n " ~=? parse whiteSpace "\t \n ",
      "parse no whitespace" ~: Just "" ~=? parse whiteSpace "abc"
    ]

-- Run all tests
tests :: Test
tests =
  TestList
    [ TestLabel "CharP Tests" testCharP,
      TestLabel "StringP Tests" testStringP,
      TestLabel "JSON Null Tests" testJsonNull,
      TestLabel "JSON Boolean Tests" testJsonBool,
      TestLabel "JSON String Tests" testJsonString,
      TestLabel "JSON Number Tests" testJsonNumber,
      TestLabel "JSON Array Tests" testJsonArray,
      TestLabel "JSON Object Tests" testJsonObject,
      TestLabel "Complex JSON Tests" testJsonValue,
      TestLabel "White Space Tests" testWhiteSpace
    ]

main :: IO Counts
main = runTestTT tests
