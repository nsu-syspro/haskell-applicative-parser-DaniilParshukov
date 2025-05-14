{-# OPTIONS_GHC -Wall #-}
-- The above pragma enables all warnings

module Task3 where

import Parser
import ParserCombinators
import Control.Applicative (empty, many, some, (<|>), optional)
import Data.Char (isSpace, isDigit, toLower)
import Data.List (intercalate)


skipMany :: Parser a -> Parser ()
skipMany p = many p *> pure ()

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- | JSON representation
--
-- See <https://www.json.org>
--
data JValue =
    JObject [(String, JValue)]
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
 deriving (Show, Eq)

-- | Parses JSON value
--
-- See full grammar at <https://www.json.org>
--
-- Usage example:
--
-- >>> parse json "{}"
-- Parsed (JObject []) (Input 2 "")
-- >>> parse json "null"
-- Parsed JNull (Input 4 "")
-- >>> parse json "true"
-- Parsed (JBool True) (Input 4 "")
-- >>> parse json "3.14"
-- Parsed (JNumber 3.14) (Input 4 "")
-- >>> parse json "{{}}"
-- Failed [PosError 0 (Unexpected '{'),PosError 1 (Unexpected '{')]
--
json :: Parser JValue
json = whitespace *> jvalue <* whitespace

whitespace :: Parser ()
whitespace = skipMany (satisfy isSpace)

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

symbol :: String -> Parser String
symbol s = string s <* whitespace

jvalue :: Parser JValue
jvalue = jnull <|> jbool <|> jnumber <|> jstring <|> jarray <|> jobject

jnull :: Parser JValue
jnull = JNull <$ symbol "null"

jbool :: Parser JValue
jbool = (JBool True <$ symbol "true") <|> (JBool False <$ symbol "false")

jnumber :: Parser JValue
jnumber = lexeme $ do
  sign <- optional (char '-')
  intPart <- (char '0' *> pure "0") <|> (some digit)
  fracPart <- optional (char '.' *> some digit)
  hasExp <- optional (oneOf "eE")
  expSign <- optional (oneOf "+-")
  expDigits <- optional (some digit)
  
  let numStr = maybe id (:) sign $
               intPart ++ 
               maybe "" ('.':) fracPart ++ 
               maybe "" (const "e") hasExp ++
               maybe "" (:[]) expSign ++
               maybe "" id expDigits
  case reads numStr of
    [(n, "")] -> pure $ JNumber n
    _         -> empty
  where
    digit = satisfy isDigit

jstring :: Parser JValue
jstring = lexeme $ do
  _ <- char '"'
  parts <- many (normalChar <|> escapeSeq)
  _ <- char '"'
  pure $ JString (concat parts)
  where
    normalChar = do
      c <- satisfy (\x -> x /= '"' && x /= '\\')
      pure [c]
    
    escapeSeq = do
      _ <- char '\\'
      c <- anyChar
      pure $ ['\\', c]
    
    anyChar = satisfy (const True)

jarray :: Parser JValue
jarray = JArray <$> (symbol "[" *> elements <* symbol "]")
  where
    elements = sepBy jvalue (symbol ",")

jobject :: Parser JValue
jobject = do
  _ <- symbol "{"
  ps <- sepBy pair (symbol ",")
  _ <- symbol "}"
  pure $ JObject ps
  where
    pair = do
      k <- jstring
      _ <- symbol ":"
      v <- jvalue
      case k of
        JString s -> pure (s, v)
        _ -> empty

readHex :: String -> Int
readHex = foldl (\acc c -> acc * 16 + digitToInt c) 0
  where
    digitToInt c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
      | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
      | otherwise = 0

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

-- * Rendering helpers

-- | Renders given JSON value as oneline string
render :: JValue -> String
render = concatMap readable . renderTokens
  where
    -- Adds some nice spacing for readability
    readable ":" = ": "
    readable "," = ", "
    readable s   = s

-- | Renders given JSON value as list of separate tokens ready for pretty printing
renderTokens :: JValue -> [String]
renderTokens JNull        = ["null"]
renderTokens (JBool b)    = [map toLower $ show b]
renderTokens (JNumber d)  = [show d]
renderTokens (JString s)  = ["\"" ++ s ++ "\""]
renderTokens (JArray xs)  = ["["] ++ intercalate [","] (map renderTokens xs) ++ ["]"]
renderTokens (JObject xs) = ["{"] ++ intercalate [","] (map renderPair xs) ++ ["}"]
 where
  renderPair :: (String, JValue) -> [String]
  renderPair (k, v) = ["\"" ++ k ++ "\""] ++ [":"] ++ renderTokens v

-- | Renders 'Parsed' or 'Failed' value as string
renderParsed :: Parsed JValue -> String
renderParsed (Parsed v _) = render v
renderParsed (Failed err) = show err

-- | Parses given file as JSON and renders result
renderJSONFile :: String -> IO String
renderJSONFile file = renderParsed <$> parseJSONFile file

-- | Parses given file as JSON
parseJSONFile :: String -> IO (Parsed JValue)
parseJSONFile file = parse json <$> readFile file
