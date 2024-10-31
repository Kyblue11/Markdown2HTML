{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (convertADTHTML, markdownParser, saveHTML)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty)
import          Control.Monad.IO.Class  (liftIO)
import Data.Maybe (fromMaybe)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= (pack value :: Text) | (key, value) <- pairs]


main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser str) convertADTHTML

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]
    

  post "/api/saveHTML" $ do
    requestBody <- body
    let requestBodyText = decodeUtf8 requestBody
        str = unpack requestBodyText
        formData = parseFormData str
        parsedTitle = fromMaybe "" $ lookup "title" formData
        parsedHtml = fromMaybe "" $ lookup "html" formData
    -- Save the HTML file
    liftIO $ saveHTML parsedTitle (removeInitialEquals parsedHtml)
    -- Respond with some success message
    json $ object ["status" .= ("success" :: String)]

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- Manually parse URL-encoded form data
parseFormData :: String -> [(String, String)]
parseFormData = map (break (== '=')) . split '&'
  where
    split _ [] = []
    split delim str =
      let (before, remainder) = break (== delim) str
      -- If the delimiter is found, split the string into two parts
      in before : case remainder of
                    [] -> []
                    x  -> split delim (tail x)

-- Remove initial '=' if present
removeInitialEquals :: String -> String
removeInitialEquals ('=':xs) = xs
removeInitialEquals str      = str