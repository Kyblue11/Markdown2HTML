module Assignment (markdownParser, convertADTHTML,saveHTML) where


import           Data.Time.Clock     (getCurrentTime)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Instances           (Parser (..), ParseResult (..), ParseError (..))
import           Parser              (failed, unexpectedCharParser, char, int, eof, satisfy, is, isNot, oneof, noneof, digit, space, lower, upper, alpha, spaces, spaces1, inlineSpace, string, tok, charTok, commaTok, stringTok)
import           Control.Applicative (Alternative (..), optional)
import           Data.Char           (isSpace)
import           Control.Monad       (when)
import           Data.Functor        (($>))

----------------------------------------------------------------------------
-- PART C: SAVE HTML

getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" <$> getCurrentTime

saveHTML :: String -> String -> IO ()
saveHTML title html = do
    timestamp <- getTime
    let filename = "test\\" ++ timestamp ++ ".html"
    writeFile filename html


----------------------------------------------------------------------------
-- PART B: HTML CONVERSION

convertADTHTML :: ADT -> String
convertADTHTML (ADT adts) = concat
  [ "<!DOCTYPE html>\n"
  , "<html lang=\"en\">\n"
  , "\n"
  , "<head>\n"
  , "    <meta charset=\"UTF-8\">\n"
  , "    <title>Test</title>\n"
  , "</head>\n"
  , "\n"
  , "<body>\n"
  , concatMap (convertADT 1) adts
  , "</body>\n"
  , "\n"
  , "</html>"
  , "\n"
  ]

-- Convert each high-level ADT type to HTML
convertADT :: Int -> AdtType -> String
convertADT level (FreeTextADT freeText) = convertFreeTextADT level freeText
convertADT level (FootnoteReference num content) = indent level (convertFootnoteReference num content)
convertADT level (Image alt url title) = convertImage alt url title
convertADT level (Heading level' freeText) = indent level (convertHeading level' freeText)
convertADT level (Blockquote freeTexts) = convertBlockquote level freeTexts
convertADT level (CodeBlock language code) = convertCodeBlock language code
convertADT level (OrderedListADT items) = convertOrderedList level items
convertADT level (Table rows) = convertTable level rows
convertADT level (EmptyLineADT s) = indent level ("<p>" ++ s ++ "</p>")

-- Req 4: Free text --
convertFreeTextADT :: Int -> FreeText -> String
convertFreeTextADT level = indent level . wrapInParagraph . convertFreeText

convertFreeText :: FreeText -> String
convertFreeText (FreeText chunks) = concatMap convertFreeTextorModifier chunks

convertFreeTextorModifier :: FreeTextorModifier -> String
convertFreeTextorModifier (FreeTextChunk str) = str
convertFreeTextorModifier (TextModifier modifier) = convertTextModifier modifier

convertFreeTextChunk :: FreeTextorModifier -> String
convertFreeTextChunk (FreeTextChunk str) = str
convertFreeTextChunk (TextModifier modifier) = convertTextModifier modifier

-- Req 1: Text Modifiers --
convertTextModifier :: TextModifier -> String
convertTextModifier (Italic ft) = "<em>" ++ convertFreeText ft ++ "</em>"
convertTextModifier (Bold ft) = "<strong>" ++ convertFreeText ft ++ "</strong>"
convertTextModifier (Strikethrough ft) = "<del>" ++ convertFreeText ft ++ "</del>"
convertTextModifier (Link ft url) = "<a href=\"" ++ url ++ "\">" ++ convertFreeText ft ++ "</a>"
convertTextModifier (InlineCode ft) = "<code>" ++ convertFreeText ft ++ "</code>"
convertTextModifier (Footnote num) =
  "<sup><a id=\"fn" ++ show num ++ "ref\" href=\"#fn" ++ show num ++ "\">" ++ show num ++ "</a></sup>"

-- Req 2: Images --
convertImage :: String -> String -> String -> String
convertImage alt url title =
  "    <img src=\"" ++ url ++ "\" alt=\""
  ++ alt ++ "\" title=\"" ++ title ++ "\">\n"

-- Req 3: Footnote References --
convertFootnoteReference :: Int -> String -> String
convertFootnoteReference num content = "<p id=\"fn" ++ show num ++ "\">" ++ content ++ "</p>"

-- Req 5: Headings --
convertHeading :: Int -> FreeText -> String
convertHeading level (FreeText content) =
  "<h" ++ show level ++ ">"
  ++ concatMap convertFreeTextChunk content ++ "</h" ++ show level ++ ">"

-- Req 6: Blockquotes --
convertBlockquote :: Int -> [FreeText] -> String
convertBlockquote level freeTexts =
  indent level "<blockquote>\n"-- wrap the indentation below in a blockquote tag
  ++ concatMap (indent (level + 1) . wrapInParagraph . convertFreeText) freeTexts
  ++ indent level "</blockquote>"

-- Req 7: CodeBlock --
convertCodeBlock :: String -> String -> String
convertCodeBlock language code = -- there may be no language specified
  let classAttr = if null language then "" else " class=\"language-" ++ language ++ "\""
  in "    <pre><code" ++ classAttr ++ ">" ++ code ++ "</code></pre>" ++ "\n"

-- Req 8: Ordered Lists --
convertOrderedList :: Int -> [OrderedListItem] -> String
convertOrderedList level items =
  indent level "<ol>\n" ++ concatMap (convertOrderedListItem (level + 1)) items
  ++ indent level "</ol>\n"

convertOrderedListItem :: Int -> OrderedListItem -> String
convertOrderedListItem level (OrderedListItem text maybeSubList) =
  let itemContent = convertFreeText text
      -- try to convert the sublist if it exists, or fallback to an empty string
      subListContent = maybe "" (\subList -> "\n" ++ convertOrderedList (level - 1)
                                (getOrderedListItems subList) ++ indent level "") maybeSubList
  in indent level ("<li>" ++ itemContent ++ subListContent ++ "</li>\n")

getOrderedListItems :: OrderedList -> [OrderedListItem]
getOrderedListItems (OrderedList items) = items

-- Req 9: Tables --
convertTable :: Int -> [TableRow] -> String
convertTable level rows =
  indent level "<table>\n"
  ++ convertTableHeader (level + 1) (head rows)
  ++ convertTableBody (level + 1) (tail rows)
  ++ indent level "</table>\n"

convertTableHeader :: Int -> TableRow -> String
convertTableHeader level (TableRow cells) =
  indent level "<thead>\n"
  ++ indent (level + 1) "<tr>\n" -- wrap the indented String in a tr tag
  ++ concatMap (indent (level + 2) . wrapInTh . convertFreeText) cells
  ++ indent (level + 1) "</tr>\n" ++ indent level "</thead>\n"

convertTableBody :: Int -> [TableRow] -> String
convertTableBody level rows =
  indent level "<tbody>\n"
  ++ concatMap (convertTableRow (level + 1)) rows
  ++ indent level "</tbody>\n"

convertTableRow :: Int -> TableRow -> String
convertTableRow level (TableRow cells) =
  indent level "<tr>\n"
  ++ concatMap (indent (level + 1) . wrapInTd . convertFreeText) cells
  ++ indent level "</tr>\n"

wrapInTh :: String -> String
wrapInTh content = "<th>" ++ content ++ "</th>\n"

wrapInTd :: String -> String
wrapInTd content = "<td>" ++ content ++ "</td>\n"

-- Helper function to indent HTML content
indent :: Int -> String -> String
indent level = unlines . map (replicate (level * 4) ' ' ++) . lines

-- Function to wrap FreeText in <p> tags only when needed
wrapInParagraph :: String -> String
wrapInParagraph content = "<p>" ++ content ++ "</p>"


----------------------------------------------------------------------------


----------------------------------------------------------------------------
-- PART A: PASSING MARKDOWN

newtype OrderedList = OrderedList [OrderedListItem]
  deriving (Show, Eq)

data OrderedListItem = OrderedListItem FreeText (Maybe OrderedList)
  deriving (Show, Eq)

newtype TableRow = TableRow [FreeText]
  deriving (Show, Eq)

data TextModifier
  = Italic FreeText
  | Bold FreeText
  | Strikethrough FreeText
  | Link FreeText String
  | InlineCode FreeText
  | Footnote Int
  deriving (Show, Eq)

newtype FreeText = FreeText [FreeTextorModifier]
  deriving (Show, Eq)

data FreeTextorModifier
  = FreeTextChunk String
  | TextModifier TextModifier
  deriving (Show, Eq)

data AdtType
  = Image String String String
  | FootnoteReference Int String
  | Heading Int FreeText
  | Blockquote [FreeText]
  | CodeBlock String String
  | OrderedListADT [OrderedListItem]
  | Table [TableRow]
  | EmptyLineADT String
  | FreeTextADT FreeText

  deriving (Show, Eq)

newtype ADT = ADT [AdtType]
  deriving (Show, Eq)

----------------------------------------------------------------------------

-- Wrap the entire markdownParser in an ADT type
markdownParser :: Parser ADT
markdownParser = do
  ADT <$> parseAllADTs

-- Parse all ADT types in the input
parseAllADTs :: Parser [AdtType]
parseAllADTs = do
  input <- manyTill anyChar eof
  let trimmedInput = trim input
  case parseInput trimmedInput of
    Result _ res -> pure res
    Error err    -> failed err

-- Return the result of the parser
parseInput :: String -> ParseResult  [AdtType]
parseInput ""    = Result "" []
parseInput input = parse (some parseAnyADT) input 

-- Parse any ADT type according to precedence
parseAnyADT :: Parser AdtType
parseAnyADT
   =  parseImage
  <|> parseFootnoteReference
  <|> parseHeading
  <|> parseBlockquote
  <|> parseCodeBlock
  <|> parseOrderedList
  <|> parseTable
  <|> parseEmptyLine
  <|> parseFreeTextADT

-- Parse a free text ADT type by wrapping it in a FreeTextADT constructor
parseFreeTextADT :: Parser AdtType
parseFreeTextADT = FreeTextADT <$> parseFreeTextLine

-- Any <p> tag with no content, or only whitespace, should be considered an empty line
parseEmptyLine :: Parser AdtType
parseEmptyLine = do
  s <- optional inlineSpace
  _ <- is '\n'
  pure $ EmptyLineADT (maybe "" id s)

-- Parse a free text line
parseModifier :: Parser FreeTextorModifier
parseModifier = TextModifier <$> (
      parseItalic
  <|> parseBold
  <|> parseStrikethrough
  <|> parseLink
  <|> parseInlineCode
  <|> parseFootnote
  )


-- Req 1: Text Modifiers --
parseItalic :: Parser TextModifier
parseItalic = do
  content <- parseDelimitedContentM "_" "_"
  pure (Italic (FreeText content))

parseBold :: Parser TextModifier
parseBold = do
  content <- parseDelimitedContentM "**" "**"
  pure (Bold (FreeText content))

parseStrikethrough :: Parser TextModifier
parseStrikethrough = do
  content <- parseDelimitedContentM "~~" "~~"
  pure (Strikethrough (FreeText content))

parseLink :: Parser TextModifier
parseLink = do
  link <- parseDelimitedContentM "[" "]"
  _ <- spaces
  url <- parseDelimitedContent "(" ")"
  pure (Link (FreeText link) url)

parseInlineCode :: Parser TextModifier
parseInlineCode = do
  content <- parseDelimitedContentM "`" "`"
  pure (InlineCode (FreeText content))

parseFootnote :: Parser TextModifier
parseFootnote = do
  Footnote <$> parseFootnoteNumber


-- Req2: Images --
parseImage :: Parser AdtType
parseImage = do
  _ <- inlineSpace
  altText <- parseDelimitedContent "![" "]"
  _ <- spaces
  _ <- string "("
  url <- some (isNot ' ')
  _ <- spaces1
  caption <- parseDelimitedContent "\"" "\")"
  _ <- isolatedLineAbv
  pure (Image altText url caption)


-- Req3: Footnote Reference --
parseFootnoteReference :: Parser AdtType
parseFootnoteReference = do
  number <- parseFootnoteNumber
  _ <- string ":"
  _ <- inlineSpace
  referenceText <- some (noneof "\n")
  _ <- optional (string "\n")
  pure (FootnoteReference number referenceText)


-- Req4: Free text --
parseFreeTextLine :: Parser FreeText
parseFreeTextLine = do
  input <- many (noneof "\n")
  _ <- optional (string "\n")
  parseOneLine input

parseOneLine :: String -> Parser FreeText
parseOneLine ""     =  failed $ UnexpectedChar 'E'
parseOneLine input  =  case parse (some parseModifierOrFree) input of
    Result _ chunks -> pure (FreeText chunks)
    _               -> pure (FreeText [FreeTextChunk input])

parseModifierOrFree :: Parser FreeTextorModifier
parseModifierOrFree =  parseModifier <|> parseFreeTextChunk <|> parseFakeModifierSymbol

parseFreeTextChunk :: Parser FreeTextorModifier
parseFreeTextChunk = do
  content <- some (noneof "_*~`[]")
  pure (FreeTextChunk content)

-- Mini-parser for handling "fake" modifier symbols like single '*' or '~'
-- An edge case where the user might want to include these symbols in the text
parseFakeModifierSymbol :: Parser FreeTextorModifier
parseFakeModifierSymbol = do
    symbol <- oneof "_*~`[]"
    pure (FreeTextChunk [symbol])


-- Req 5: Heading --
-- Note: The heading.diff is different because the 
-- provided input markdown had hardcoded two empty lines
parseHeading :: Parser AdtType
parseHeading = parseHashHeading <|> parseAlternativeHeading

parseHashHeading :: Parser AdtType
parseHashHeading = do
  _ <- inlineSpace
  level <- length <$> some (is '#')
  _ <- is ' '
  _ <- inlineSpace
  content <- parseFreeTextLine
  parseHashHeadingLevel level content

parseHashHeadingLevel :: Int -> FreeText -> Parser AdtType
parseHashHeadingLevel level content
  | level <= 6 = pure (Heading level content)
  | otherwise  = unexpectedCharParser '#'

parseAlternativeHeading :: Parser AdtType
parseAlternativeHeading = do
  _ <- inlineSpace
  content <- parseFreeTextLine
  _ <- inlineSpace
  firstChar <- is '=' <|> is '-'
  let expectedChar = if firstChar == '=' then '=' else '-'
  _ <- some (is expectedChar)
  _ <- isolatedLineAbv
  pure $ parseAlternativeHeadingLevel expectedChar content

parseAlternativeHeadingLevel :: Char -> FreeText -> AdtType
parseAlternativeHeadingLevel '=' content = Heading 1 content
parseAlternativeHeadingLevel '-' content = Heading 2 content
parseAlternativeHeadingLevel _ _ = error "Invalid alt heading character"


-- Req 6: Blockquotes --
parseBlockquote :: Parser AdtType
parseBlockquote = do
  quotes <- some parseBlockquoteLine
  pure (Blockquote quotes)

parseBlockquoteLine :: Parser FreeText
parseBlockquoteLine = do
  _ <- inlineSpace
  _ <- string ">"
  _ <- inlineSpace
  parseFreeTextLine


-- Req 7: CodeBlock
parseCodeBlock :: Parser AdtType
parseCodeBlock = do
  _ <- inlineSpace
  _ <- string "```"
  language <- some (noneof "\n")  <|> pure "" -- optional language
  _ <- string "\n"
  content <- parseDelimitedContent "" "\n```"
  _ <- isolatedLineAbv
  pure (CodeBlock language content)


-- Req 8: Ordered Lists --
parseOrderedList :: Parser AdtType
parseOrderedList = do
  firstItem <- parseOrderedListItem 0 True
  items <- many (parseOrderedListItem 0 False)
  pure (OrderedListADT (firstItem : items))

parseOrderedListItem :: Int -> Bool -> Parser OrderedListItem
parseOrderedListItem level isFirst = do
  -- Each level of nesting is 4 spaces, but ignore the result
  when (level > 0) (string (replicate (4 * level) ' ') $> ())
  -- Decide whether its the first item in the list
  number <- parseListNumber isFirst
  _ <- string ". "
  content <- parseFreeTextLine
  -- Parse the sublist if it exists
  sublist <- optional (parseSubOrderedList (level + 1))
  _ <- optional (string "\n")
  pure (OrderedListItem content sublist)

parseListNumber :: Bool -> Parser String
parseListNumber True  = string "1"
parseListNumber False = show <$> positiveInt

parseSubOrderedList :: Int -> Parser OrderedList
parseSubOrderedList level = do
  firstItem <- parseOrderedListItem level True
  items <- many (parseOrderedListItem level False)
  pure (OrderedList (firstItem : items))


-- Req 9: Tables --
-- Note: The tables.diff may be different due to part D implementation
parseTable :: Parser AdtType
parseTable = do
  (header, headerRawLengths) <- parseTableRow
  let headerLen = length (extractCells header)
  separatorLen <- parseSeparatorRow
  parseTableRows header headerLen headerRawLengths separatorLen

parseTableRows :: TableRow -> Int -> [Int] -> Int -> Parser AdtType
parseTableRows header headerLen headerRawLengths separatorLen
  | separatorLen == headerLen = do
      rows <- many (parseTableRowWithCheck headerLen headerRawLengths)
      pure (Table (header : rows))
  | otherwise = failed $ UnexpectedChar 's'

extractCells :: TableRow -> [FreeText]
extractCells (TableRow cells) = cells

-- Destructor for TableRow
parseTableRowWithCheck :: Int -> [Int] -> Parser TableRow
parseTableRowWithCheck expectedLen expectedRawLengths = do
  (row, rawLengths) <- parseTableRow
  parseTableRowLengthCheck row expectedLen rawLengths expectedRawLengths

-- Ensure that the row has the same number of cells and raw lengths as the header
parseTableRowLengthCheck :: TableRow -> Int -> [Int] -> [Int] -> Parser TableRow
parseTableRowLengthCheck row expectedLen rawLengths expectedRawLengths
  | length (extractCells row) == expectedLen && rawLengths == expectedRawLengths = pure row
  | otherwise = failed $ UnexpectedChar 't'

-- This new data type is important so that we can 
-- evaluate the raw content and the trimmed content separately
data RawCell = RawCell { rawContent :: String, trimmedContent :: FreeText }
  deriving (Show, Eq)

parseTableRow :: Parser (TableRow, [Int])
parseTableRow = do
  _ <- inlineSpace
  _ <- is '|'
  cells <- sepBy trimEachCell (is '|')
  _ <- optional $ string "\n"
  -- Remove empty cells that appear at the end of the each row
  let nonEmptyCells = filter (\(RawCell _ trimmed) ->
        trimmed /= FreeText [FreeTextChunk ""]) cells
  let row = TableRow (map trimmedContent nonEmptyCells)
  -- Get the raw lengths of the cells as a separate list
  let rawLengths = map (length . rawContent) cells
  pure (row, rawLengths)

-- Parse each cell after trimming the whitespace encapsulating the content
trimEachCell :: Parser RawCell
trimEachCell = do
  rawInput <- many (noneof "|\n")
  let trimmedInput = trim rawInput
  trimmedCell <- parseTrimmedCell trimmedInput
  pure (RawCell rawInput trimmedCell)

parseTrimmedCell :: String -> Parser FreeText
parseTrimmedCell ""     =  pure (FreeText [FreeTextChunk ""])
parseTrimmedCell input  =  case parse (some parseModifierOrFree) input of
        Result _ chunks -> pure (FreeText chunks)
        _               -> pure (FreeText [FreeTextChunk input])


parseSeparatorRow :: Parser Int
parseSeparatorRow = do
  _ <- inlineSpace
  _ <- is '|'
  cells <- sepBy parseDashes (is '|')
  _ <- is '|'
  _ <- string "\n"
  pure (length cells)

parseDashes :: Parser ()
parseDashes = do
  _ <- inlineSpace
  _ <- string "---"
  _ <- many (is '-')
  _ <- inlineSpace
  pure ()

----------------------------------------------------------------------------
-- Helper functions
----------------------------------------------------------------------------

-- Ensure that the input does not have some parser in the beginning
notFollowedBy :: Parser a -> Parser ()
notFollowedBy p = do
  res <- optional p
  maybe (pure ()) (const $ failed $ UnexpectedChar ' ') res

-- MUST be a positive integer
positiveInt :: Parser Int
positiveInt = do
  notFollowedBy spaces1
  x <- int
  checkPositive x

checkPositive :: Int -> Parser Int
checkPositive x
  | x > 0     = pure x
  | otherwise = failed $ UnexpectedChar '-'

-- Parse a footnote number in the format [^Z+]
parseFootnoteNumber :: Parser Int
parseFootnoteNumber = do
  _ <- inlineSpace
  _ <- string "[^"
  num <- positiveInt
  _ <- is ']'
  return num

----------------------------------------------------------------------------

-- Parse many of a parser until the end parser is reached
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = (end $> []) <|> ((:) <$> p <*> go)

-- manyTill variant that requires at least one successful parse
someTill :: Parser a -> Parser end -> Parser [a]
someTill p end = (:) <$> p <*> manyTill p end

-- Parse any single character
anyChar :: Parser Char
anyChar = satisfy (const True)

-- Parse general content between two delimiters
parseDelimitedContent :: String -> String -> Parser String
parseDelimitedContent opening closing = do
  _ <- string opening
  someTill anyChar (string closing $> "")

-- A different variant for parsing nested FreeTextorModifiers
parseDelimitedContentM :: String -> String -> Parser [FreeTextorModifier]
parseDelimitedContentM opening closing = do
  _ <- string opening
  someTill parseModifierOrFree (string closing)

----------------------------------------------------------------------------

-- Reverse the string, drop spaces, reverse again
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Implementation of the `sepBy` function from Week 9 Tutorial
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

-- On a line by themselves
isolatedLineAbv :: Parser ()
isolatedLineAbv = eof <|> (string "\n" $> ())