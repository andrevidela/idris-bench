module CSV

import Text.Parser
import Text.Lexer
import Data.Either
import Data.Maybe
import Data.List
import Data.Strings

public export
CSV : Type
CSV = List (List String)

data CSVToken = Comma | CRLF | Content

Eq CSVToken where
  (==) Comma Comma = True
  (==) CRLF CRLF = True
  (==) Content Content = True
  (==) _ _ = False

TokenKind CSVToken where
  TokType Content = String
  TokType _ = ()

  tokValue Content = id
  tokValue Comma = const ()
  tokValue CRLF = const ()

quote : Lexer
quote = is '"'

stringLex : Lexer
stringLex = quote <+> manyUntil quote any <+> quote

plain : Lexer
plain = (non (is ',')) <+> manyUntil (is ',' <|> exact "\n") any

tokenMap : TokenMap (Token CSVToken)
tokenMap = toTokenMap $ [
  (is ',', Comma),
  (exact "\n", CRLF),
  (exact "\r\n", CRLF),
  (exact "\n\r", CRLF),
  (stringLex, Content),
  (plain, Content)]

export
lexLuthor: String -> Maybe (List (Token CSVToken))
lexLuthor str
  = case lex tokenMap str of
         (tokens, _, _, "") => Just $ map TokenData.tok tokens
         _ => Nothing

Syntax : Bool -> Type -> Type
Syntax = Grammar (Token CSVToken)

parseContent : Grammar (Token CSVToken) True String
parseContent = match Content

parseList : Grammar (Token CSVToken) True (List String)
parseList = sepBy1 (match Comma) parseContent

csvGrammar : Syntax True CSV
csvGrammar = sepBy1 (match CRLF) parseList

parseCSV' : List (Token CSVToken) -> Either (ParseError (Token CSVToken)) (CSV, List (Token CSVToken))
parseCSV' = parse csvGrammar

export
printCSV : CSV -> String
printCSV file =  unlines $ map (concat . intersperse ",") file

export
parseCSV : String -> Maybe CSV
parseCSV input = do
  tokens <- lexLuthor input
  map fst . eitherToMaybe . parseCSV' $ tokens

