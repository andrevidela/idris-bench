module CommandLine

import Data.Maybe
import Data.String.Extra
import Data.Strings

%default total

data Flag = Short | Long | Both | Auto

FlagType : Flag -> Type
FlagType Short = Char
FlagType Long = String
FlagType Both = (Char, String)
FlagType Auto = String

data ParserGen : Type -> Type -> Type where
  MkParser : (input -> Maybe (t, input)) -> ParserGen input t

public export
interface HasParser type where
  parser : String -> Maybe type

export
HasParser String where
  parser = Just

export
HasParser Int where
  parser = parseInteger

runParser : ParserGen i t -> i -> Maybe (t, i)
runParser (MkParser p) i = p i

public export
Parser : Type -> Type
Parser = ParserGen (List String)

export
parseAll : Eq i => Monoid i => ParserGen i t -> i -> Maybe t
parseAll (MkParser p) input =
  case p input of
       Just (result, leftover) => toMaybe (leftover == neutral) result
       _ => Nothing

public export
Functor (ParserGen input) where
  map f (MkParser p) = MkParser (\input => do (o, rest) <- p input; pure (f o, rest))

mapInput : (i -> j) -> (j -> i) -> ParserGen i a -> ParserGen j a
mapInput f g (MkParser p) = MkParser $ \input => do (v, rest) <- p (g input)
                                                    Just (v, f rest)

public export
Applicative (ParserGen input) where
  pure v = MkParser $ Just . MkPair v
  (<*>) (MkParser f) (MkParser g) = MkParser $ \input => do (f, rest) <- f input
                                                            (v, rest') <- g rest
                                                            Just (f v, rest')

public export
Monad (ParserGen input) where
  (>>=) f g = MkParser $ \input => do (v , rest) <- runParser f input
                                      runParser (g v) rest

public export
Alternative (ParserGen input) where
  empty = MkParser (const Nothing)
  (<|>) a b = MkParser $ \input => runParser a input <|> runParser b input

public export
alt : ParserGen input a -> ParserGen input b -> ParserGen input (Either a b)
alt a b = map Left a <|> map Right b

public export
seq : ParserGen input a -> ParserGen input b -> ParserGen input (a, b)
seq a b = [| MkPair a b |]

public export
(*>) : ParserGen input a -> ParserGen input b -> ParserGen input b
(*>) a b = map snd (seq a b)

public export
(<*) : ParserGen input a -> ParserGen input b -> ParserGen input a
(<*) a b = map fst (seq a b)

public export
expect : (Eq token) => token -> ParserGen (List token) ()
expect input = MkParser $ \ls => case ls of
                                      (x :: xs) => toMaybe (x == input) ((), xs)
                                      _ => Nothing

public export
expectMany : (Eq token) => List token -> ParserGen (List token) ()
expectMany [] = pure ()
expectMany (x :: xs) = expect x >>= const (expectMany xs)

public export
expectStr : String -> ParserGen String ()
expectStr exp = let expectChars : ParserGen (List Char) () = expectMany (unpack exp) in
                    mapInput pack unpack expectChars

data CommandLine : Type -> Type where
  CmdFlag : (flag : Flag) -> FlagType flag -> (t : Type) -> HasParser t => CommandLine t
  BoolFlag : String -> CommandLine Bool
  CmdAdd  : CommandLine x -> CommandLine y -> CommandLine (Either x y)
  CmpMult : CommandLine x -> CommandLine y -> CommandLine (x, y)

parseToken : (input -> Maybe a) -> ParserGen (List input) a
parseToken (p) = MkParser $
  \input => case input of
                 [] => Nothing
                 (x :: xs) => do v <- p x
                                 Just (v, xs)

expectShort : HasParser result => Char -> Parser result
expectShort char = expect (pack ['-', char]) *> parseToken parser

expectLong : HasParser result => String -> Parser result
expectLong str = expect ("--" ++ str) *> parseToken parser

strHead : String -> Char
strHead str = case unpack str of
                   (x :: xs) => x
                   [] => ?lolno

toParser : CommandLine opt -> Parser opt
toParser (CmdFlag Short char v) = expectShort char
toParser (CmdFlag Long str v) = expectLong str
toParser (CmdFlag Both (c, str) v) = expectLong str <|> expectShort c
toParser (CmdFlag Auto str v) = expectLong str <|> expectShort (strHead str)
toParser (BoolFlag str) = (map (const True) $ expect ("--" ++ str)) <|> pure False
toParser (CmdAdd x y) = toParser x `alt` toParser y
toParser (CmpMult x y) = toParser x `seq` toParser y

export
longFlag : String -> (t : Type) -> HasParser t => Parser t
longFlag str t = expectLong str

export
flag : String -> (t : Type) -> HasParser t => Parser t
flag str t = expectLong str <|> expectShort (strHead str)

export
flagCheck : String -> Parser Bool
flagCheck str = (map (const True) $ expect ("--" ++ str)) <|> pure False


