{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import GHC.Generics
import Data.Void
import Control.Applicative
import Control.Arrow
import Control.Monad.Trans.State
import Data.Char
import Data.Maybe


class Unparse a where
    unparse :: a -> String

instance Unparse a => Unparse [a] where
    unparse = concatMap unparse

instance (Unparse a, Unparse b) => Unparse (Either a b) where
    unparse = either unparse unparse


data Lexeme
  = Alphanum   String
  | Symbol     String
  | Open       String
  | Close      String
  deriving (Show, Eq, Ord, Generic)

instance Unparse Lexeme where
    unparse (Alphanum s) = s
    unparse (Symbol   s) = s
    unparse (Open     s) = s
    unparse (Close    s) = s


data Whitespace
  = Newline
  | Blank   String
  | Comment String
  deriving (Show, Eq, Ord, Generic)

instance Unparse Whitespace where
    unparse Newline     = "\n"
    unparse (Blank   s) = s
    unparse (Comment s) = "//" ++ s

type LexemeW = LexemeVW' Void ()
type Source = [LexemeW]
type Pattern = [LexemeVW]

lwLexes :: Source -> [Lexeme]
lwLexes = mapMaybe lwLexemeOf
  where
    lwLexemeOf (LLex l) = Just l
    lwLexemeOf _         = Nothing

parseLexemeW :: String -> Source
parseLexemeW [] = []
parseLexemeW (c:cs) | isAlphaNum c = case parseLexemeW cs of
    LLex (Alphanum s) : ls -> LLex (Alphanum (c:s)) : ls
    ls                      -> LLex (Alphanum [c]  ) : ls
parseLexemeW ('\n':cs) = LWhite Newline : parseLexemeW cs
parseLexemeW (c:cs) | isSpace c = case parseLexemeW cs of
    LWhite (Blank s) : ls -> LWhite (Blank (c:s)) : ls
    ls                  -> LWhite (Blank [c]  ) : ls
parseLexemeW ('/':'/':cs) = LWhite (Comment comment) : parseLexemeW cs'
  where
    (comment, cs') = break (== '\n') cs
parseLexemeW ('{':cs) = LLex (Open  "{") : parseLexemeW cs
parseLexemeW ('[':cs) = LLex (Open  "[") : parseLexemeW cs
parseLexemeW ('(':cs) = LLex (Open  "(") : parseLexemeW cs
parseLexemeW (')':cs) = LLex (Close ")") : parseLexemeW cs
parseLexemeW (']':cs) = LLex (Close "]") : parseLexemeW cs
parseLexemeW ('}':cs) = LLex (Close "}") : parseLexemeW cs
parseLexemeW (c:cs) = case parseLexemeW cs of
    LLex (Symbol s) : ls -> LLex (Symbol (c:s)) : ls
    ls                    -> LLex (Symbol [c]  ) : ls

matchingParen :: String -> String
matchingParen "{" = "}"
matchingParen "[" = "]"
matchingParen "(" = ")"
matchingParen s   = error $ "unrecognized paren '%s'"


newtype Var = Var String
  deriving (Show, Eq, Ord, Generic)

instance Unparse Var where
    unparse (Var s) = s

isVar :: Lexeme -> Maybe Var
isVar (Symbol "...") = Just (Var "...")
isVar (Alphanum s) | all isUpper s = Just (Var s)
isVar _ = Nothing


type LexemeV = LexemeVW' () Void

parseLexemeV :: String -> [LexemeV]
parseLexemeV = fmap go . lwLexes . parseLexemeW
  where
    go :: Lexeme -> LexemeV
    go x = case isVar x of
        Just v  -> LVar v
        Nothing -> LLex x


data LexemeVW' var white
  = LVar' !var Var
  | LWhite' !white Whitespace
  | LLex Lexeme
  deriving (Eq, Show, Ord, Generic)

type LexemeVW = LexemeVW' () ()


pattern LVar :: Var -> LexemeVW' () white
pattern LVar v <- LVar' () v
  where
    LVar v = LVar' () v

pattern LWhite :: Whitespace -> LexemeVW' var ()
pattern LWhite v <- LWhite' () v
  where
    LWhite v = LWhite' () v

{-# COMPLETE LLex, LVar, LWhite #-}
{-# COMPLETE LLex, LVar #-}
{-# COMPLETE LLex, LWhite #-}


parseLexemeVW :: String -> [LexemeVW]
parseLexemeVW = fmap expandVars . parseLexemeW

expandVars :: LexemeW -> LexemeVW
expandVars (LWhite  w) = LWhite w
expandVars (LLex x) =
  case isVar x of
    Just v  -> LVar v
    Nothing -> LLex x
expandVars (LVar' void _) = absurd void

data Subst1 = Subst1
  { substVar :: Var
  , substReplacement :: Source
  }
  deriving (Eq, Show, Ord, Generic)

type Subst = [Subst1]

substitute11 :: Subst1 -> [LexemeVW] -> [LexemeVW]
substitute11 (Subst1 var replacement) = go
  where
    go :: [LexemeVW] -> [LexemeVW]
    go []                     = []
    go (LVar v:xs) | v == var = fmap lwToLVW replacement ++ xs
    go (x     :xs)            = x : go xs

substitute1 :: Subst1 -> [LexemeVW] -> [LexemeVW]
substitute1 subst = concatMap $ subst1 subst

subst1 :: Subst1 -> LexemeVW -> [LexemeVW]
subst1 (Subst1 var replacement) (LVar v) | v == var = fmap lwToLVW replacement
subst1 _ (x     )            = [x]

lwToLVW :: LexemeVW' a b -> LexemeVW
lwToLVW (LVar' _ w)   = LVar w
lwToLVW (LWhite' _ w) = LWhite w
lwToLVW (LLex w)      = LLex w

substitute :: Subst -> [LexemeVW] -> Source
substitute replacements = fmap assertR
                        . substituteAll replacements
                        . substituteOnce replacements
  where
    compose :: [a -> a] -> (a -> a)
    compose = foldr (>>>) id

    substituteOnce :: Subst -> [LexemeVW] -> [LexemeVW]
    substituteOnce = compose . fmap substitute11

    substituteAll :: Subst -> [LexemeVW] -> [LexemeVW]
    substituteAll = compose . fmap substitute1

    assertR :: LexemeVW -> LexemeW
    assertR (LVar (Var var)) = error $ "'%s' not in scope"
    assertR (LWhite x) = LWhite x
    assertR (LLex x) = LLex x


type Parser a = StateT Source [] a

runParser :: Parser a -> Source -> Maybe (a, Source)
runParser parser input = case runStateT parser input of
    []    -> Nothing
    (x:_) -> Just x

evalParser :: Parser a -> Source -> Maybe a
evalParser parser = fmap fst . runParser parser

pMaybeToken :: (LexemeW -> Maybe a) -> Parser a
pMaybeToken p = do
    (x:xs) <- get
    Just y <- return (p x)
    put xs
    return y

pExactToken :: LexemeW -> Parser ()
pExactToken x = pMaybeToken go
  where
    go :: LexemeW -> Maybe ()
    go x' | x == x' = Just ()
    go _ = Nothing

pWhitespace :: Parser Whitespace
pWhitespace = pMaybeToken go
  where
    go :: LexemeW -> Maybe Whitespace
    go (LWhite w) = Just w
    go _ = Nothing

pWhitespaces :: Parser [Whitespace]
pWhitespaces = ((:) <$> pWhitespace <*> pWhitespaces)
           <|> return []

pLexeme :: Parser Lexeme
pLexeme = pMaybeToken go
  where
    go :: LexemeW -> Maybe Lexeme
    go (LLex x) = Just x
    go _ = Nothing

pLexemeW :: Parser LexemeW
pLexemeW = pMaybeToken Just

pOpen :: Parser String
pOpen = pMaybeToken go
  where
    go :: LexemeW -> Maybe String
    go (LLex (Open s)) = Just s
    go _ = Nothing

pClose :: Parser String
pClose = pMaybeToken go
  where
    go :: LexemeW -> Maybe String
    go (LLex (Close s)) = Just s
    go _ = Nothing

-- not an Open nor a Close
pFlatLexemeW :: Parser LexemeW
pFlatLexemeW = pMaybeToken go
  where
    go :: LexemeW -> Maybe LexemeW
    go (LLex (Open  _)) = Nothing
    go (LLex (Close _)) = Nothing
    go x = Just x

-- not an Open nor a Close
pFlatLexeme :: Parser Lexeme
pFlatLexeme = pMaybeToken go
  where
    go :: LexemeW -> Maybe Lexeme
    go (LLex (Open  _)) = Nothing
    go (LLex (Close _)) = Nothing
    go (LLex x) = Just x
    go _ = Nothing

-- match as little as possible, possibly nothing.
pWildcard0 :: Parser Source
pWildcard0 = return []
         <|> ((++) <$> pNesting     <*> pWildcard0)
         <|> ((:)  <$> pFlatLexemeW <*> pWildcard0)

pWildcard :: Parser Source
pWildcard = ((++) <$> pNesting                <*> pWildcard0)
        <|> ((:)  <$> (LLex <$> pFlatLexeme) <*> pWildcard0)
        <|> ((:)  <$> (LWhite  <$> pWhitespace) <*> pWildcard )

pWildcardW :: Parser Source
pWildcardW = (++) <$> pWildcard <*> (fmap LWhite <$> pWhitespaces)

pNesting :: Parser Source
pNesting = do
    sOpen <- pOpen
    xs <- pWildcard0
    sClose <- pClose
    if matchingParen sOpen == sClose
    then return $ [LLex (Open sOpen)]
               ++ xs
               ++ [LLex (Close sClose)]
    else error $ "mismatched parens: '%s' and '%s'"


pMatchVar :: Var -> Parser Subst1
pMatchVar v = Subst1 <$> pure v <*> (clean <$> pWildcardW)
  where
    -- remove blanks at the beginning and end
    clean :: Source -> Source
    clean = reverse
          . dropWhile isBlank
          . reverse
          . dropWhile isBlank

    isBlank :: LexemeW -> Bool
    isBlank (LWhite (Blank _)) = True
    isBlank _ = False

pMatchPattern :: [LexemeV] -> Parser Subst
pMatchPattern []           = return []
pMatchPattern (LLex x:xs) = pWhitespaces
                          >> pExactToken (LLex x)
                          >> pMatchPattern xs
pMatchPattern (LVar  v:xs) = (:) <$> pMatchVar v <*> pMatchPattern xs
pMatchPattern (LWhite' void _:_) = absurd void

transliterate :: [LexemeV] -> [LexemeVW]
              -> Source -> Source
transliterate patternFrom patternTo = go
  where
    parser :: Parser Subst
    parser = pMatchPattern patternFrom

    go :: Source -> Source
    go [] = []
    go (x:xs) = case runParser parser (x:xs) of
        Just (subst, xs') -> substitute subst patternTo
                          ++ go xs'
        Nothing -> x : go xs


var :: Var -> LexemeVW' () a
var = LVar

lex :: Lexeme -> LexemeVW' a b
lex = LLex

white :: Whitespace -> LexemeVW' a ()
white = LWhite

mkSubst :: Var -> Source -> Subst1
mkSubst = Subst1

both :: Source -> Source -> Source
both = (++)

bothP :: Pattern -> Pattern -> Pattern
bothP = (++)

emptyP :: Pattern
emptyP = []

loosen :: LexemeW -> LexemeVW
loosen (LLex x) = LLex x
loosen (LWhite w) = LWhite w

main :: IO ()
main = pure ()

