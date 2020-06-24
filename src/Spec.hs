{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec where

import Control.Monad
import Data.Char
import Data.Proxy
import Test.QuickCheck
import QuickSpec
import Main hiding (main)
import Prelude hiding (lex)

main :: IO ()
main = quickSpec
  [ withMaxTermSize 8
  , withMaxTests 1000
  -- , defaultTo $ Proxy @LexemeVW

  , background
    [ -- con "pure" $ pure @[] @A
    -- , lists
     -- con "map" $ map @A @B
      con ">=>" $ (>=>) @[] @A @B @C
    ]

  , con "subst1" subst1
  -- , con "substitute1" substitute1
  -- , con "var" $ var @A
  -- , con "lex" $ lex @A @B
  -- , con "white" $ white @A
  -- , con "mkSubst" mkSubst
  -- , con "inj" loosen

  , monoTypeWithVars ["s"]   $ Proxy @Subst1
  , monoTypeWithVars ["lv"]  $ Proxy @LexemeV
  , monoTypeWithVars ["lw"]  $ Proxy @LexemeW
  , monoTypeWithVars ["lvw"] $ Proxy @LexemeVW
  , monoTypeWithVars ["l"]   $ Proxy @Lexeme
  , monoTypeWithVars ["v"]   $ Proxy @Var
  , monoTypeWithVars ["w"]   $ Proxy @Whitespace
  , monoTypeWithVars ["src"]   $ Proxy @Source
  , monoTypeWithVars ["pat"]   $ Proxy @Pattern

  -- , monoTypeObserve $ Proxy @Void
  ]

instance Arbitrary Var where
  arbitrary = fmap (Var . mappend "VAR" . pure) $ elements ['1' .. '9']
  shrink = genericShrink

instance Arbitrary Subst1 where
  arbitrary = Subst1 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary LexemeVW where
  arbitrary = oneof
    [ LVar   <$> arbitrary
    , LWhite <$> arbitrary
    , LLex   <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary LexemeV where
  arbitrary = oneof
    [ LVar   <$> arbitrary
    , LLex   <$> arbitrary
    ]
  -- shrink = genericShrink

instance Arbitrary LexemeW where
  arbitrary = oneof
    [ LWhite <$> arbitrary
    , LLex   <$> arbitrary
    ]
  -- shrink = genericShrink

instance Arbitrary Lexeme where
  arbitrary = oneof
    [ fmap Alphanum $ listOf $ elements $ ['0'..'9'] ++ ['A'..'Z']
    , fmap Symbol $ listOf $ arbitrary `suchThat` isSymbol
    , fmap (Open . pure) $ elements ['(', '{', '[']
    , fmap (Close . pure) $ elements [')', '}', ']']
    ]
  shrink = genericShrink

instance Arbitrary Whitespace where
  arbitrary = oneof
    [ pure Newline
    , fmap Blank $ listOf $ pure ' '
    , fmap (Comment . getPrintableString) arbitrary
    ]
  shrink = genericShrink

