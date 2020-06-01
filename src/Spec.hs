{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec where

import Control.Monad
import Data.Char
import Data.Void
import Data.Proxy
import Test.QuickCheck
import QuickSpec
import Main hiding (main)


main :: IO ()
main = quickSpec
  [ withMaxTermSize 7
  , withMaxTests 1000
  , background
      [ con "map" $ map @A @B
      , con ">=>" $ (>=>) @[] @A @B @C
      , con "const" $ const @A @B
      ]
  , defaultTo $ Proxy @LexemeVW

  , con "subst1" subst1
  , monoTypeWithVars ["s"] $ Proxy @Subst1
  , monoTypeWithVars ["lv"] $ Proxy @LexemeV
  , monoTypeWithVars ["lw"] $ Proxy @LexemeW
  , monoTypeWithVars ["lvw"] $ Proxy @LexemeVW
  ]

instance Arbitrary Var where
  arbitrary = fmap (Var . mappend "VAR" . pure) $ elements ['1' .. '9']
  shrink = genericShrink

instance Arbitrary Subst1 where
  arbitrary = Subst1 <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (LexemeVW' () ()) where
  arbitrary = oneof
    [ LVar   <$> arbitrary
    , LWhite <$> arbitrary
    , LLex   <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary (LexemeVW' () Void) where
  arbitrary = oneof
    [ LVar   <$> arbitrary
    , LLex   <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary (LexemeVW' Void ()) where
  arbitrary = oneof
    [ LWhite <$> arbitrary
    , LLex   <$> arbitrary
    ]
  shrink = genericShrink

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

instance Arbitrary Void where
  arbitrary = error "void"

