{-# LANGUAGE TemplateHaskell #-}

module Data.UUID.Quasi (uuid) where

import           Data.Maybe                 (fromMaybe)
import           Data.UUID                  (fromString, fromWords, toWords)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

uuid :: QuasiQuoter
uuid = QuasiQuoter
  { quoteExp  = uuidExp
  , quotePat  = \_ -> fail "illegal UUID QuasiQuote (allowed as expression only, used as a pattern)"
  , quoteType = \_ -> fail "illegal UUID QuasiQuote (allowed as expression only, used as a type)"
  , quoteDec  = \_ -> fail "illegal UUID QuasiQuote (allowed as expression only, used as a declaration)"
  }

uuidExp :: String -> Q Exp
uuidExp uuidStr =
  return $ AppE (AppE (AppE (AppE (VarE 'fromWords) w1e) w2e) w3e) w4e

  where
    (w1, w2, w3, w4) = toWords parsedUUID
    wordExp          = LitE . IntegerL . fromIntegral
    w1e              = wordExp w1
    w2e              = wordExp w2
    w3e              = wordExp w3
    w4e              = wordExp w4
    parsedUUID       = fromMaybe (error errmsg) $ fromString uuidStr
    errmsg           = "'" ++ uuidStr ++ "' is not a valid UUID"
