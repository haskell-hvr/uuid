{-# LANGUAGE TemplateHaskell #-}
module Data.UUID.Orphans () where

import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text as T (pack, unpack)
import Data.UUID.Types (UUID, toString, fromString)
import Web.Routes.PathInfo

$(deriveSafeCopy 0 'base ''UUID)

instance PathInfo UUID where
  toPathSegments = (:[]) . T.pack . toString
  fromPathSegments = pToken (const ("UUID" :: String)) checkUUID
    where checkUUID txt = fromString (T.unpack txt)
