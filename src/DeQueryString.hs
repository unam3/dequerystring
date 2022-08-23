{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}


module DeQueryString where

import Data.Proxy
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.Read
import qualified Data.Text as T
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits

-- https://discord.com/channels/280033776820813825/505367988166197268/937755222959337523
class FromQuery a where
  parseParams :: [(Text, Maybe Text)] -> Either String a
  default parseParams :: (Generic a, GFromQuery (Rep a)) => [(Text, Maybe Text)] -> Either String a
  parseParams = fmap GHC.Generics.to . gparseParams

class GFromQuery a where
  gparseParams :: [(Text, Maybe Text)] -> Either String (a x)

class HasParse a where
  parse :: Text -> Either String a

instance HasParse Int where
  parse = readEither . T.unpack

instance HasParse Text where
  parse = Right

--instance HasParse a => HasParse (Maybe a) where
--  parse = Right . Just . parse

instance HasParse a => HasParse (Vector a) where
  parse = fmap V.fromList . traverse parse . T.splitOn ","  . T.dropEnd 1 . T.drop 1 
  -- can't be arsed to write a proper parser

instance GFromQuery x => GFromQuery (D1 a (C1 b x)) where
  gparseParams = fmap (M1 . M1) . gparseParams

-- ' for https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/data_kinds.html#distinguishing-between-types-and-constructors
instance forall x y a b c d. (KnownSymbol x, HasParse y) => GFromQuery (S1 ('MetaSel ('Just x) a b c) (K1 d y)) where
    gparseParams params = case lookup (T.pack fieldName) params of
      Nothing -> Left $ "Couldn't find field " <> fieldName
      Just Nothing -> Left $ "Missing value for field " <> fieldName
      Just (Just val) -> do
        fieldVal <- parse val
        pure $ M1 $ K1 fieldVal

      where fieldName = symbolVal (Proxy :: Proxy x)

instance (GFromQuery x, GFromQuery y) => GFromQuery (x :*: y) where
  gparseParams params = (:*:) <$> gparseParams params <*> gparseParams params
