{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

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

--instance HasParse a => HasParse (Vector a) where
--  parse = fmap V.fromList . traverse parse . T.splitOn ","  . T.dropEnd 1 . T.drop 1 
--  -- can't be arsed to write a proper parser

instance GFromQuery x => GFromQuery (D1 a (C1 b x)) where
  gparseParams = fmap (M1 . M1) . gparseParams

instance (KnownSymbol x, HasParse y) => GFromQuery (S1 ('MetaSel ('Just x) a b c) (K1 d y)) where
    gparseParams params = case lookup (T.pack fieldName) params of
      Nothing -> Left $ "Couldn't find field " <> fieldName
      Just Nothing -> Left $ "Missing value for field " <> fieldName
      Just (Just val) -> do
        fieldVal <- parse val
        pure $ M1 $ K1 fieldVal

      where fieldName = symbolVal (Proxy :: Proxy x)
--instance (HasParse y) => GFromQuery (S1 ('MetaSel ('Just x) a b c) (K1 d y)) where
--    gparseParams params = case lookup x params of
--      Nothing -> Left $ "Couldn't find field " <> fieldName
--      Just Nothing -> Left $ "Missing value for field " <> fieldName
--      Just (Just val) -> pure $ M1 $ K1 x

instance (GFromQuery x, GFromQuery y) => GFromQuery (x :*: y) where
  gparseParams params = (:*:) <$> gparseParams params <*> gparseParams params


queryString :: [(Text, Maybe Text)]
queryString = [("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "12")]

data TestQueryStringRequest = TestQueryStringRequest
  { pluh     :: Int
  , mah      :: Text
  -- , tags_ids :: Vector Int
  } deriving (Eq, Show, FromQuery, Generic)

{-
-- Right (TestQueryStringRequest {pluh = 12, mah = "12", tags_ids = [1,2,3]})
parseParams @TestQueryStringRequest queryString
-}
