{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DeQueryStringSpec where 

import Test.Hspec (Spec, describe, it, pending, shouldBe)

import Data.Text
import Data.Time.Calendar (Day)
import Data.Vector (Vector, fromList)
import GHC.Generics

import DeQueryString


data TestQueryStringRequest = TestQueryStringRequest
  { pluh     :: Int
  , mah      :: Text
  , tags_ids :: Vector Int
  } deriving (Eq, Show, FromQuery, Generic)

testQueryString :: [(Text, Maybe Text)]
testQueryString = [("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "12")]


--data MaybeQS = MaybeQS {
--    maybeInt :: Maybe Int
--    --maybeVectorInt :: Maybe (Vector Int)
--} deriving (Eq, Show, FromQuery, Generic)

maybeQueryString :: [(Text, Maybe Text)]
maybeQueryString = [("haveChildren", Nothing), ("haveChildren", Just "12"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "12")]


spec :: Spec
spec = do
    describe "DeQueryString" $ do
        it "works for Int, Text, Vector Int fields"
            $ shouldBe
                (parseParams testQueryString :: Either String TestQueryStringRequest)

                $ Right TestQueryStringRequest {pluh = 12, mah = "12", tags_ids = fromList [1,2,3]}

        it "works for Maybe a fields"
            pending

        it "works for Vector Day fields, Vector [Int]"
            pending
        --    $ shouldBe
        --        ((parseParams createUserQueryString) :: Either String CreateUser)

        --        $ Right CreateUserSR {pluh = 12, mah = "12", tags_ids = fromList [1,2,3]}

        --it "works for List"
