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


data MaybeQS = MaybeQS {
    maybeInt :: Maybe Int
    --maybeVectorInt :: Maybe (Vector Int)
} deriving (Eq, Show, FromQuery, Generic)

maybeQS :: [(Text, Maybe Text)]
maybeQS = [("maybeInt", Just "1")]

maybeQSWithoutValue :: [(Text, Maybe Text)]
maybeQSWithoutValue = [("maybeInt", Nothing)]

maybeQSWithoutParam :: [(Text, Maybe Text)]
maybeQSWithoutParam = []


spec :: Spec
spec = do
    describe "DeQueryString" $ do
        it "works for Int, Text, Vector Int parameters"
            $ shouldBe
                (parseParams testQueryString :: Either String TestQueryStringRequest)

                $ Right TestQueryStringRequest {pluh = 12, mah = "12", tags_ids = fromList [1,2,3]}

        it "works for Maybe"
            $ shouldBe
                (parseParams maybeQS :: Either String MaybeQS)

                $ Right MaybeQS {maybeInt = Just 1}

        it "works for Maybe parameter without value"
            $ shouldBe
                (parseParams maybeQSWithoutValue :: Either String MaybeQS)

                $ Right MaybeQS {maybeInt = Nothing}

        --it "works for Maybe parameter that not in the query string"
        --    $ shouldBe
        --        (parseParams maybeQSWithoutParam :: Either String MaybeQS)

        --        $ Left "Pluh"
        --        -- $ Right MaybeQS {maybeInt = 12}

        --it "works for Vector Day, Int32 parameters"
        --    pending
        --    $ shouldBe
        --        ((parseParams createUserQueryString) :: Either String CreateUser)

        --        $ Right CreateUserSR {pluh = 12, mah = "12", tags_ids = fromList [1,2,3]}

        --it "works for List"
