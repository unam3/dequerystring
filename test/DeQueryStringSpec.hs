{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DeQueryStringSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Text
import Data.Time.Calendar (Day)
import Data.Vector (Vector, fromList)
import GHC.Generics

import DeQueryString

testQueryString :: [(Text, Maybe Text)]
testQueryString = [("author_id", Just "12"), ("author_id", Nothing), ("abyr", Nothing), ("ad", Nothing), ("abyr", Just "Valg3"), ("meh", Nothing), ("tags_ids", Just "[1,2,3]"), ("pluh", Just "12"), ("mah", Just "12")]

data TestQueryStringRequest = TestQueryStringRequest
  { pluh     :: Int
  , mah      :: Text
  , tags_ids :: Vector Int
  } deriving (Eq, Show, FromQuery, Generic)

--data CreateUser = CreateUserConstructor {
--    name :: Text,
--    haveChildren :: Maybe Int,
--    childrenBirthdays :: Maybe (Vector Day)
--} deriving (Eq, Show, FromQuery, Generic)


spec :: Spec
spec = do
    describe "DeQueryString" $ do
        it "works"
            $ shouldBe
                -- plain wrong TypeApplication use? (type must be Either String TestQueryStringRequest)
                --parseParams @TestQueryStringRequest queryString

                ((parseParams testQueryString) :: Either String TestQueryStringRequest)

                $ Right (TestQueryStringRequest {pluh = 12, mah = "12", tags_ids = fromList [1,2,3]})
