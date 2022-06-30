module DeQueryStringSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Text
import Data.Time.Calendar (Day)
import Data.Vector (Vector)

import DeQueryString

data CreateUser = CreateUser {
    name :: Text,
    haveChildren :: Maybe Int,
    childrenBirthdays :: Maybe (Vector Day)
}

spec :: Spec
spec = do
    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
