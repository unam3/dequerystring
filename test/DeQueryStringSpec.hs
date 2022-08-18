{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DeQueryStringSpec where 

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Data
import Data.Text
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
--import GHC.Generics (Generic)
import GHC.Generics

--import DeQueryString

data CreateUser = CreateUserConstructor {
    name :: Text,
    haveChildren :: Maybe Int,
    childrenBirthdays :: Maybe (Vector Day)
} deriving (Generic, Show, Data)

{-
:kind! (Rep CreateUser)
(Rep CreateUser) :: * -> *
= D1
    ('MetaData "CreateUser" "DeQueryStringSpec" "main" 'False)
    (C1
       ('MetaCons "CreateUserConstructor" 'PrefixI 'True)
       (S1
          ('MetaSel
             ('Just "name")
             'NoSourceUnpackedness
             'NoSourceStrictness
             'DecidedLazy)
          (Rec0 Text)
        :*: (S1
               ('MetaSel
                  ('Just "haveChildren")
                  'NoSourceUnpackedness
                  'NoSourceStrictness
                  'DecidedLazy)
               (Rec0 (Maybe Int))
             :*: S1
                   ('MetaSel
                      ('Just "childrenBirthdays")
                      'NoSourceUnpackedness
                      'NoSourceStrictness
                      'DecidedLazy)
                   (Rec0 (Maybe (Vector Day))))))
-}

spec :: Spec
spec = do
    describe "f" $ do
        it "works"
            $ shouldBe
                42
                42
