{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module DeQueryString where

import GHC.Classes


-- https://downloads.haskell.org/~ghc/latest/docs/users_guide/exts/scoped_type_variables.html#extension-ScopedTypeVariables

-- a will be the same in f body as in type annotation
f :: forall a. [a] -> [a]
f xs = ys ++ ys
     where
       ys :: [a]
       ys = reverse xs

--An equivalent form for that example, avoiding explicit forall uses Pattern type signatures:

f1 :: [a] -> [a]
f1 (xs :: [aa]) = xs ++ ys
  where
    ys :: [aa]
    ys = reverse xs

instance IP "you" String where
  ip = "gd"
