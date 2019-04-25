{-# LANGUAGE TemplateHaskell #-}

module Test.Generation where

import Language.Haskell.TH

typeclassNames :: [String]
typeclassNames = ["Semigroup", "Monoid"]


monoidInstances :: String -> Q [InstanceDec]
monoidInstances typeName =
  do
    optName <- lookupTypeName typeName
    case optName of
      Nothing   -> pure []
      Just name -> reifyInstances ''Monoid [ConT name]
