{-# LANGUAGE TypeFamilies
           , PolyKinds
  #-}

module Singleton where

data family Singleton ( t :: k )
