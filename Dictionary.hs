{-# Language DeriveDataTypeable #-}
module Dictionary where

import Data.Data
data Cell = Alive | Dead deriving (Eq, Show, Data, Typeable)

toChar enum = case enum of
   Alive -> '*'
   Dead -> ' '

toString enum = case enum of
    Alive -> "*"
    Dead -> " "