{-# Language DeriveDataTypeable #-}
module Cell
( Cell(..)
, toChar
, toString
) where
import Data.Data

data Cell = Alive | Dead deriving (Eq, Show, Data, Typeable)

toChar :: Cell -> Char
toChar enum = case enum of
   Alive -> '*'
   Dead -> ' '

toString :: Cell -> String
toString enum = case enum of
    Alive -> "*"
    Dead -> " "