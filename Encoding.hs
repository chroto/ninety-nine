module Encoding ( Encoded(Multiple, Single) ) where

data Encoded a = Multiple Int a 
    | Single a
    deriving (Eq, Show)
