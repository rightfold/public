module Granite.Position
  ( Position (..)
  ) where

-- |
-- A position in the source code of a file.
data Position =
  Position
    { positionLine   :: {-# UNPACK #-} Int
    , positionColumn :: {-# UNPACK #-} Int
    , positionFile   :: FilePath }
  deriving stock (Eq, Show)
