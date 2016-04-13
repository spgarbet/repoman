{-# OPTIONS_GHC -XDatatypeContexts #-}

--------------------------------------------------------------
-- Haskell formal type declaration of table relations
--
-- The type class definition of functions that must be supported to be
-- of class "Renderable"
-- This applies to both a Table or an individual Cell of a table
class Show a => Renderable a where
  latex     :: a -> String
  rmarkdown :: a -> String
  html5     :: a -> String
  label     :: a -> String -- This is for indexing
  show      :: a -> String -- Haskell version of R's summary

-- A stop is simply a string id
type Stop = String

-- Alignment is a collection of ordered stops
data Alignment = Alignment {stops :: [Stop]}

-- A cell is renderable content, and has a right and left stop
data Renderable a => Cell a = Cell { leftalign  :: Stop,
                                     content    :: a,
                                     rightalign :: Stop}

-- A Row is a collection Renderables with left and right stop
-- http://stackoverflow.com/questions/18934882/haskell-line-of-code-not-compiling-illegal-datatype-context
data Row a = Row {cells :: [(Cell a)]}

-- A Table is a collection of descending rows with Alignment information
data Table a = Table {rows :: [Row a], alignment ::  Alignment}

