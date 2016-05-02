
-- The type class definition of functions that must be supported to be
-- of class "Renderable"
-- This applies to both a Table or an individual Cell of a table
class Show a => Renderable a where
  latex     :: a -> String
  rmarkdown :: a -> String
  html5     :: a -> String
  text      :: a -> String  
  label     :: a -> String -- This is for indexing
  show      = text         -- In Haskell this generates "summary" from text func
  
-- A Table is either a Renderable Cell
-- Or it's a grid of rows/columns that contain Tables
--   Note: In the type declaration, one can constrain this to a rectangle
--         but this invokes higher order array types. This declaration
--         assumes that it's taken care properly, which is not guaranteed 
--         by type.
data Table a = Grid {cells :: [[Table a]],  embedded :: Boolean} | Cell a

-- Returns outer rows, cols of a table
size :: Table a -> (Int, Int)

summary :: Formula -> Data -> Table a


