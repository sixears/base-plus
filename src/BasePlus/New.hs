{-| generic construction of objects from tuples or records -}
module BasePlus.New
  ( New( new ) )
where

--------------------------------------------------------------------------------

{-| objects that may be constructed from a tuple, record, or simple data type -}
class New α β where new ∷ β → α

-- that's all, folks! ----------------------------------------------------------
