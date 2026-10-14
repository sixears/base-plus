{-| base tools for working with Word64 -}

module BasePlus.Word64
  ( IsWord64(..) )
where

-- base --------------------------------

import Data.Word  ( Word64 )

-- lens --------------------------------

import Control.Lens.Iso  ( Iso', iso )

--------------------------------------------------------------------------------

{-| I is a Word64 -}
class    IsWord64 α  where {-| convert to/from Word64 -} word64 ∷ Iso' α Word64
instance IsWord64 Word64  where  word64 = iso id id

-- that's all, folks! ----------------------------------------------------------
