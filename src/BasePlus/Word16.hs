{-| base tools for working with Word16 -}

module BasePlus.Word16
  ( IsWord16(..) )
where

-- base --------------------------------

import Data.Word  ( Word16 )

-- lens --------------------------------

import Control.Lens.Iso  ( Iso', iso )

--------------------------------------------------------------------------------

{-| I is a Word16 -}
class    IsWord16 α  where {-| convert to/from Word16 -} word16 ∷ Iso' α Word16
instance IsWord16 Word16  where  word16 = iso id id

-- that's all, folks! ----------------------------------------------------------
