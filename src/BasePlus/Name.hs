{-| things that have names -}
module BasePlus.Name
  ( HasName( name, nameS, nameT ), Name )
where

import Base0

-- base --------------------------------

import Data.String  ( IsString )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens    ( (⊢) )
import Data.MoreUnicode.String  ( 𝕊 )
import Data.MoreUnicode.Text    ( 𝕋 )

-- text --------------------------------

import qualified  Data.Text  as  T

------------------------------------------------------------
--                     local imports                       -
------------------------------------------------------------

import BasePlus.New  ( New( new ) )

--------------------------------------------------------------------------------

{-| A `Name` is just a well-typed `𝕋` -}
newtype Name = Name { unName ∷ 𝕋 }  deriving  (IsString,Show)

----------

instance New Name 𝕋  where  new = Name

------------------------------------------------------------

{-| I has a name -}
class HasName α where
  name  ∷ Lens' α Name
  nameT ∷ Lens' α 𝕋
  nameT = lens (unName ∘ view name) (\ a s → a & name ⊢ Name s)
  nameS ∷ Lens' α 𝕊
  nameS = lens (T.unpack ∘ unName ∘ view name)
               (\ a s → a & name ⊢ Name (T.pack s))

----------

instance HasName Name  where  name = lens id (const id)

-- that's all, folks! ----------------------------------------------------------
