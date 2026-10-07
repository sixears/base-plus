{-| a few more utilities for working with lists -}
module BasePlus.List
  ( firstJust, takeWhileM )
where

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝓙, pattern 𝓝 )
import Data.MoreUnicode.Monad    ( (≫) )

--------------------------------------------------------------------------------

{-| like `Data.List.takeWhile`, but monadic -}
takeWhileM ∷ Monad η => (α → η 𝔹) → [α] → η [α]
takeWhileM _ []    = return []
takeWhileM p (x:xs)= p x ≫ \ b→ if b then (x:) ⊳ takeWhileM p xs else return []

----------------------------------------

{-| The first non-𝓝 value in a list, if any -}
firstJust ∷ [𝕄 α] → 𝕄 α
firstJust []          = 𝓝
firstJust ((𝓙 x) : _) = 𝓙 x
firstJust (𝓝 : xs)    = firstJust xs

-- that's all, folks! ----------------------------------------------------------
