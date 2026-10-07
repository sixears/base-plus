{-| additions to the base package -}
module BasePlus
  ( whenJust )
where

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝓙, pattern 𝓝 )

--------------------------------------------------------------------------------

{-| akin to `Control.Monad.when`; act on a value only if it is a
    `Data.Maybe.Just` -}
whenJust ∷ ∀ α η . Monad η => (α → η ()) → 𝕄 α → η ()
whenJust _  𝓝  = return ()
whenJust io (𝓙 y) = io y

-- that's all, folks! ----------------------------------------------------------
