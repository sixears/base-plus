{-| base additions for Async -}
module BasePlus.Async
  ( HasAsync(..), ThreadIsRunning(..), threadIsRunning )
where

import Base0

-- async -------------------------------

import Control.Concurrent.Async  ( Async, poll, wait )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )
import Data.MoreUnicode.Maybe  ( pattern 𝓙, pattern 𝓝 )
import Data.MoreUnicode.Monad  ( (≫) )

--------------------------------------------------------------------------------

{-| I has an `Async β` -}
class HasAsync α β where
  async_      ∷ Lens' α (Async β)
  waitAsync   ∷ MonadIO μ => α → μ β
  waitAsync a = liftIO $ wait (a ⊣ async_)

----------

instance HasAsync (Async β) β where async_ = lens id (const id)

------------------------------------------------------------

{-| is a thread still running? -}
data ThreadIsRunning = ThreadIsRunning | ThreadIsNotRunning deriving (Eq, Show)

------------------------------------------------------------

-- I don't know why this the output has to be a `()`; but I can't make it work
-- any other way.
{-| check whether a the thread attached to a given `HasAsync` thing is still a
    going concern -}
threadIsRunning ∷ ∀ α μ . (MonadIO μ, HasAsync α ()) => α -> μ ThreadIsRunning
threadIsRunning x = liftIO $
    let a ∷ Async () = x ⊣ async_
    in  poll a ≫ \ case
      𝓝   → return ThreadIsRunning
      𝓙 _ → return ThreadIsNotRunning

-- that's all, folks! ----------------------------------------------------------
