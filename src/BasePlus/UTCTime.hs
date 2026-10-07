{-| base data structures & functions for handling UTCTime -}
module BasePlus.UTCTime
  ( HasUTCTime( utcTime ), HasUTCTimeMay( utcTimeMay ) )
where

import Base0

-- more-unicode ------------------------

import Data.MoreUnicode.Maybe  ( 𝕄 )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime )

--------------------------------------------------------------------------------

{-| I has a `UTCTime` field -}
class HasUTCTime α where
  utcTime ∷ Lens' α UTCTime

instance HasUTCTime UTCTime where
  utcTime = id

------------------------------------------------------------

{-| I maybe has a `UTCTime` field -}
class HasUTCTimeMay α where
  utcTimeMay ∷ Lens' α (𝕄 UTCTime)

instance HasUTCTimeMay (𝕄 UTCTime) where
  utcTimeMay = id

-- that's all, folks! ----------------------------------------------------------
