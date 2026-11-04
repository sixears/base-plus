{-| parse bytes with (case-insensitive) multipliers, e.g., "34", "100mb" or
    "20KiB" with suitable handling for powers-of-two multipliers (signified with
    the use of an 'i' in the multiplier) -}

module BasePlus.BytesParse
  ( parseBytes, parserBytes )
where

import Base0

import Prelude  ( Integer, Integral, Num, (^) )

-- base --------------------------------

import Control.Monad.Fail  ( MonadFail, fail )
import Data.Char           ( toLower )
import Data.Functor        ( (<$) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⋪) )
import Data.MoreUnicode.Either       ( 𝔼, pattern 𝓛, pattern 𝓡 )
import Data.MoreUnicode.Monoid       ( ф )
import Data.MoreUnicode.String       ( 𝕊 )

-- natural -----------------------------

import Natural ( (⨻) )
import Natural.BoundedError  ( BoundedError )
import Natural.Unsigned ( fromI )

-- trifecta ----------------------------

import  Text.Trifecta ( Parser, Result( Failure, Success ), char, decimal, eof
                      , oneOf, optional, parseString )

--------------------------------------------------------------------------------

{-| check for bounds errors, converting an 𝔼 (typically from a MonadError) to a
    `MonadFail` (typically for `Parser` -}
checkBE ∷ ∀ α μ . (Show α, MonadFail μ) => 𝔼 (BoundedError α) α → μ α
checkBE (𝓛 e) = fail $ toString e
checkBE (𝓡 a) = return a

----------------------------------------

{-| Parse the [kbtg][i][b] at the end.  With an 'i' means powers of two; without
    an 'i' means powers of ten.  This handles all the exponents available to
    a Word64; which goes up to exbibytes. -}
parseMult ∷ Parser Word64
parseMult = let (⨇) ∷ (Num α, Integral α) => α → α → α
                (⨇) = (^)
                exponent c =case toLower c of {'k'→1;'m'→2;'g'→3;'t'→4;'p'→5;_→6}
                powerOfTen c = 1_000⨇(exponent c)
                powerOfTwo c = 1_024⨇(exponent c)
            in  (do s ← oneOf "kKmMgGtTpPeE"
                    let dec = powerOfTen s
                        bin = powerOfTwo s
                        -- "i" (optionally + "b") → binary;
                    (char 'i' *> optional (oneOf "bB") *> pure bin)
                        -- bare letter (optionally + "b") → decimal
                     ∤ (optional (oneOf "bB") *> pure dec))
                    ∤ (1 <$ oneOf "bB")   -- bare "b"/"B" suffix = bytes
                    ∤ pure 1              -- no suffix at all = bytes

----------------------------------------

{-| Parse bytes into a Word64, as a `Parser`.  The bytes may be represented as
    a simple non-negative integer, optionally followed by a 'b'; or with an
    appended multiplier (no spaces), being 'k'/'m'/'g'/'t'/'p'/'e' (for kilo/kibi
    through to exo/exbi): that multiplier may be followed by an 'i' to ensure
    power-of-two multipliers (kibibytes, etc.) or not to ensure power-of-ten
    multipliers (kilobytes, etc.) and again optionally followed by a 'b'.  All
    parsing of the multiplier is case-insenstive.
 -}
parserBytes ∷ Parser Word64
parserBytes = do
  -- the `decimal` function is poorly-named; it parses a non-negative denary
  -- integer into an `Integer`
  n ∷ Integer ← decimal -- the Integer is important, it never overflows...
  ñ ← checkBE $ fromI n -- ... but check that it fits into a W64...
  mult ← parseMult
  checkBE (ñ ⨻ mult)    -- ... and likewise that the full value fits, too

----------------------------------------

{-| parse a bytes count like "34", "10Mb", "2GiB", "512", "1T", "300b" - see
    `parserBytes` for detailed rules -}
parseBytes ∷ 𝕊 → 𝔼 𝕊 Word64
parseBytes input = case parseString (parserBytes ⋪ eof) ф input of
                     Success n  →  𝓡 n
                     Failure err → 𝓛 (show err)

-- that's all, folks! ----------------------------------------------------------
