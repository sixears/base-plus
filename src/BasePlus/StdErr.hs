{-| simple easy writing of lines to stderr (a la Perl's warn) -}
module BasePlus.StdErr
  ( eToStderr, eToStderrIO, stdErr, stdErrS, stdErrT )
where

import Base0

-- base --------------------------------

import System.IO  ( stderr )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( 𝔼, pattern 𝓛, pattern 𝓡 )
import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄, pattern 𝓙, pattern 𝓝 )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.String   ( 𝕊 )
import Data.MoreUnicode.Text     ( 𝕋 )

-- text --------------------------------

import Data.Text.IO  ( hPutStrLn )

--------------------------------------------------------------------------------

{-| write some text to stderr (in case something fails within the logging) -}
stdErr ∷ (MonadIO μ, Printable τ) => τ → μ ()
stdErr = liftIO ∘ hPutStrLn stderr ∘ toText

----------

{-| `stdErr`, input type reified to `𝕊` -}
stdErrS ∷ MonadIO μ => 𝕊 → μ ()
stdErrS = stdErr

----------

{-| `stdErr`, input type reified to `𝕋` -}
stdErrT ∷ MonadIO μ => 𝕋 → μ ()
stdErrT = stdErr

--------------------

{-| given an Either, dump a `Left` to stderr (return `Data.Maybe.None`); else
    return `Right` as a `Just` -}
eToStderr ∷ ∀ ε α μ . (MonadIO μ, Printable ε) => 𝔼 ε α → μ (𝕄 α)
eToStderr (𝓛 e) = stdErrT (toText e) ⪼ return 𝓝
eToStderr (𝓡 r) = return (𝓙 r)

----------

{-| `eToStderr`, reified to `IO()` -}
eToStderrIO ∷ Printable ε => 𝔼 ε α → IO ()
eToStderrIO = const () ⩺ eToStderr

-- that's all, folks! ----------------------------------------------------------
