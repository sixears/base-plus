{-| very much like `ExceptT`; but that errors are reported direct to stderr -}
module BasePlus.EMonad
  ( ꙗ, ꙝ, ꙝ', runEMonad, runEMonadX )
where

import Base0

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Either   ( 𝔼, pattern 𝓛, pattern 𝓡 )
import Data.MoreUnicode.Functor  ( (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Monad    ( (≫) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import BasePlus.StdErr  ( eToStderr )

--------------------------------------------------------------------------------

-- odd ordering of variables make definition of Functor, Applicative, Monad
-- instances easier (or maybe possible)
{-| an either/error monad, designed to exit with the first error -}
data EMonad ε μ α = MonadIO μ => EMonad { runEMonadE ∷ μ (𝔼 ε α) }

--------------------

instance Functor (EMonad ε μ) where
  fmap f (EMonad m) = EMonad $ fmap (fmap f) m

--------------------

instance MonadIO μ => Applicative (EMonad ε μ) where
  pure x = EMonad $ return (𝓡 x)
  (EMonad f) <*> (EMonad x) = EMonad $ do
    f' ← f
    x' ← x
    return $ f' <*> x'

--------------------

instance MonadIO μ => Monad (EMonad ε μ) where
  (EMonad io) >>= f = EMonad $ do
    result ← io
    case result of
      𝓛 e → return (𝓛 e)      -- halt further computation
      𝓡 b → runEMonadE (f b)

--------------------

{-| construct an `EMonad` from an `ExceptT`; e.g., a `MonadError` -}
eMonad ∷ ∀ ε α μ . MonadIO μ => ExceptT ε μ α → EMonad ε μ α
eMonad = EMonad ∘ ѥ

{-| unicode alias for `eMonad` (`EMonad` construction) -}
ꙗ ∷ ∀ ε α μ . MonadIO μ => ExceptT ε μ α → EMonad ε μ α
ꙗ = eMonad

--------------------

{-| run a sequence of potentially errorful computations; provide a function to
    dispose of the errors; so this can be used to (e.g.,) pass them to logging,
    or somesuch -}
runEMonadX ∷ ∀ ε α β η . Monad η => (𝔼 ε α → η β) → EMonad ε η α → η β
runEMonadX x m = runEMonadE m ≫ x

{-| run a sequence of potentially errorful computations; writing any failures to
    stderr, maybe returning a result -}
runEMonad ∷ ∀ ε α μ . (MonadIO μ, Printable ε) => EMonad ε μ α → μ (𝕄 α)
runEMonad = runEMonadX eToStderr -- runEMonadE m ≫ eToStderr

{-| shortcut for making and running an `EMonad`, with a unicode alias -}
ꙝ ∷ ∀ ε α μ . (MonadIO μ, Printable ε) => ExceptT ε μ α → μ (𝕄 α)
ꙝ = runEMonad ∘ eMonad

{-| like `ꙝ`, discarding the result -}
ꙝ' ∷ ∀ ε α μ . (MonadIO μ, Printable ε) => ExceptT ε μ α → μ ()
ꙝ' = const () ⩺ ꙝ

-- that's all, folks! ----------------------------------------------------------
