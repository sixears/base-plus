{-| functions for working with a filemode (A.K.A., `CMode`), that is, the Word16
    passed around UFS & derivatives for the ugo±rw[xsStT],

     POSIX refers to the stat.st_mode bits corresponding to the mask S_IFMT (see below) as the file type, the 12 bits corresponding to the mask 07777 as the file mode bits and the least significant 9 bits (0777) as the file permission bits.

-}

-- !!! XXX !!!
-- update fstat to use this

module BasePlus.FilePerms
  ( parseFilePerms, filePermsParser )
where

import Base0
import Debug.Trace  ( traceShow )
import Prelude  ( error )

-- base --------------------------------

import Control.Applicative  ( (<|>) )
import Control.Monad        ( fail )
import Data.Bits            ( setBit,clearBit,complement,(.&.),(.|.),(.>>.) )
import Data.Char            ( Char, isOctDigit, digitToInt )
import Data.Enum            ( fromEnum )
import Data.List            ( foldl' )
import Data.Word            ( Word16 )
import GHC.Real             ( Integral )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( (×) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋫), (∤) )
import Data.MoreUnicode.Bool         ( pattern 𝓕, pattern 𝓣 )
import Data.MoreUnicode.Char         ( ℂ )
import Data.MoreUnicode.Either       ( 𝔼, pattern 𝓛, pattern 𝓡 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ф, ю )
import Data.MoreUnicode.String       ( 𝕊 )
import Data.MoreUnicode.Text         ( 𝕋 )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertFailure,testCase,(@?=) )

-- text --------------------------------

import qualified  Data.Text  as  T

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- trifecta ----------------------------

import Text.Trifecta  ( Parser, Result( Failure, Success )
                      , char, choice, eof, oneOf, option, parseString, satisfy
                      , sepBy1, spaces, unexpected
                      )

--------------------------------------------------------------------------------

-- testing helpers -----------------------------------------

{-| check the value of a successful parse -}
(~~) ∷ (Eq α, Show α, Integral α) => Result α → α → Assertion
(Success x) ~~ y = assertBool ([fmt|0o%04o ≡ 0o%04o|] x y) (x ≡ y)
(Failure x) ~~ _ = assertFailure (show x)

----------------------------------------

{-| check the value of a successful parse -}
(~~~) ∷ (Eq α, Show α, Integral α) => (Parser α, 𝕊) → α → TestTree
(p,x) ~~~ exp = testCase (show x) $
  case parseString p ф x of
    Success r → assertBool ([fmt|0o%04o ≡ 0o%04o|] r exp) (r ≡ exp)
    Failure e → assertFailure (show e)
{-
(p, Success x) ~~~ y = testCase (show x) $
  let x' = parseString p ф x
  in  assertBool ([fmt|0o%04o ≡ 0o%04o|] x' y) (x' ≡ y)
(p, Failure x) ~~~ _ = testCase (show x) $ assertFailure (show x)
-}

----------------------------------------

{-| assert a parse failure -}
(~!~) ∷ Show α => Result α → 𝕊 → Assertion
(Failure x) ~!~ _ = return ()
(Success x) ~!~ y =
  assertFailure $ ю ["expected failure for ", show y, ", got ", show x]

----------------------------------------

(‼) ∷ (Parser α,𝕊) → 𝕊 → TestTree
(p,str) ‼ msg =
  let isFailure (Failure _) = 𝓣
      isFailure (Success _) = 𝓕
  in  testCase msg $ assertBool str (isFailure $ parseString p ф str)
--      isFailure (Failure _) = 𝓣
--      isFailure (Success _) = 𝓕

--       , testCase "leading '-' should fail" $ assertBool "g-r" (isFailure $ parseString chmodMode ф "g-r")


------------------------------------------------------------

-- | 0777 or 07777; the leading '0' is mandatory.
octalMode ∷ Parser Word16
octalMode = do
  _  ← char '0'
  ds ← some (satisfy isOctDigit)
  let val = foldl' (\acc d → acc×8 + fromIntegral (digitToInt d)) (0∷Word16) ds
  if val > 0o7777
  then unexpected "octal perms larger than 07777"
  else return val

--------------------

octalModeTests ∷ TestTree
octalModeTests =
  testGroup "octalMode" $
    [ testCase "plain octal 0777" $
        parseString octalMode ф "0777" ~~ 0o777
    , testCase "octal with special bits 07777" $
        parseString octalMode ф "07777" ~~ 0o7777
    , testCase "octal without leading zero is rejected" $
        parseString octalMode ф "777" ~!~ "octalMode:777"
    , testCase "octal above 07777 is rejected" $
        parseString octalMode ф "010000" ~!~ "octalMode:010000"
    ]

--------------------

{- | E.g., "rwsrwsrwt", "rwsr-Sr-t", "rwxrwxrwx"; as might be found in the
     output of 'ls' but there is no parsing of a leading file-type char.

     In each triplet the third slot may be:
       'x' → execute only
       '-' → neither
       's' (user/group slots) or 't' (other slot) → execute + special bit
       'S' (user/group slots) or 'T' (other slot) → special bit, no execute
-}

lsMode ∷ Parser Word16
lsMode = do
  let rw c v = (char c ⋫ pure v) ∤ (char '-' ⋫ pure 0)

      userTriplet = do
        r ← rw 'r' 0o400
        w ← rw 'w' 0o200
        x ← choice [ char 'x' ⋫ pure 0o100
                   , char 's' ⋫ pure (0o100 .|. 0o4000)
                   , char 'S' ⋫ pure 0o4000
                   , char '-' ⋫ pure 0
                   ]
        return (r .|. w .|. x)

      groupTriplet = do
        r ← rw 'r' 0o040
        w ← rw 'w' 0o020
        x ← choice [ char 'x' ⋫ pure 0o010
                   , char 's' ⋫ pure (0o010 .|. 0o2000)
                   , char 'S' ⋫ pure 0o2000
                   , char '-' ⋫ pure 0
                   ]
        return (r .|. w .|. x)

      otherTriplet = do
        r ← rw 'r' 0o004
        w ← rw 'w' 0o002
        x ← choice [ char 'x' ⋫ pure 0o001
                   , char 't' ⋫ pure (0o001 .|. 0o1000)
                   , char 'T' ⋫ pure 0o1000
                   , char '-' ⋫ pure 0
                   ]
        return (r .|. w .|. x)

  u ← userTriplet
  g ← groupTriplet
  o ← otherTriplet
  return (u .|. g .|. o)

--------------------

lsModeTests ∷ TestTree
lsModeTests =
  testGroup "lsMode"
    [ testCase "plain permissions rwxr-xr--" $
        parseString lsMode ф "rwxr-xr--" ~~ 0o754
    , testCase "special bits --------T" $
        parseString lsMode ф "--------T" ~~ 0o1000
    , testCase "special bits --------t" $
        parseString lsMode ф "--------t" ~~ 0o1001
    , testCase "special bits --S------" $
        parseString lsMode ф "--S------" ~~ 0o4000
    , testCase "special bits --s------" $
        parseString lsMode ф "--s------" ~~ 0o4100
    , testCase "special bits -----S---" $
        parseString lsMode ф "-----S---" ~~ 0o2000
    , testCase "special bits -----s---" $
        parseString lsMode ф "-----s---" ~~ 0o2010
    , testCase "normal bits rwxr--r-x" $
        parseString lsMode ф "rwxr--r-x" ~~ 0o0745
    , testCase "special bits rwsr-Sr-t" $
        parseString lsMode ф "rwsr-Sr-t" ~~ (0o0745.|.0o4100.|.0o2000.|.0o1001)
    ]


----------------------------------------

{-| Parse a mode line as comma-separated sets of permissions on
    user/group/other, read from left-to-right, each of the form

      WhoOpBits

    Who may be

      u - user
      g - group
      o - others

      ug,gu,go,og,uo,ou - combine any two other "who"s.  "a" may not be combined
                          in this way

      a - each of u,g,o

      An absent Who preceding a valid op will be taken to mean "a"

    Op may be

      + - Add the bits to this user.
      / - Add the complement of the bits to the user.  The special bits [sStT]
          will not be complemented in or out.
      = - Set exactly these bits to the user.
      - - Remove the cited bits from the user.  Only valid for overriding, hence
          never as the first clause

    Bits may be:
      r - read bit
      w - write bit
      x - execute bit
    The bits below do will not be parsed in conjunction with the "/" Op.
      s - setuid bit, implies execute bit (applicable to u,g only)
      S - setuid bit, without execute bit (applicable to u,g only)
      t - sticky bit, implies execute bit (applicable to o only)
      T - sticky bit, without execute bit (applicable to o only)

    An explicit attempt to add, say, a setuid bit to an "o" Who will fail to
    parse.  However, something like (e.g.) "a+s" will result in the setuid bit &
    execute bits being set on the "u","g" who with /neither/ being set on "o".

    The list of clauses is read from l-r, with later clauses overriding earlier
    clauses.

    EXAMPLES:

    "u+rw,g/w,o-x"  -  rw-r-xrw-
    "u+s,+t,o=a"    -  --s---rwt
    "ug+wx"         -  -wx-wx---
    "a+rx"          -  r-xr-xr-x
    "a=rx"          -  r-xr-xr-x
    "a/w"           -  r-xr-xr-x
    "+rx"           -  r-xr-xr-x
    "=rx"           -  r-xr-xr-x
    "/w"            -  r-xr-xr-x
    "ga+rx" -- FAIL -  cannot mix "a" with other who
    "g+rxa" -- FAIL -  cannot mix "a" with other bit
    "u-w"   -- FAIL -  cannot use an "-" as the first clause

-}

-- | e.g. u+rw,g-w,o=a,u+s,+t,u=rwS ; empty who-list defaults to 'a' (all of ugo).
--
-- Permission letters:
--   's' → x on affected classes, plus setuid (u) / setgid (g)
--   'S' → setuid (u) / setgid (g) WITHOUT x   (non-standard extension)
--   't' → x on affected classes, plus sticky bit
--   'T' → sticky bit WITHOUT x                (non-standard extension)
chmodMode ∷ Parser Word16
chmodMode = do
  clauses ← chmodClause `sepBy1` char ','
  result  ← foldM applyClause (0,0) clauses
  traceShow ("clauses", (\ (who,op,perms) → (unWho who,op,[fmtT|0o%04o|] perms)) ⊳ clauses) $ return (fst ({- foldl' -} result))
  where

    unWho 0o4700 = "u"
    unWho 0o6770 = "ug"
    unWho 0o2070 = "g"
    unWho 0o3077 = "go"
    unWho 0o1007 = "o"
    unWho 0o5707 = "uo"
    unWho 0o7777 = "a"
    unWho x      = error $ [fmt|unknown unWho: 0o%04o|] x

    -- perms at this point is user-generic; it's always 0oXXX
    applyClause ∷ (Word16,Word16) → (Word16,ℂ,Word16) → Parser (Word16,Word16)
    applyClause (m,users_so_far) (who, op, perms) =
      let pp     = perms .&. who -- the perms; applied to specific user(s)
          result ∷ (𝔼 𝕊 Word16)
          result = case op of
                     '+' → 𝓡 $ m .|. pp
                     '/' → 𝓡 $ m .|. (complement pp .&. who .&. 0o0777) -- 0o0777 to mask out the special bits, which do not partake
                     '=' → 𝓡 $ m .|. (who .&. pp)
                     '-' → if users_so_far .&. who ≡ 0
                       then 𝓛 $ [fmt|attempt to use '-' clause on user %w without prior setting clause|] (unWho who)
                       else 𝓡 $ m .&. (complement pp) -- m .|. -- (who - pp)
                     _   → 𝓛 $ [fmt|chmodMode applyClause op %w|] op
      in traceShow ("applyClause", [fmtT|0o%04o|] m, unWho who, [fmtT|0o%04o|] perms, [fmtT|0o%04o|] pp, [fmtT|0o%04o|] ⊳ result) $ either fail (\ r → return (r,users_so_far .|. who)) result

-- XXX '-' shouldn't apply to s/t, I think, except for overriding prior (l-r)
-- requests?

    whoParser = do
      ws ← some (oneOf "ugoa")
      return (foldr ((.|.) . whoBit) 0 ws)
    whoBit 'u' = 0o4700
    whoBit 'g' = 0o2070
    whoBit 'o' = 0o1007
    whoBit 'a' = 0o7777   -- 'a'
    whoBit c   = error $ [fmt|chmodMode whoBit op %w|] c

    opParser = oneOf "+-=/"

    -- Expand s/S/t/T into concrete bits per the who-list.
    permsParser ∷ Word16 → Parser Word16
    permsParser who = do
      ps ← many (oneOf "rwxstSTa")
      let base     = foldr ((.|.) . permBit)     0 ps
          specials = foldr ((.|.) . specialBit who) 0 ps
      traceShow ("permsParser", "ps", ps, "base", [fmtT|0o%04o|] base, "who", [fmtT|0o%04o|] who, "specials", [fmtT|0o%04o|] specials, [fmtT|0o%04o|] ⊳ (specialBit who ⊳ ps)) $ return (base .|. specials)

    permBit ∷ Char → Word16
    permBit 'r' = 0o444
    permBit 'w' = 0o222
    permBit 'x' = 0o111
    permBit 'a' = 0o777
    permBit 's' = 0o110          -- implies x on the affected classes
    permBit 't' = 0o001          -- ditto, for the sticky variant
    permBit 'S' = 0              -- 'S' contributes no permission bits
    permBit 'T' = 0              -- 'T' contributes no permission bits
    permBit c   = error $ [fmt|chmodMode permBit op %w|] c

    specialBit ∷ Word16 → Char → Word16
    specialBit who 's' = 0o6000 .&. who   -- setuid for u, setgid for g
    specialBit who 'S' = 0o6000 .&. who   -- same, but no x implied
    specialBit who 't' = 0o1000 .&. who   -- sticky (o or 'a')
    specialBit who 'T' = 0o1000 .&. who   -- same, but no x implied
    specialBit _   _   = 0

    chmodClause ∷ Parser (Word16,ℂ,Word16)
    chmodClause = do
      who ← option 0o7777 whoParser   -- empty lhs == 'a'
      op ← opParser
      perms ← traceShow("chmodClause", [fmtT|0o%04o|] who) $ permsParser who
      return (who, op, perms)

chmodModeTests ∷ TestTree
chmodModeTests =
  let x ≈ y = (chmodMode,x) ~~~ y
      isFailure (Failure _) = 𝓣
      isFailure (Success _) = 𝓕

  in  testGroup "chmodMode"
        [ "u+r"   ≈ 0o400
        , "u+x"   ≈ 0o100
        , "u=w"   ≈ 0o200
        , "u/x"   ≈ 0o600
        , "u+rw"  ≈ 0o600


{-
    "u+rw,g/w,o-x"  -  rw-r-xrw-
    "u+s,+t,o=a"    -  --s---rwt
    "ug+wx"         -  -wx-wx---
    "a+rx"          -  r-xr-xr-x
    "a=rx"          -  r-xr-xr-x
    "a/w"           -  r-xr-xr-x
    "+rx"           -  r-xr-xr-x
    "=rx"           -  r-xr-xr-x
    "/w"            -  r-xr-xr-x
    "ga+rx" -- FAIL -  cannot mix "a" with other who
    "g+rxa" -- FAIL -  cannot mix "a" with other bit

-}

        -- testCase "u+rw" $
--          (chmodMode,"u+rw") ~~~ 0o600
      , -- testCase "g+rw" $
          "g+rx" ≈ 0o050
      ,   "u+rw,g+rw" ≈ 0o660
      ,   "g/r" ≈ 0o030
      ,   "o=a" ≈ 0o007
      , (chmodMode, "g-r") ‼ "leading '-' should fail"
      , (chmodMode, "u+x,g-r") ‼ "leading '-' should fail (absent g+)"
      , "g+x,g-r" ≈ 0o010
      , "g+a,g-r" ≈ 0o030
      , "a+x,g-r" ≈ 0o111
      , "a+a,g-r" ≈ 0o737

      , "a+x,g-r" ≈ 0o111
      , (chmodMode, "o=a,g-w") ‼ "'-' g should fail after (only) o clause"
      , "go=a,g-w" ≈ 0o057
      , "a=a,g-w" ≈ 0o757

      , "a+rw,g-w" ≈ 0o646
      , (chmodMode,"u+rw,g-w") ‼ "'-' g should fail due to only prior (u) clause"
      , (chmodMode,"u+rw,g-w,o=a") ‼ "'-' g should fail due to only prior (u) clause"
      , (chmodMode,"u+rw,o=a,g-w") ‼ "'-' g should fail due to only prior (u+o) clauses"
      , "u+rw,a=a,g-w" ≈ 0o757
      , "a+rw,o-w" ≈ 0o664
      , "u+x" ≈ 0o0100
      , "u+s" ≈ 0o4100
      , "u+S" ≈ 0o4000
-- test for ordering, with a=g,r-x reading l→r so r-x "overrides"
      , "+t" ≈ 0o1001
      , "u+s,+t" ≈ 0o5101
      , "u=rwS,o=rT" ≈ 0o5604
      , "u=rwS,g=s,o=rT" ≈ 0o7614
      , "u=rwS,g=s,o=rT,a-r" ≈ 0o7210
      ]

----------------------------------------

{-| render the brwxr-xr-- bit of an 'ls'-style output line -}
permLSTxt ∷ Word16 → 𝕋
permLSTxt p =
  let -- fstat user perms
      permsUser  ∷ Word8
      permsUser  = fromIntegral $ p .&. 0o700 .>>. 6
      -- fstat group perms
      permsGroup ∷ Word8
      permsGroup = fromIntegral $ p .&. 0o070 .>>. 3
      -- fstat other perms
      permsOther ∷ Word8
      permsOther = fromIntegral $ p .&. 0o007

      -- '-' for no perm; c for regular perm (r/w/x); upper case s for irregular
      -- perm without c (setUserID/setGroupID: S || sticky bit: T); lower case
      -- s for irregular perm with c
      t'' c i s i' n = [fmt|%t|] (case (i .&. n ≡ 0, i' .&. p ≡ 0) of
                                    (𝓣,𝓣) → "-"
                                    (𝓣,𝓕) → T.toUpper s
                                    (𝓕,𝓣) → c
                                    (𝓕,𝓕) → s)
      -- Simple mask to character, c is character, i is bitmask to match
      -- like t'' but using ⵥ for the irregular char; which should never be
      -- used, since the mask for the irregular char is 0
      t' c i n = t'' c i "ⵥ" 0 n
      t s x n = -- Word16 (0b000-0b111) → rwx with s+t for uid/sticky bits
        ю [ t' "r" 4 n, t' "w" 2 n, t'' "x" 1 s x n ]
  in  [fmt|%t%t%t|] (t "s" 0o4000 $ permsUser) (t "s" 0o2000 $ permsGroup)
                    (t "t" 0o1000 $ permsOther)

----------------------------------------

filePermsParser ∷ Parser (Word16, Bool)
filePermsParser = spaces ⋫ choice
  [ (\m → (m, 𝓕)) <$> octalMode
  , (\m → (m, 𝓕)) <$> lsMode
  , (\m → (m, 𝓣))  <$> chmodMode
  ] <* spaces <* eof

parseFilePerms ∷ String → Result (Word16, Bool)
parseFilePerms = parseString filePermsParser ф

-- tests ---------------------------------------------------
{-

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Text.Trifecta (Result (..))

import CPerms (parseFilePerms)

-- | Extract the value from a successful parse, failing the test otherwise.
ok ∷ Result (Word16, Bool) → (Word16, Bool)
ok (Success r) = r
ok (Failure d) = error ("unexpected parse failure: " ++ show d)

-- | Assert that the input fails to parse.
rejects ∷ String → Result (Word16, Bool) → Assertion
rejects what (Failure _) = return ()
rejects what (Success r) =
  assertFailure ("expected failure for " ++ show what ++ ", got " ++ show r)

main ∷ IO ()
main = defaultMain tests

tests ∷ TestTree
tests = testGroup "cperms parser"
  [ testGroup "octal"
      [ testCase "plain octal 0777" $
          ok (parseFilePerms "0777") @?= (0o777, 𝓕)
      , testCase "octal with special bits 07777" $
          ok (parseFilePerms "07777") @?= (0o7777, 𝓕)
      , testCase "octal without leading zero is rejected" $
          rejects "777" (parseFilePerms "777")
      , testCase "octal above 07777 is rejected" $
          rejects "010000" (parseFilePerms "010000")
      ]
  , testGroup "ls-style"
      [ testCase "plain permissions rwxr-xr--" $
          ok (parseFilePerms "rwxr-xr--") @?= (0o754, 𝓕)
      , testCase "special bits rwsr-Sr-t" $
          ok (parseFilePerms "rwsr-Sr-t") @?= (0o7774, 𝓕)
      ]
  , testGroup "chmod-style"
      [ testCase "comma-separated clauses u+rw,g-w,o=a" $
          ok (parseFilePerms "u+rw,g-w,o=a") @?= (0o644, 𝓣)
      , testCase "s/t imply execute: u+s,+t" $
          ok (parseFilePerms "u+s,+t") @?= (0o4111 .|. 0o4000 .|. 0o1000, 𝓣)
      , testCase "S/T give special bits without execute: u=rwS,o=rT" $
          ok (parseFilePerms "u=rwS,o=rT") @?= (0o644 .|. 0o4000 .|. 0o1000, 𝓣)
      ]
  ]

{-

A few notes on the cases:
0777 — the basic octal path; Bool is False.
07777 — confirms all four high bits (setuid, setgid, sticky, and the leading octal digit) round-trip.
777 — the mandatory-leading-zero rule: this must fail (the octal branch insists on char '0', and 777 matches nothing else).
010000 — the range check in octalMode.
rwxr-xr-- — a plain ls triplet expansion, 0754.
rwsr-Sr-t — exercises all four special-bit letters at once: setuid+setgid with execute (s), setgid without execute (S), sticky with execute (t); equals 0o7774 = rws r-S r-t → 4 2 7 octal digits... more precisely 0o4000 .|. 0o2000 .|. 0o0774 = 0o7774? Let me be precise: user rws = 0o700 + 0o4000, group r-S = 0o040 + 0o2000 (no group execute), other r-t = 0o004 + 0o1000 (no other execute) → 0o4000 .|. 0o2000 .|. 0o1000 .|. 0o744 = 0o7744. So the expected literal should be 0o7744, not 0o7774 — please use:


      , testCase "special bits rwsr-Sr-t" $
          ok (parseFilePerms "rwsr-Sr-t") @?= (0o7744, 𝓕)

u+rw,g-w,o=a — the multi-clause example from your original spec; applied from base 0 it yields 0644, Bool True.
u+s,+t — lowercase s/t imply execute: setuid + x on user, sticky + x on other → 0o4101 .|. 0o1000 = 0o5101. Correction to my expectation above: u+s gives 0o4111? No — u+s sets setuid and x only for user → 0o4000 .|. 0o100 = 0o4100; +t (who defaults to a) sets sticky and x for all → 0o1000 .|. 0o111 = 0o1111; combined: 0o4100 .|. 0o1111 = 0o5111. So:
haskell
Copy


      , testCase "s/t imply execute: u+s,+t" $
          ok (parseFilePerms "u+s,+t") @?= (0o5111, 𝓣)

9. u=rwS,o=rT — the non-standard extension: setuid and sticky without any execute bits → 0o4000 .|. 0o644 .|. 0o1000 = 0o7644. So:

      , testCase "S/T give special bits without execute: u=rwS,o=rT" $
          ok (parseFilePerms "u=rwS,o=rT") @?= (0o7644, 𝓣)

10. That's nine in the listing above plus the corrected literals — if you want a clean tenth, a good one is the empty who-list with -:


      , testCase "empty who-list defaults to 'a': -w" $
          ok (parseFilePerms "-w") @?= (0o222, 𝓣)

Apologies for the arithmetic wobble mid-answer — the final, correct expectations are: case 6 → 0o7744, case 8 → 0o5111, case 9 → 0o7644. With those substitutions the ten cases are: 0777, 07777, 777 (rejected), 010000 (rejected), rwxr-xr--, rwsr-Sr-t, u+rw,g-w,o=a, u+s,+t, u=rwS,o=rT, and -w.

-}

-}

tests = testGroup "FilePerms" [ octalModeTests, lsModeTests, chmodModeTests ]

_test ∷ IO()
_test = defaultMain tests

-- that's all, folks! ----------------------------------------------------------
