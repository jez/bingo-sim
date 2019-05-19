-- |
-- This is a port of
-- [xoshiro256**](http://xoshiro.di.unimi.it/xoshiro256starstar.c) from C to
-- Haskell. The documentation and comments below come from the original C
-- sources.
--
-- Written in 2018 by David Blackman and Sebastiano Vigna (vigna@acm.org)
--
-- To the extent possible under law, the author has dedicated all copyright
-- and related and neighboring rights to this software to the public domain
-- worldwide. This software is distributed without any warranty.
--
-- See <http://creativecommons.org/publicdomain/zero/1.0/>.
module BingoSim.Prng
  (
  -- * Seeding the generator
    State
  , mkState
  -- * Generating random numbers
  , next
  -- * Jumps
  , jump
  , longJump
  )
where

import           Data.Bits
import           Data.Word

-- | The state of the generator.
--
-- The state must be seeded so that it is not everywhere zero. If you have
-- a 64-bit seed, we suggest to seed a splitmix64 generator and use its
-- output to fill s.
--
-- In Haskell, we've made this type opaque. See 'mkState' to construct a 'State'.
data State = State
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64

instance Show State where
  show (State s0 s1 s2 s3) =
    "mkState " ++ (show s0) ++ " " ++ (show s1) ++ " " ++ (show s2) ++ " " ++ (show s3)

-- | Create an initial state from a seed.
--
-- Raises an exception if the initial seed is all zeros.
--
-- >>> mkState 0 0 0 0
-- *** Exception: The state must be seeded so that it is not zero everywhere.
-- >>> mkState 1 2 3 4
-- mkState 1 2 3 4
mkState :: Word64 -> Word64 -> Word64 -> Word64 -> State
mkState 0 0 0 0 =
  error "The state must be seeded so that it is not zero everywhere."
mkState s0 s1 s2 s3 = State s0 s1 s2 s3

-- | This is xoshiro256** 1.0, our all-purpose, rock-solid generator. It has
-- excellent (sub-ns) speed, a state (256 bits) that is large enough for
-- any parallel application, and it passes all tests we are aware of.
--
-- For generating just floating-point numbers, xoshiro256+ is even faster.
--
-- >>> let state = mkState 1 2 3 4
-- >>> next state (\rand _state' -> rand)
-- 11520
--
-- Note: When porting to Haskell, we've explicitly chosen to make the API of
-- 'next' in CPS-style (i.e., using a callback) to avoid always allocating a
-- tuple for the result.
--
-- If you'd rather just have a tuple, you can pass @(,)@ as the callback:
--
-- >>> next state (,)
-- (11520,mkState 7 0 262146 211106232532992)
next :: State -> (Word64 -> State -> a) -> a
next (State s0 s1 s2 s3) f =
  let result = ((s1 * 5) `rotateL` 7) * 9

      t      = s1 `unsafeShiftL` 17

      s2'    = s2 `xor` s0
      s3'    = s3 `xor` s1
      s1'    = s1 `xor` s2'
      s0'    = s0 `xor` s3'

      s2''   = s2' `xor` t
      s3''   = s3' `rotateL` 45
  in  f result (State s0' s1' s2'' s3'')

-- | This is the jump function for the generator. It is equivalent
-- to 2^128 calls to next(); it can be used to generate 2^128
-- non-overlapping subsequences for parallel computations.
--
-- Note: This function is not yet implemented in Haskell.
jump :: State -> State
jump (State _s0 _s1 _s2 _s3) = undefined

-- | This is the long-jump function for the generator. It is equivalent to
-- 2^192 calls to next(); it can be used to generate 2^64 starting points,
-- from each of which jump() will generate 2^64 non-overlapping
-- subsequences for parallel distributed computations.
--
-- Note: This function is not yet implemented in Haskell.
longJump :: State -> State
longJump (State _s0 _s1 _s2 _s3) = undefined
