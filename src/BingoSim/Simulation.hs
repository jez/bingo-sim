{-# LANGUAGE BinaryLiterals #-}

-- |
-- Description: ← Start here
--
-- Simulates the likelyhood of winning a children's carnival bingo game.
--
-- The game itself works like this:
--
-- 1. There's a 6 x 6 grid, each with it's own special character identifying it.
-- 2. There are 36 tiles, one for each grid space.
-- 3. Initially, all tiles are face down.
-- 4. To play, a contestant chooses 15 of the 36 tiles and flips them over.
-- 5. The contestant places the flipped tiles onto the correct spots.
-- 6. If placing the 15 tiles forms a bingo in any row, column, or full
--    diagonal, it's a win. Otherwise, it's a loss.
--
-- Our question is: if one of our friends wins this game, how lucky should they
-- consider themself? Rather than compute the probability exactly, here we run
-- a simulation to approximate the exact probability of a win.
--
-- To represent a bingo board and the operations on them, we've created the
-- 'Board' type, which is a bit vector representing the grid in row major order
-- where a @1@ means that a tile was placed on that grid space. There is also a
-- 'hasBingo' helper to figure out whether a 'Board' has a bingo.
--
-- This module has the logic to actually carry out the simulation:
-- 'runSimulation'
--
-- (Note: This module is basically the @Main@ module, except I couldn't figure
-- out how to generate Haddock documentation for an executable target, not a
-- library target.)

module BingoSim.Simulation where

import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Word
import           Text.Printf

import           BingoSim.Board
import           BingoSim.Prng  (mkState, next)
import qualified BingoSim.Prng  as Prng

-- | Run the entire simulation, consisting of @trials@ trials.
--
-- Prints the results to stdout when done.
--
-- >>> runSimulation 100000
-- Trials:   100000
-- Bingos:   3615
-- Hit rate: 0.03615
--
-- This function is called directly by the @bingo-sim@ executable's @main@
-- method, so you can get the same effect by running @bingo-sim@ at the command
-- line, instead of the Haskell REPL:
--
-- @
-- ❯ bingo-sim 100000
-- Trials:   100000
-- Bingos:   3615
-- Hit rate: 0.03615
-- @
runSimulation
  :: Int -- ^ @trials@: The number of trials to run.
  -> IO ()
runSimulation trials = do
  count  <- newIORef 0
  genRef <- newIORef (mkState 111 222 333 444)

  replicateM_ trials $ do
    gen           <- readIORef genRef
    (board, gen') <- randomBoard gen
    writeIORef genRef gen'
    case hasBingo board of
      Just _  -> modifyIORef count (+ 1)
      Nothing -> return ()

  bingos <- readIORef count
  let rate = (fromIntegral bingos) / (fromIntegral trials)
  printf "Trials:   %d\n" (trials :: Int)
  printf "Bingos:   %d\n" (bingos :: Int)
  printf "Hit rate: %f\n" (rate :: Float)

-- * Simulation helpers

-- | Generate a random board.
--
-- Uses a somewhat contrived strategy:
--
-- 1. Start with a bit sequence with fifteen 1's (@0x7fff@).
-- 2. Use [Fisher-Yates](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle) to shuffle the individual bits among the lower 36 bits of the sequence.
--
-- This is much faster than the naive strategy of:
--
-- 1. Generate the numbers @0@ to @35@ in a list and shuffle them.
-- 2. Take the first 15, to represent picking 15 random tiles.
-- 3. Flip on the bits corresponding to each tile we picked.
--
-- The Fisher-Yates on bits approach is faster because we don't have to
-- generate a linked list of thunks and instead can operate on a single 64-bit
-- word.
--
-- The sacrifice is that the naive strategy nearly exactly matches our
-- intuition for how this game works in the real world.
randomBoard :: Prng.State -> IO (Board, Prng.State)
randomBoard gen = do
  let board = Board 0x7fff
  return $ shuffleBits gen board 36

-- | Implements [Fisher-Yates](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle) at the bit level for a 'Board'.
--
-- Uses recursion to swap the current bit into place, from most to least
-- significant.
shuffleBits
  :: Prng.State
  -> Board
  -> Int -- ^ @n@: The current bit we're considering swapping or leaving alone (1-indexed).
  -> (Board, Prng.State)
shuffleBits gen board      1 = (board, gen)
shuffleBits gen (Board bs) n = next gen withRand
 where
  n' = n - 1
  withRand rand gen' =
    let i   = rand `mod` (fromIntegral n)
        bs' = swapBits bs n' (fromIntegral i)
    in  shuffleBits gen' (Board bs') n'

-- | Helper for swapping two specific bits.
--
-- Graciously taken from Sean Eron Anderson's [Bit Twiddling
-- Hacks](https://graphics.stanford.edu/~seander/bithacks.html#SwappingBitsXOR),
-- specialized to the case of a length 1 range of bits.
swapBits
  :: Word64 -- ^ Input bits
  -> Int -- ^ @i@: Index of one bit to swap
  -> Int -- ^ @j@: Index of the other bit to swap
  -> Word64 -- ^ Swapped result
swapBits bs i j | i == j = bs
swapBits bs i j =
  let x = ((shiftR bs i) `xor` (shiftR bs j)) .&. 0x1
  in  bs `xor` ((shiftL x i) .|. (shiftL x j))

