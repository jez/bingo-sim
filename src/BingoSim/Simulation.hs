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
import           System.Random
import           System.Random.Shuffle
import           Text.Printf

import           BingoSim.Board

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
  genRef <- newStdGen >>= newIORef

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
-- Uses a somewhat naive strategy:
--
-- 1. Generate the numbers @0@ to @35@ in a 'List'
-- 2. Shuffle them using 'System.Random' and 'System.Random.Shuffle'
-- 3. Take the first 15 of this list to represent picking 15 random tiles.
-- 4. Flip on the bit corresponding to each tile we picked.
--
-- The benefit is that this exactly matches the intuition we have for how this
-- game works in the real world.
randomBoard :: RandomGen g => g -> IO (Board, g)
randomBoard gen = do
  let (rands, gen') = randomSequence gen 35 1
  let shuffled      = shuffle [0 .. 35] rands
  let hits          = take 15 shuffled
  let bits          = map bit hits
  let board         = foldr (.|.) (0x0 :: Word64) bits
  return (Board board, gen')

randomSequence
  :: RandomGen g
  => g
  -> Int -- ^ @n@: The number of elements this sequence will be used to sort
  -> Int -- ^ @i@: The current random index we're generating (1-indexed)
  -> ([Int], g)
randomSequence gen n i | n == i = ([], gen)
randomSequence gen n i =
  let (tl, gen' ) = randomSequence gen n (i + 1)
      (hd, gen'') = randomR (0, 36 - i) gen'
  in  (hd : tl, gen'')

