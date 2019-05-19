{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}

-- |
-- This module contains the data types we'll be using, which is really just a
-- way to represent and operate on boards.
--
-- For our purposes, we only need random boards, so it's sufficient to identify
-- a board position by its index and then represent a board by a bit vector.
-- Compared to a list or some other kind of sequence, this lets us use less
-- space and perform operations on boards in fewer instructions.
--
-- Since we're using a 'Word64' but only representing boards with 36 grid
-- spaces, we only use the 36 least significant bits: @0xf_ffff_ffff@
--
-- We represent boards in row-major order, so each multiple of 6 bits is a row.
module BingoSim.Board where

import           Data.Bits
import           Data.Word
import           Text.Printf

-- * Boards

-- | The type of a 6x6 game board, as a bit sequence. Only the 36 least
-- significant bits are meaningful.
newtype Board = Board Word64
  deriving
    ( PrintfArg -- ^ So we can access the underlying 'Word64' easily when
                -- printing:
                --
                -- >>> printf "%#0*b\n" 38 (Board 0x1)
                -- 0b000000000000000000000000000000000001
    )

-- | Only compares the 36 least significant bits.
instance Eq Board where
  (==) (Board board1) (Board board2) =
    let mask = 0xf_ffff_ffff
     in (board1 .&. mask) == (board2 .&. mask)


-- |
-- Visualize a board as a 6x6 grid.
--
-- >>> print (Board 0x123456789)
-- 000100
-- 100011
-- 010001
-- 010110
-- 011110
-- 001001
instance Show Board where
  show (Board board) =
    let r1 = (board .&. row1) `shiftR` 30
        r2 = (board .&. row2) `shiftR` 24
        r3 = (board .&. row3) `shiftR` 18
        r4 = (board .&. row4) `shiftR` 12
        r5 = (board .&. row5) `shiftR` 6
        r6 = (board .&. row6)
     in printf "%06b\n%06b\n%06b\n%06b\n%06b\n%06b\n" r1 r2 r3 r4 r5 r6

-- * Bingos
--
-- $Bingos
-- The main operation we want to do on a board is check whether it has a bingo
-- or not.

-- | Check whether there's a bingo, and if so, return the BingoLocation
-- corresponding to where the bingo occured (i.e., which row, column, or
-- diagonal).
--
-- In the case where there are multiple bingos, returns one of them
-- arbitrarily.
--
-- >>> hasBingo (Board 0x123456789)
-- Nothing
-- >>> hasBingo (Board 0x3f)
-- Just Row6
hasBingo :: Board -> Maybe BingoLocation
hasBingo (Board board) | (board .&. dia1 == dia1) = Just Dia1
hasBingo (Board board) | (board .&. dia2 == dia2) = Just Dia2
hasBingo (Board board) | (board .&. row1 == row1) = Just Row1
hasBingo (Board board) | (board .&. row2 == row2) = Just Row2
hasBingo (Board board) | (board .&. row3 == row3) = Just Row3
hasBingo (Board board) | (board .&. row4 == row4) = Just Row4
hasBingo (Board board) | (board .&. row5 == row5) = Just Row5
hasBingo (Board board) | (board .&. row6 == row6) = Just Row6
hasBingo (Board board) | (board .&. col1 == col1) = Just Col1
hasBingo (Board board) | (board .&. col2 == col2) = Just Col2
hasBingo (Board board) | (board .&. col3 == col3) = Just Col3
hasBingo (Board board) | (board .&. col4 == col4) = Just Col4
hasBingo (Board board) | (board .&. col5 == col5) = Just Col5
hasBingo (Board board) | (board .&. col6 == col6) = Just Col6
hasBingo _             = Nothing

-- | An enum of where a bingo could occur on a 6x6 board.
data BingoLocation
  = Dia1 -- ^ Top left to bottom right
  | Dia2 -- ^ Top right to bottom left
  | Row1 -- ^ Top row
  | Row2
  | Row3
  | Row4
  | Row5
  | Row6 -- ^ Bottom row
  | Col1 -- ^ Far left column
  | Col2
  | Col3
  | Col4
  | Col5
  | Col6 -- ^ Far right column
  deriving (Eq, Show)

-- * Board Constants
--
-- $Constants
-- These are a handful of bit masks to project out the rows, columns, and
-- diagonals of a board.

-- | Diagonal from top left to bottom right
dia1 :: Word64
dia1 = 0b100000010000001000000100000010000001
-- | Diagonal from top right to bottom left
dia2 :: Word64
dia2 = 0b000001000010000100001000010000100000
-- | Top row
row1 :: Word64
row1 = 0b111111000000000000000000000000000000
-- | Second row from top
row2 :: Word64
row2 = 0b000000111111000000000000000000000000
-- | Third row from top
row3 :: Word64
row3 = 0b000000000000111111000000000000000000
-- | Fourth row from top
row4 :: Word64
row4 = 0b000000000000000000111111000000000000
-- | Fifth row from top
row5 :: Word64
row5 = 0b000000000000000000000000111111000000
-- | Bottom row
row6 :: Word64
row6 = 0b000000000000000000000000000000111111
-- | Far left column
col1 :: Word64
col1 = 0b100000100000100000100000100000100000
-- | Second column from left
col2 :: Word64
col2 = 0b010000010000010000010000010000010000
-- | Third column from left
col3 :: Word64
col3 = 0b001000001000001000001000001000001000
-- | Fourth column from left
col4 :: Word64
col4 = 0b000100000100000100000100000100000100
-- | Fifth column from left
col5 :: Word64
col5 = 0b000010000010000010000010000010000010
-- | Far right column
col6 :: Word64
col6 = 0b000001000001000001000001000001000001

