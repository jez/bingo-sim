{-# LANGUAGE LambdaCase #-}

module Main where

import           System.Environment

import           BingoSim.Simulation

main :: IO ()
main = getArgs >>= \case
  [arg1] -> runSimulation (read arg1)
  _      -> runSimulation 5000
