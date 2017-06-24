{-# LANGUAGE TemplateHaskell #-}
module Occupancy where

import Control.Lens

data Occupancy
  = Occupancy { _occupied, _totalCount :: Int }
  deriving (Show, Eq, Ord)

makeLenses ''Occupancy

instance Monoid Occupancy where
  mempty = Occupancy 0 0
  mappend a b = Occupancy (_occupied a + _occupied b) (_totalCount a + _totalCount b)

addOccupied :: Int -> Occupancy -> Occupancy
addOccupied seatRequest = over occupied (+ seatRequest)

toPercentage :: Occupancy -> Double
toPercentage r = fromIntegral (_occupied r) / fromIntegral (_totalCount r)
