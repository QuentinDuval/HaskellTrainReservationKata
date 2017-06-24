{-# LANGUAGE TemplateHaskell #-}
module ReservationExpr where

import Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Domain types (concepts)
--------------------------------------------------------------------------------

type TrainId = String
type CoachId = String
type SeatId = Int
type DateTime = Int

data ReservationRequest = ReservationRequest {
  _seatCount :: Int,
  _dateTime :: DateTime
} deriving (Show, Eq, Ord)

data CoachTypology = CoachTypology {
  _totalSeatCount :: Int,
  _availableSeats :: Set.Set SeatId
} deriving (Show, Eq, Ord)

makeLenses ''CoachTypology

data TrainTypology = TrainTypology {
  _coaches :: Map.Map CoachId CoachTypology
} deriving (Show, Eq, Ord)

coaches :: Lens' TrainTypology (Map.Map CoachId CoachTypology)
coaches = lens _coaches (\train newCoachs -> train { _coaches = newCoachs })

data Reservation = Reservation {
  _reservedTrainId :: TrainId,
  _reservedCoachId :: CoachId,
  _reservedSeatNbs :: Set.Set SeatId
} deriving (Show, Eq, Ord)

data ReservationResult
  = Confirmed Reservation
  | NoTrainAvailable
  deriving (Show, Eq, Ord)


--------------------------------------------------------------------------------
-- Helper constructors
--------------------------------------------------------------------------------

trainTypology :: [(CoachId, CoachTypology)] -> TrainTypology
trainTypology = TrainTypology . Map.fromList

coachTypology :: Int -> [SeatId] -> CoachTypology
coachTypology totalSeats availableSeats = CoachTypology totalSeats (Set.fromList availableSeats)


--------------------------------------------------------------------------------
-- Definition of a DSL for reservation
--------------------------------------------------------------------------------

data ReservationExpr a where
  SearchTrain :: DateTime -> ReservationExpr [TrainId]
  GetTypology :: TrainId -> ReservationExpr (Maybe TrainTypology)
  Reserve :: Reservation -> ReservationExpr (Maybe Reservation)
  Log :: String -> ReservationExpr ()
  Pure :: ta -> ReservationExpr ta
  Bind :: ReservationExpr ta -> (ta -> ReservationExpr tb) -> ReservationExpr tb

instance Functor ReservationExpr where
  fmap fn expr = expr >>= Pure . fn

instance Applicative ReservationExpr where
  pure = Pure
  fExpr <*> aExpr = fExpr >>= \f -> fmap f aExpr

instance Monad ReservationExpr where
  return = pure
  (>>=) = Bind
