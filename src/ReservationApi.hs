module ReservationApi (reserve) where

import Control.Lens
import Control.Monad
import Data.List(partition)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Occupancy
import ReservationExpr

--------------------------------------------------------------------------------

trainMaxOccupancy :: Double
trainMaxOccupancy = 0.7

coachMaxOccupancy :: Double
coachMaxOccupancy = 0.8

coachOccupancy :: CoachTypology -> Occupancy
coachOccupancy coach =
  let totalSeats = _totalSeatCount coach
      freeSeats = length (_availableSeats coach)
  in Occupancy { _occupied = max 0 (totalSeats - freeSeats), _totalCount = totalSeats }

trainOccupancy :: TrainTypology -> Occupancy
trainOccupancy = foldMap coachOccupancy . _coaches

trainTypologies :: (TrainId, TrainTypology) -> [(TrainId, CoachId, CoachTypology)]
trainTypologies (trainId, train) = map (\(coachId, coach) -> (trainId, coachId, coach)) (Map.toList (_coaches train))

coachToReservation :: Int -> (TrainId, CoachId, CoachTypology) -> Reservation
coachToReservation seatRequest (trainId, coachId, coach) =
  Reservation { _reservedTrainId = trainId
              , _reservedCoachId = coachId
              , _reservedSeatNbs = (Set.fromDistinctAscList . take seatRequest . Set.toAscList) (_availableSeats coach) }

projectedOccupancy :: Int -> Occupancy -> Double
projectedOccupancy seatRequest = toPercentage . addOccupied seatRequest

reservationsByDecreasingPreference :: Int -> [(TrainId, TrainTypology)] -> [Reservation]
reservationsByDecreasingPreference seatRequest trains =
  let freeTrains = filter ((<= trainMaxOccupancy) . projectedOccupancy seatRequest . trainOccupancy . snd) trains
      allCoaches = concatMap trainTypologies freeTrains
      validCoaches = filter ((<= 1.0) . projectedOccupancy seatRequest . coachOccupancy . view _3) allCoaches
      (best, next) = partition ((<= coachMaxOccupancy) . projectedOccupancy seatRequest . coachOccupancy . view _3) validCoaches
  in map (coachToReservation seatRequest) (best ++ next)

queryTrainTypologies :: [TrainId] -> ReservationExpr [(TrainId, TrainTypology)]
queryTrainTypologies trainIds = do
  typologies <- forM trainIds $ \trainId -> (fmap . fmap) (trainId,) (GetTypology trainId)
  pure (catMaybes typologies)

reserve :: ReservationRequest -> ReservationExpr ReservationResult
reserve request = do
    trainIds <- SearchTrain (_dateTime request)
    typologies <- queryTrainTypologies trainIds
    let reservations = reservationsByDecreasingPreference (_seatCount request) typologies
    Log ("Valid reservations (by preference): " ++ show reservations)
    confirmByPref reservations
  where
    -- TODO: refactor (in any case, we should look in reserve... but before)
    confirmByPref [] = Pure NoTrainAvailable
    confirmByPref (r:rs) = do
      validated <- Reserve r
      case validated of
        Nothing -> confirmByPref rs
        Just ok -> Pure (Confirmed ok)
