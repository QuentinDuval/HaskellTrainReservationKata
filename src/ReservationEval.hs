-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
module ReservationEval where

import Control.Lens
import Control.Monad.State
import Data.List(find, isSubsequenceOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import ReservationExpr

--------------------------------------------------------------------------------
-- Interpreter: Transformation from the abstract problem to real world (fake one here)
--------------------------------------------------------------------------------

class SPI spi where
  searchTrainAt :: spi -> DateTime -> IO [TrainId]
  getTypologyOf :: spi -> TrainId -> IO (Maybe TrainTypology)
  confirmCommand :: spi -> Reservation -> IO (Maybe Reservation)

evalReservation :: SPI spi => spi -> ReservationExpr ty -> IO ty
evalReservation spi = evalCmd
  where
    evalCmd :: ReservationExpr ty -> IO ty
    evalCmd (Log msg) = putStrLn msg
    evalCmd (Pure val) = pure val
    evalCmd (Bind val next) = evalCmd val >>= evalCmd . next
    evalCmd (SearchTrain time) = searchTrainAt spi time
    evalCmd (GetTypology trainId) = getTypologyOf spi trainId
    evalCmd (Reserve command) = confirmCommand spi command

-- type AppLens a b = forall f. Applicative f => (b -> f b) -> (a -> f a)

tryReserve :: Map.Map TrainId TrainTypology -> Reservation -> Maybe (Map.Map TrainId TrainTypology)
tryReserve trains command = do
  seats <- trains ^? zoomSeats command
  if _reservedSeatNbs command `Set.isSubsetOf` seats
    then pure (trains & zoomSeats command .~ (seats `Set.difference` _reservedSeatNbs command))
    else Nothing
  where
    -- zoomSeats :: Reservation -> AppLens (Map.Map TrainId TrainTypology) (Set.Set SeatId)
    zoomSeats command = ix (_reservedTrainId command) . coaches . ix (_reservedCoachId command) . availableSeats

evalStateReservation :: ReservationExpr ty -> State (Map.Map TrainId TrainTypology) ty
evalStateReservation = evalCmd
  where
    evalCmd :: ReservationExpr ty -> State (Map.Map TrainId TrainTypology) ty
    evalCmd (Log msg) = pure ()
    evalCmd (Pure val) = pure val
    evalCmd (Bind val next) = evalCmd val >>= evalCmd . next
    evalCmd (SearchTrain time) = fmap Map.keys get
    evalCmd (GetTypology tid) = fmap (Map.lookup tid) get
    evalCmd (Reserve command) = do
      trains <- get
      case tryReserve trains command of
        Nothing -> pure Nothing
        Just newTrains -> do
          put newTrains
          pure (Just command)
