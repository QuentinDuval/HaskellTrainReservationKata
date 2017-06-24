module Main where

import Control.Monad (when)
import qualified Data.Map as Map
import ReservationApi
import ReservationEval
import ReservationExpr


--------------------------------------------------------------------------------
-- Test version with a Fake SPI implementation
--------------------------------------------------------------------------------

data FakeSPI = FakeSPI (Map.Map TrainId TrainTypology)

instance SPI FakeSPI where
  searchTrainAt (FakeSPI db) _ = pure (Map.keys db)
  getTypologyOf (FakeSPI db) tid = pure $ Map.lookup tid db
  confirmCommand (FakeSPI db) r = pure (Just r)

fakeTrainDB :: FakeSPI
fakeTrainDB = FakeSPI $ Map.fromList $
  [ ("T1", trainTypology [("A", coachTypology 100 [71..100]), ("B", coachTypology 100 [71..100])]) -- Full Train
  , ("T2", trainTypology [("A", coachTypology 100 [80..100]), ("B", coachTypology 100 [50..100])]) -- First coach full
  , ("T3", trainTypology [("A", coachTypology 100 [20..100]), ("B", coachTypology 100 [1..100])]) -- Plenty of places
  ]

sendReservation :: Int -> IO ()
sendReservation seatCount = do
  result <- evalReservation fakeTrainDB $ reserve (ReservationRequest seatCount 10)
  print result

main :: IO ()
main = do
  putStrLn "> [Test] Number of seats to reserve:"
  seatCount <- fmap read getLine
  when (seatCount > 0) $ do
    sendReservation seatCount
    main
