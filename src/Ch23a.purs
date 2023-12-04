module Ch23a where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, delay, forkAff, joinFiber, killFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class.Console (log)
import Effect.Exception (error)

data TickTock = Tick | Tock

derive instance eqTickTock :: Eq TickTock

data BombState = WaitingTick | WaitingTock

clock :: AVar TickTock -> Aff Unit
clock ttAVar = do
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tick ttAVar
  void $ AVar.take ttAVar
  delay (Milliseconds 1000.0)
  AVar.put Tock ttAVar
  clock ttAVar

bomb :: AVar TickTock -> Int -> Aff Unit
bomb ttAVar destCount = go 0 WaitingTick
  where
  go :: Int -> BombState -> Aff Unit
  go count state = do
    if count == destCount then log "BOOM"
    else do
      delay (Milliseconds 500.0)
      tt <- AVar.read ttAVar
      case state of
        WaitingTick ->
          if tt == Tick then log "Tick" *> go count WaitingTock
          else go count state
        WaitingTock ->
          if tt == Tock then log "Tock" *> go (count + 1) WaitingTick
          else go count state

test :: Effect Unit
test = launchAff_ do
  ttAVar <- AVar.empty
  clockFiber <- forkAff $ clock ttAVar
  bombFiber <- forkAff $ bomb ttAVar 3
  AVar.put Tick ttAVar
  joinFiber bombFiber
  killFiber (error "Exploded") clockFiber

