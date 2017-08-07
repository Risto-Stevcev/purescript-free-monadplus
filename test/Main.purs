module Test.Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Test.Spec.Runner (RunnerEffects)
import Test.Control.MonadPlus.Free (main) as Free

main ∷ Eff (RunnerEffects ()) Unit
main = Free.main
