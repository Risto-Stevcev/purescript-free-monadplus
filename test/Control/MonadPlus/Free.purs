module Test.Control.MonadPlus.Free where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.MonadPlus.Free (FreeMonadPlus, foldFree, liftFree)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude (class Functor, type (~>), Unit, pure, bind, discard, unit, (*>), (<>))
import Test.Spec (describe, it)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)

data ConsoleF next
  = ConsoleLog String next
  | ConsoleError String next

type Console a = FreeMonadPlus ConsoleF a

instance functorConsoleF ∷ Functor ConsoleF where
  map f (ConsoleLog   string next) = ConsoleLog   string (f next)
  map f (ConsoleError string next) = ConsoleError string (f next)

consoleLog ∷ String → Console Unit
consoleLog string = liftFree (ConsoleLog string unit)

consoleError ∷ String → Console Unit
consoleError string = liftFree (ConsoleError string unit)

interpretConsole ∷ ∀ e. ConsoleF ~> Aff (console ∷ CONSOLE | e)
interpretConsole (ConsoleLog string next) = log string *> pure next
interpretConsole (ConsoleError string next) = log string *> pure next

foo ∷ String
foo = "Foo"

bar ∷ String
bar = "Bar"

hello ∷ String
hello = "Hello"

world ∷ String
world = "World"

program ∷ Console Unit
program = do
  _ ← consoleLog hello
  empty <|> consoleLog foo <|> consoleLog bar
  consoleLog world

main ∷ Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "Control.MonadPlus.Free" do
    it ("should print '" <> hello <> "', '" <> foo <> "', and '" <> world <> "' to the console") do
      foldFree interpretConsole program
