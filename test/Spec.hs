import           Data.Either
import qualified Data.Set    as S
import           Test.Hspec

import           FOL.Base
import           FOL.Parser
import           FOL.Util

testTerminalCollectors :: String -> String -> [String] -> Spec
testTerminalCollectors f s l =
  let fol = fromRight (error $ "Parsing of " <> s <> " failed.") $ parse s
  in
    it (f <> " " <> show fol <> " should be " <> show l) $
      (case f of
        "free"   -> free
        "bound"  -> bound
        "consts" -> constants
        "vars"   -> variables
        _        -> undefined) fol `shouldBe` S.fromList l

testRenaming :: String -> String -> String -> String -> Spec
testRenaming old new toRename expected =
  let fol = fromRight (error $ "Parsing of " <> toRename <> " failed.") $ parse toRename
      fol' = fromRight (error $ "Parsing of " <> expected <> " failed.") $ parse expected
  in
    it ("After renaming the bound variable `" <> old <> "` to `" <> new <> "` in " <> show fol <>
        " we should get " <> show fol') $ do
      renameBound old new fol `shouldBe` fol'

main :: IO ()
main = hspec $ do
  describe "Free variables" $ do
    testTerminalCollectors "free" "@ :x . Eq(:x, :x)" []
    testTerminalCollectors "free" "@ :x . ? :y . Neq(:x, :y)" []
    testTerminalCollectors "free" "? :y . Neq(:x, :y)" ["x"]
    testTerminalCollectors "free" "@ :eps . Gt(:eps, _0) -> ? :delta . Gt(:delta, _0) &\
      \ @ :x . Between(_0, sub(:x, :p), :delta) -> Between(_0, sub(f(:x), :L), :eps)" ["p", "L"]
  describe "Free variables" $ do
    testTerminalCollectors "bound" "@ :x . Eq(:x, :x)" ["x"]
    testTerminalCollectors "bound" "@ :x . ? :y . Neq(:x, :y)" ["x", "y"]
    testTerminalCollectors "bound" "? :y . Neq(:x, :y)" ["y"]
    testTerminalCollectors "bound" "@ :eps . Gt(:eps, _0) -> ? :delta . Gt(:delta, _0) &\
      \ @ :x . Between(_0, sub(:x, :p), :delta) -> Between(_0, sub(f(:x), :L), :eps)"
        ["eps", "delta", "x"]
  describe "Constants" $ do
    testTerminalCollectors "consts" "@ :x . Eq(:x, :x)" []
    testTerminalCollectors "consts" "@ :x . ? :y . Neq(:x, :y)" []
    testTerminalCollectors "consts" "? :y . Neq(:x, :y)" []
    testTerminalCollectors "consts" "@ :eps . Gt(:eps, _0) -> ? :delta . Gt(:delta, _0) &\
      \ @ :x . Between(_0, sub(:x, :p), :delta) -> Between(_0, sub(f(:x), :L), :eps)" ["0"]
  describe "All variables" $ do
    testTerminalCollectors "vars" "@ :x . Eq(:x, :x)" ["x"]
    testTerminalCollectors "vars" "@ :x . ? :y . Neq(:x, :y)" ["x", "y"]
    testTerminalCollectors "vars" "? :y . Neq(:x, :y)" ["x", "y"]
    testTerminalCollectors "vars" "@ :eps . Gt(:eps, _0) -> ? :delta . Gt(:delta, _0) &\
      \ @ :x . Between(_0, sub(:x, :p), :delta) -> Between(_0, sub(f(:x), :L), :eps)"
        ["eps", "delta", "x", "p", "L"]
  describe "Bound variable renaming" $ do
    testRenaming "x" "x'" "@ :x . Eq(:x, :x)" "@ :x' . Eq(:x', :x')"
    testRenaming "x" "x'" "@ :y . Eq(:x, :y)" "@ :y . Eq(:x, :y)"
    testRenaming "x" "x'"
      "(@ :x . Eq(:y, :x)) -> (@ :y . Eq(:x, :y))"
      "(@ :x' . Eq(:y, :x')) -> (@ :y . Eq(:x, :y))"
