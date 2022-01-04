module Main where

import           Control.Monad.Trans.Class
import qualified Data.Set                  as S
import           System.Console.Haskeline

import           FOL.Parser
import           FOL.Util

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "fol> "
      case minput of
        Nothing    -> return ()
        Just input -> case input of
          s | s `elem` reserved -> goReserved s
          _ -> do
            case parse input of
              Left peb  -> outputStrLn (show peb)
              Right fol -> outputStrLn (show fol)
            loop

    goReserved :: String -> InputT IO ()
    goReserved s = do
      minput <- getInputLine $ "fol-" <> s <> "> "
      case minput of
        Nothing    -> loop
        Just input -> do
          case parse input of
            Left peb  -> outputStrLn (show peb)
            Right fol -> case s of
              "free"  -> lift . print . S.toList $ free fol
              "bound" -> lift . print . S.toList $ bound fol
              "consts" -> lift . print . S.toList $ constants fol
              "vars" -> lift . print . S.toList $ variables fol
              "rename-bound" -> do
                outputStrLn "What variable to rename?"
                old <- lift getLine
                outputStrLn "What's its new identifier?"
                new <- lift getLine
                lift . print $ renameBound old new fol
              _       -> outputStrLn "Not one of the reserved words."
          goReserved s

    reserved :: [String]
    reserved = ["free", "bound", "rename-bound"]
