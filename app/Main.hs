
{-# LANGUAGE TupleSections #-}

module Main where

import Text.Printf
import Parser
import qualified VM as VM

import System.Environment (getArgs)
import System.Directory (doesFileExist)

import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT(..),runExceptT)

err :: Monad m => a -> ExceptT a m b
err a = ExceptT $ return (Left a)

readFile' :: FilePath -> ExceptT String IO String
readFile' file = do
    exist <- liftIO $ doesFileExist file
    when (not exist) (err "input file not found")
    liftIO $ readFile file

main :: IO ()
main = do
    res <- main'
    either putStrLn print res

main' = runExceptT $ do
    args <- liftIO $ getArgs
    (name,src) <- case args of
        [] -> ("",) <$> liftIO getContents
        _  -> let file = head args in (file,) <$> readFile' file

    case parse name src of
        Left m -> err (show m)
        Right prog ->
            case toExp prog of
                Left m -> err $ "###Syntax Error### " ++ m
                Right exp ->
                    case VM.run exp [] of
                        Left m -> err $ "###Runtime Error###" ++ m
                        Right v -> return v
