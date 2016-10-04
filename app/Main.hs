{-# LANGUAGE GADTs #-}

module Main where

import System.Environment (getArgs)

import VCMangler

parse l = case parseMangledName l of 
            (Left a) -> show a
            (Right b) -> show b

main :: IO ()
main = do
    [inputFile, outputFile] <- getArgs
    content <- readFile inputFile
    writeFile outputFile $ unlines $ map (\l -> parse l ++ "   " ++ l) (lines content)