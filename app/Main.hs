{-# LANGUAGE OverloadedStrings #-}


module Main where

import Turtle
import Data.String (fromString)
import Data.Text.IO as Tio
import Data.Text as Text
import Data.Maybe (fromJust)

newtype HaskellFile = HF Text deriving (Show, Eq)

fromHaskellFile :: HaskellFile -> Text 
fromHaskellFile (HF text) = text 

main :: IO () 
main = sh $ do 
   haskellFiles <- getHaskellFiles 
   avgWords     <- calculateAverageWords haskellFiles
   liftIO $ Prelude.putStrLn $ "Number of haskell files are: " ++ (show $ Prelude.length haskellFiles)
   liftIO $ mapM_ (Tio.putStrLn . fromHaskellFile) haskellFiles
   liftIO $ Prelude.putStrLn $ "Average word count = " ++ (show avgWords)
   
calculateAverageWords :: [HaskellFile] -> Shell Double
calculateAverageWords haskF = do 
    let listOfWC  = (Main.countWords <$> haskF) :: [IO Int]
        ints      = sequence listOfWC :: IO [Int]
        shellInts = liftIO ints 
    wordCountL    <- shellInts
    let avg       = (fromIntegral $ sum wordCountL) / (fromIntegral $ Prelude.length wordCountL)
    return avg 


countWords :: HaskellFile -> IO Int 
countWords haskF = do 
     wordCount <- ((Tio.readFile (Text.unpack $ fromHaskellFile haskF)) >>= \t -> 
                     (pure $ Text.words t) >>= \t' -> 
                        (pure $ Prelude.length t'))
     return $ wordCount

getHaskellFiles :: Shell [HaskellFile]
getHaskellFiles = do 
      currentDirectryPath       <- pwd     
      paths                     <- reduce foldLSOut (ls currentDirectryPath)
      let haskellFiles = Prelude.filter (Text.isSuffixOf ".hs") paths
      return $ HF <$> haskellFiles

step :: [FilePath] -> FilePath -> [FilePath]
step acc path = (Prelude.lines path) ++ acc
initial :: [FilePath]
initial = [] 
extract :: [FilePath] -> [Text]
extract paths = (format fp) <$> paths 

foldLSOut = Fold step initial extract 



                    
    
    



    



