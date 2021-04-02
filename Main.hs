module Main where

import System.Directory.Recursive ( getFilesRecursive )
import System.Environment ( getArgs )   
import Control.Monad ( join )
import Debug.Trace ( trace )
import Data.List ( isSubsequenceOf, intercalate )
import System.Directory
import qualified Text.Megaparsec as P
import System.Process
import Parser ( runParser, compile )

allFiles :: FilePath -> IO [FilePath]
allFiles dirName = (\a b c d e f -> a++b++c++d++e++f++[dirName ++ "/main.ecsl"])
    <$> getFilesRecursive (filePrefix "/src/assets" )
    <*> getFilesRecursive (filePrefix "/src/entities" )
    <*> getFilesRecursive (filePrefix "/src/systems")
    <*> getFilesRecursive (filePrefix "/src/states")
    <*> getFilesRecursive (filePrefix "/src/components")
    <*> getFilesRecursive (filePrefix "/src/modules")
    where filePrefix = ((dirName ++ "/") ++)

changeExtension :: [Char] -> [Char] -> [Char]
changeExtension fname newExt = takeWhile (/= '.') fname ++ newExt

compileFile :: FilePath -> IO ()
compileFile fileName = do
    contents <- readFile fileName 
    case runParser fileName $ filter (/= '\t') contents of
        Right n -> writeFile (changeExtension fileName ".lua") $ compile n
        Left e -> error $ P.errorBundlePretty e

compileDir :: FilePath -> IO ()
compileDir fs = allFiles fs >>= \fs -> mapM_ compileFile $ filter (isSubsequenceOf ".ecsl") fs

runDir :: FilePath -> IO ()
runDir dirName = do
    compileDir dirName
    callCommand ("lovec " ++ dirName)

createDirs :: FilePath -> IO ()
createDirs dir = do
    createDirectory dir
    mapM_ (\dn -> createDirectory $ dir ++ "/" ++ dn) ["lib", "assets", "src"]
    mapM_ (\fn -> copyFile ("./lib/" ++ fn) (dir ++ "/lib/" ++ fn)) ["30log.lua", "anim8.lua", "beholder.lua", "gamestate.lua", "multisource.lua", "sti.lua", "tiny.lua"]
    mapM_ (\x -> createDirectory $ dir ++ "/src/" ++ x) ["assets", "entities", "systems", "states", "components", "modules"]
    mapM_ (\fn -> copyFile fn (dir ++ "/" ++ fn)) ["main.ecsl", "conf.lua"]

main :: IO ()
main = do 
    args <- getArgs
    dispatch args where
        dispatch ["compile", dir] = compileDir dir
        dispatch ["run", dir] = runDir dir
        dispatch ["init", dir] = createDirs dir
        dispatch a = error $ "Unknown set of arguments: " ++ intercalate ", " a