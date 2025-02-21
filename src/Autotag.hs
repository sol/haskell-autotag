{-# LANGUAGE LambdaCase #-}
module Autotag (
  main
, TagCreated(..)
, run
, getOutputFile
, extractVersion
) where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Version
import qualified System.Environment as Force (getEnv)
import           System.Environment.Blank
import           System.Process
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP

main :: IO ()
main = run createTag

die :: String -> IO a
die err = throwIO (ErrorCall err)

run :: CreateTag -> IO ()
run create = do
  dir <- getEnv "AUTOTAG_PACKAGE_PATH"

  tagPreReleases <- (== Just "true") <$> getEnv "AUTOTAG_PRE_RELEASES"
  dryRun <- (== Just "true") <$> getEnv "AUTOTAG_DRY_RUN"

  v@(Version branch tags) <- packageVersion dir

  prefix <- fromMaybe "v" <$> getEnv "AUTOTAG_PREFIX"
  let version = showVersion (makeVersion branch)
      versionWithTags = showVersion v
      tagName = prefix <> versionWithTags

  when (null tags || tagPreReleases) $ do
    r <- if dryRun then return TagCreated else create tagName
    when (r == TagCreated) $ do
      setOutput "created" "true"
  setOutput "name" tagName
  setOutput "version" version
  setOutput "version-tags" (intercalate "-" tags)
  setOutput "version-with-tags" versionWithTags

setOutput :: String -> String -> IO ()
setOutput name value = do
  outputFile <- getOutputFile
  appendFile outputFile $ name <> "=" <> value <> "\n"

getOutputFile :: IO FilePath
getOutputFile = Force.getEnv "GITHUB_OUTPUT"

packageVersion :: Maybe FilePath -> IO Version
packageVersion dir = do
  name <- findCabalFile dir
  input <- readFile name
  case extractVersion input of
    Just v -> return v
    Nothing -> die $ "Couldn't extract a version from " <> name

extractVersion :: String -> Maybe Version
extractVersion = extract >=> parse

extract :: String -> Maybe String
extract = mconcat . map stripVersionPrefix . lines . joinLineContinuations . dropComments
  where
    stripVersionPrefix :: String -> Maybe String
    stripVersionPrefix = stripPrefix "version:" . dropSpaces

    dropSpaces :: String -> String
    dropSpaces = filter (not . isSpace)

joinLineContinuations :: String -> String
joinLineContinuations = go
  where
    go = \ case
      '\n' : x : xs | isSpace x -> go (x : xs)
      x : xs -> x : go xs
      [] -> []

dropComments :: String -> String
dropComments = unlines . filter (not . isComment) . lines
  where
    isComment = isPrefixOf "--" . dropWhile isSpace

parse :: String -> Maybe Version
parse input = case reverse $ readP_to_S parseVersion input of
  (v, "") : _ -> Just v
  _ -> Nothing

findCabalFile :: Maybe FilePath -> IO FilePath
findCabalFile mDir = do
  cabalFiles <- filter isCabalFile <$> listDirectory dir
  case cabalFiles of
    [cabalFile] -> return (prependDir cabalFile)
    [] -> die $ "Couldn't find a .cabal file in " <> dir
    _ -> die $ "Multiple cabal files found in " <> dir
  where
    prependDir :: FilePath -> FilePath
    prependDir = maybe id ((</>)) mDir

    dir = fromMaybe "." mDir

    isCabalFile :: FilePath -> Bool
    isCabalFile name = ".cabal" `isSuffixOf` name && (not . isDotFile) name

    isDotFile :: FilePath -> Bool
    isDotFile name = "." `isPrefixOf` name

type CreateTag = String -> IO TagCreated

data TagCreated = TagCreated | TagAlreadyExists
  deriving (Eq, Show)

createTag :: CreateTag
createTag name = do
  tags <- lines <$> readProcess "git" ["tag"] ""
  if name `elem` tags then do
    return TagAlreadyExists
  else do
    callProcess "git" ["tag", name]
    callProcess "git" ["push", "--tags"]
    return TagCreated
