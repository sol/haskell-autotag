{-# LANGUAGE LambdaCase #-}
module Autotag (
  main
, TagCreated(..)
, run
, extractVersion
) where

import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Version
import           System.Environment.Blank
import           System.Exit hiding (die)
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
  dir <- getEnv "PACKAGE_PATH"

  tagPreReleases <- (== Just "true") <$> getEnv "TAG_PRE_RELEASES"
  dryRun <- (== Just "true") <$> getEnv "DRY_RUN"

  v@(Version branch tags) <- packageVersion dir

  prefix <- fromMaybe "v" <$> getEnv "TAG_PREFIX"
  let version = showVersion (makeVersion branch)
      versionWithTags = showVersion v
      tagName = prefix <> versionWithTags

  when (null tags || tagPreReleases) $ do
    r <- if dryRun then return TagCreated else create tagName
    when (r == TagCreated) $ do
      setOutput "created" "true"
  setOutput "tag-name" tagName
  setOutput "version" version
  setOutput "version-tags" (intercalate "-" tags)
  setOutput "version-with-tags" versionWithTags

setOutput :: String -> String -> IO ()
setOutput name value = putStrLn $ "::set-output name=" <> name <> "::" <> value

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
createTag name = withGitTag $ withGitPushTags $ return TagCreated
  where
    withGitTag :: IO TagCreated -> IO TagCreated
    withGitTag action = callProcessWithExitCode "git" ["tag", name] >>= \ case
      ExitSuccess -> action
      ExitFailure 128 -> return TagAlreadyExists
      e -> throwIO e

    withGitPushTags :: IO TagCreated -> IO TagCreated
    withGitPushTags action = callProcessWithExitCode "git" ["push", "--tags"] >>= \ case
      ExitSuccess -> action
      ExitFailure 1 -> return TagAlreadyExists
      e -> throwIO e

    callProcessWithExitCode :: FilePath -> [String] -> IO ExitCode
    callProcessWithExitCode cmd args = withCreateProcess process wait
      where
        process = (proc cmd args) {delegate_ctlc = True}
        wait _ _ _ = waitForProcess
