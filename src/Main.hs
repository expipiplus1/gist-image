{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Data.Foldable
import           Data.Semigroup
import           Data.String
import           Data.Text
import           Data.Text.IO       as T
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.IO.Temp
import           Turtle             (cp, procStrict, procs)

main :: IO ()
main = do
  files <- getArgs
  gistFiles files

gistFiles :: [FilePath] -> IO ()
gistFiles files = withSystemTempDirectory "gist" $ \tmpDir -> do
  let tmpName = tmpDir </> "dummy"
  T.writeFile tmpName "_"
  procStrict "gist" ["-p", fromString tmpName] mempty >>= \case
    (ExitFailure _, out) -> do
      T.hPutStrLn stderr "gist failed"
      T.hPutStrLn stderr out
      exitFailure
    (ExitSuccess, out) -> do
      let gitRepoHTTPS = strip out
          Just gitRepoSSH = do
            pl <- stripPrefix "https://" gitRepoHTTPS
            pure ("git@" <> replace "/" ":" pl <> ".git")
          gitDir = tmpDir </> "git"
      procs "git" ["clone", gitRepoSSH, fromString gitDir] mempty
      let cpToGit file = cp (fromString file) (fromString (gitDir </> takeFileName file))
      traverse_ cpToGit files
      withCurrentDirectory gitDir $ do
        procs "git" ["rm", fromString (takeFileName tmpName)] mempty
        procs "git" ("add" : ((fromString . takeFileName) <$> files)) mempty
        procs "git" ["commit", "--amend", "--no-edit", "--allow-empty-message"] mempty
        procs "git" ["push", "--force"] mempty
        T.putStrLn out
