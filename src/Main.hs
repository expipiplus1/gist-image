{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

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
main =
  getArgs >>= \case
    [file] -> gistFile file
    _ -> do
      T.hPutStrLn stderr "Usage: gist-image FILE"
      exitFailure

gistFile :: FilePath -> IO ()
gistFile file = withSystemTempDirectory "gist" $ \tmpDir -> do
  let tmpName = tmpDir </> takeFileName file
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
      procStrict "git" ["clone", gitRepoSSH, fromString gitDir] mempty >>= \case
        (ExitFailure _, _) -> do
          T.hPutStrLn stderr "git clone failed"
          exitFailure
        (ExitSuccess, _) -> do
          cp (fromString file) (fromString (gitDir </> takeFileName file))
          withCurrentDirectory gitDir $ do
            procs "git" ["add", fromString (takeFileName file)] mempty
            procs "git" ["commit", "--amend", "--no-edit", "--allow-empty-message"] mempty
            procs "git" ["push", "--force"] mempty
            T.putStrLn out
