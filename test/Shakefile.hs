{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

import Control.DeepSeq
import Data.Binary
import Data.Hashable
import Data.List.Extra
import Data.Maybe
import Data.Typeable
import Development.Shake
import Development.Shake.FilePath
import Safe
import System.Directory
import System.Info.Extra

project = "javascript-extras"
projectTarget = "test"

-- | Unfortunately, Cabal is missing programatic querying of build outputs
-- https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products
jsexePath ghcjsVersion projVersion = "dist-newstyle/build/x86_64-"
    <> if isWindows then "windows" else "linux"
    <> "/ghcjs-" <> ghcjsVersion <> "/"
    <> project <> "-" <> projVersion <> "/x/"
    <> project <> "-" <> projectTarget <> "/build/"
    <> project <> "-" <> projectTarget <> "/"
    <> project <> "-" <> projectTarget <> ".jsexe"

-- | convert the jsexe all.js from ghcjs compilation into a node module
jsexeModule str = "module.exports = function(){\n"
    <> "var global = globalThis;\n"
    <> str
    <> "\n}\n"

buildGhcjsPackageCmd pkg pkgTarget = do
    pwd <- liftIO $ getCurrentDirectory
    let parentWd = takeDirectory $ dropTrailingPathSeparator pwd
    command_ [Cwd parentWd] "cabal" ["v2-build", "--ghcjs", pkg <> "-" <> pkgTarget]

newtype GhcjsVersion = GhcjsVersion () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcjsVersion = String
oracleGhcjsVersion = addOracle $ \(GhcjsVersion _) -> (trim . fromStdout) <$> cmd "ghcjs --numeric-version" :: Action String

newtype ProjectDirectory = ProjectDirectory () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ProjectDirectory = String
-- | project directory is above this one
oracleProjectDirectory = addOracle $ \(ProjectDirectory _) -> liftIO $ (takeDirectory . dropTrailingPathSeparator) <$> getCurrentDirectory

newtype ProjectVersion = ProjectVersion () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ProjectVersion = String
oracleProjectVersion getProjectDirectory = addOracle $ \(ProjectVersion _) -> do
    projDir <- getProjectDirectory $ ProjectDirectory ()
    Stdout out <- command [Cwd projDir] "grep"  ["^\\s*version:", project <.> "cabal"]
    pure $ words out `at` 1

main :: IO ()
main = shakeArgs shakeOptions $ do
    getGhcjsVersion <- oracleGhcjsVersion
    getProjectDirectory <- oracleProjectDirectory
    getProjectVersion <- oracleProjectVersion getProjectDirectory

    want [build </> "hsMain.js"]

    phony "clean" $ do
        putNormal $ "Deleting " <> build
        removeFilesAfter build ["//"]

    project <> "-" <> projectTarget <> ".jsexe" </> "all.js" %> \_ -> do
         alwaysRerun
         buildGhcjsPackageCmd project projectTarget

    build </> "hsMain.js" %> \out -> do
        ghcVer <- getGhcjsVersion $ GhcjsVersion ()
        projectVer <- getProjectVersion $ ProjectVersion ()
        projectDir <- getProjectDirectory $ ProjectDirectory ()
        let jsexe = jsexePath ghcVer projectVer
            alljs = projectDir </> jsexe </> "all.js"
        src <- readFile' alljs
        writeFile' out $ jsexeModule src
  where
    build = shakeFiles shakeOptions
