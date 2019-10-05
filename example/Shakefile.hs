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
import qualified Distribution.PackageDescription.Parsec as D
import qualified Distribution.Pretty as D
import qualified Distribution.Types.GenericPackageDescription as D
import qualified Distribution.Types.PackageDescription as D
import qualified Distribution.Types.PackageId as D
import qualified Distribution.Verbosity as D
import Safe
import System.Directory
import System.Info.Extra

project = "javascript-extras-example"

-- | Unfortunately, Cabal is missing programatic querying of build outputs
-- https://www.haskell.org/cabal/users-guide/nix-local-build.html#where-are-my-build-products
jsexePath ghcjsVersion projVersion = "dist-newstyle/build/x86_64-"
    <> if isWindows then "windows" else "linux"
    <> "/ghcjs-" <> ghcjsVersion <> "/"
    <> project <> "-" <> projVersion <> "/x/"
    <> project <> "/build/"
    <> project <> "/"
    <> project <> ".jsexe"

-- | convert the jsexe all.js from ghcjs compilation into a node module
jsexeModule str = "module.exports = function(){\n"
    <> "var global = globalThis;\n"
    <> str
    <> "\n}\n"

newtype GhcjsVersion = GhcjsVersion () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult GhcjsVersion = String
oracleGhcjsVersion = addOracle $ \(GhcjsVersion _) -> (trim . fromStdout) <$> cmd "ghcjs --numeric-version" :: Action String

newtype ProjectVersion = ProjectVersion () deriving (Show, Typeable, Eq, Hashable, Binary, NFData)
type instance RuleResult ProjectVersion = String
oracleProjectVersion = addOracle $ \(ProjectVersion _) -> do
    desc <- liftIO $ D.readGenericPackageDescription D.normal (project <.> "cabal")
    pure $ D.prettyShow $ D.pkgVersion $ D.package $ D.packageDescription desc

main :: IO ()
main = shakeArgs shakeOptions $ do
    getGhcjsVersion <- oracleGhcjsVersion
    getProjectVersion <- oracleProjectVersion

    want [build </> "hsMain.js"]

    phony "clean" $ do
        putNormal $ "Deleting " <> build
        removeFilesAfter build ["//"]

    project <> ".jsexe" </> "all.js" %> \_ -> do
         alwaysRerun
         cmd_ "cabal" ["v2-build", "--ghcjs", project]

    build </> "hsMain.js" %> \out -> do
        ghcVer <- getGhcjsVersion $ GhcjsVersion ()
        projectVer <- getProjectVersion $ ProjectVersion ()
        let jsexe = jsexePath ghcVer projectVer
            alljs = jsexe </> "all.js"
        src <- readFile' alljs
        writeFile' out $ jsexeModule src
  where
    build = shakeFiles shakeOptions
