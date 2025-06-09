{-# LANGUAGE RecordWildCards #-}
module StaticLS.Glean (
    getSymbols,
    findSymbol,
    refTargets,
    findRefs,
    toRange,
  ) where

import Control.Monad.IO.Class
import Data.Default
import qualified Data.Map as Map
import Data.Path (AbsPath)
import Data.Path qualified as Path
import Data.Pos (Pos (..), mkPos)
import Data.LineCol (LineCol (..))
import Data.LineColRange
import Data.Text qualified as T
import Glean.Impl.ThriftService
import Glean.Util.ThriftService
import Glean.Util.Service
import qualified Glean.Glass.Types as Glass
import qualified Glean.Glass.GlassService.Client as Glass

import StaticLS.IDE.FileWith
import StaticLS.Logger
import StaticLS.StaticEnv

glassService :: Service
glassService = HostPort "127.0.0.1" 25053

glassRepo :: Glass.RepoName
glassRepo = Glass.RepoName "stackage"

defCfg :: ThriftServiceOptions
defCfg = def { processingTimeout = Just 15000 }

svc :: Service -> ThriftService Glass.GlassService
svc s =  mkThriftService s defCfg

getSymbols ::
  (HasStaticEnv m, MonadIO m) =>
  AbsPath ->
  Bool ->
  m Glass.DocumentSymbolIndex
getSymbols path includeRefs = do
  staticEnv <- getStaticEnv
  let relPath = Path.makeRelative staticEnv.wsRoot path
  liftIO $ runThrift staticEnv.eventBase (svc glassService) $ do
      let
        query = def {
                Glass.documentSymbolsRequest_repository = glassRepo
              , Glass.documentSymbolsRequest_filepath =
                  Glass.Path (T.pack (Path.toFilePath relPath))
              , Glass.documentSymbolsRequest_include_refs = includeRefs
        }
      Glass.documentSymbolIndex query def

findSymbol
  :: LineCol
  -> Glass.DocumentSymbolIndex
  -> [Glass.SymbolX]
findSymbol (LineCol (Pos l) (Pos c)) ix =
    [ sym
    | sym@Glass.SymbolX{..} <- Map.findWithDefault [] (fromIntegral (l+1)) refs
    , inRange (fromIntegral (l+1)) (fromIntegral (c+1)) symbolX_range
    ]
  where
  refs = Glass.documentSymbolIndex_symbols ix
  inRange line col (Glass.Range lb cb le ce) =
    line >= lb && line <= le &&
    (if line == lb then col >= cb else True) &&
    (if line == le then col <= ce else True)

refTargets :: AbsPath -> [Glass.SymbolX] -> [FileLcRange]
refTargets wsRoot syms =
  [ toFileLcRange wsRoot rg
  | Glass.SymbolX{..} <- syms
  , Just rg <- [symbolX_target]
  ]

toFileLcRange :: AbsPath -> Glass.LocationRange -> FileLcRange
toFileLcRange wsRoot locRange =
  FileWith (wsRoot Path.</> Path.filePathToRel (T.unpack path))
    (toRange locRange.locationRange_range)
  where
  path = Glass.unPath locRange.locationRange_filepath

toRange :: Glass.Range -> LineColRange
toRange range = mkLineColRange begin end
  where
  begin = LineCol (pos (range.range_lineBegin-1)) (pos (range.range_columnBegin-1))
  end = LineCol (pos (range.range_lineEnd-1)) (pos (range.range_columnEnd-1))
  pos = mkPos . fromIntegral

findRefs
  :: (HasStaticEnv m, HasLogger m, MonadIO m)
  => AbsPath
  -> LineCol
  -> m [FileLcRange]
findRefs path lineCol = do
  staticEnv <- getStaticEnv
  syms <- getSymbols path False
  logInfo $ "lineCol: " <> T.pack (show lineCol)
  logInfo $ "syms: " <> T.pack (show syms)
  case findSymbol lineCol syms of
    [] -> do
      logInfo $ "not found"
      return []
    (defn:_) -> do
      -- TODO: pick the innermost or tightest range if there are many
      logInfo $ "found: " <> T.pack (show defn)
      ranges <- liftIO $ runThrift staticEnv.eventBase (svc glassService) $ do
        Glass.findReferenceRanges defn.symbolX_sym def
      return (map (toFileLcRange staticEnv.wsRoot) ranges)
