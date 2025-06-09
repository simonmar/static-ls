{-# LANGUAGE RecordWildCards #-}
module StaticLS.Glean (
    getSymbols,
    findReference
  ) where

import Control.Monad.IO.Class
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
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
import StaticLS.IDE.Monad
import StaticLS.StaticEnv

glassService :: Service
glassService = HostPort "127.0.0.1" 25053

glassRepo :: Glass.RepoName
glassRepo = Glass.RepoName "haxl"

defCfg :: ThriftServiceOptions
defCfg = def { processingTimeout = Just 15000 }

svc :: Service -> ThriftService Glass.GlassService
svc s =  mkThriftService s defCfg

getSymbols ::
  (MonadIde m, MonadIO m) =>
  AbsPath ->
  m Glass.DocumentSymbolIndex
getSymbols path = do
  staticEnv <- getStaticEnv
  let relPath = Path.makeRelative staticEnv.wsRoot path
  liftIO $ runThrift staticEnv.eventBase (svc glassService) $ do
      let
        query = def {
                Glass.documentSymbolsRequest_repository = glassRepo
              , Glass.documentSymbolsRequest_filepath =
                  Glass.Path (T.pack (Path.toFilePath relPath))
              , Glass.documentSymbolsRequest_include_refs = True
        }
      Glass.documentSymbolIndex query def

findReference
  :: AbsPath
  -> LineCol
  -> Glass.DocumentSymbolIndex
  -> Maybe FileLcRange
findReference wsRoot (LineCol (Pos l) (Pos c)) ix =
  listToMaybe
    [ toFileLcRange rg
    | Glass.SymbolX{..} <- Map.findWithDefault [] (fromIntegral (l+1)) refs
    , inRange (fromIntegral (l+1)) (fromIntegral (c+1)) symbolX_range
    , Just rg <- [symbolX_target]
    ]
  where
  refs = Glass.documentSymbolIndex_symbols ix
  inRange line col (Glass.Range lb cb le ce) =
    line >= lb && line <= le &&
    (if line == lb then col >= cb else False) &&
    (if line == le then col <= ce else False)

  toFileLcRange Glass.LocationRange{ locationRange_range = Glass.Range{..}, ..} =
    FileWith (wsRoot Path.</> Path.filePathToRel (T.unpack (Glass.unPath locationRange_filepath)))
      (mkLineColRange begin end)
    where
    begin = LineCol (pos (range_lineBegin-1)) (pos (range_columnBegin-1))
    end = LineCol (pos (range_lineEnd-1)) (pos (range_columnEnd-1))
    pos = mkPos . fromIntegral

