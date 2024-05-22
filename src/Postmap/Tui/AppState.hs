{-# LANGUAGE TemplateHaskell #-}

module Postmap.Tui.AppState where

import qualified Brick.Widgets.List
import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)
import Postmap.Spec (Record, Spec (..))


-- * State


data AppState = AppState
  { _appStateSpec :: !Spec
  , _appStateSpecPath :: !(Maybe FilePath)
  , _appStatePerspective :: !AppPerspective
  }


initAppState :: Spec -> AppState
initAppState spec =
  AppState
    { _appStateSpec = spec
    , _appStateSpecPath = Nothing
    , _appStatePerspective = AppPerspectiveRecords (mkListRecordsState spec)
    }


-- ** Perspectives


data AppPerspective
  = AppPerspectiveAbout
  | AppPerspectiveRecords RecordsPerspective


-- *** Records


type RecordsPerspective = Brick.Widgets.List.List () Record


mkListRecordsState :: Spec -> RecordsPerspective
mkListRecordsState =
  flip (Brick.Widgets.List.list ()) 1 . V.fromList . specRecords


makeLenses ''AppState
