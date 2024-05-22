module Postmap.Tui.AppEvent where

import qualified Brick
import qualified Brick.Widgets.List
import qualified Graphics.Vty as V
import Postmap.Tui.AppState


data AppEvent
  = AppEventNone


appHandleEvent :: Brick.BrickEvent () AppEvent -> Brick.EventM () AppState ()
appHandleEvent e =
  case e of
    Brick.VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl]) -> Brick.halt
    le -> do
      state <- Brick.get
      case _appStatePerspective state of
        AppPerspectiveAbout -> pure ()
        AppPerspectiveRecords _ -> appHandleEventListRecords le


appHandleEventListRecords :: Brick.BrickEvent () AppEvent -> Brick.EventM () AppState ()
appHandleEventListRecords (Brick.VtyEvent ev) = do
  state <- Brick.get
  case _appStatePerspective state of
    AppPerspectiveRecords ps -> do
      (nlr, _) <- Brick.nestEventM ps $ Brick.Widgets.List.handleListEvent ev
      Brick.put $ state {_appStatePerspective = AppPerspectiveRecords nlr}
    _ -> pure ()
appHandleEventListRecords _ = pure ()
