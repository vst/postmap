module Postmap.Tui.App where

import qualified Brick
import Postmap.Spec (Spec)


tui :: Spec -> IO ()
tui _s = do
  Brick.simpleMain (Brick.str "Hello, world!" :: Brick.Widget ())
