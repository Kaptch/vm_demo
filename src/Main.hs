{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Brick
import qualified Brick.Focus                as F
import qualified Brick.Main                 as M
import qualified Brick.Types                as T
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style
import           Brick.Widgets.Center
import qualified Brick.Widgets.Edit         as E
import           Control.Monad.Except       (liftIO, runExceptT)
import           Control.Monad.State        (execStateT)
import           Data.Text.Zipper           (clearZipper)
import qualified Data.Vector                as Vec
import           ErrM
import qualified Graphics.Vty               as V
import           Lens.Micro
import           Lens.Micro.TH
import           SECD

data St
  = St { _secdSt :: SECDState
       , _edit   :: E.Editor String ()
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [center $ joinBorders $
             withBorderStyle ascii $
             borderWithLabel (str "vm demo") $
             drawS st
             <=> drawE st
             <=> drawC st
             <=> drawD st
             <=> drawInput st
            ]

drawS :: St -> Widget ()
drawS st = withBorderStyle ascii $
  borderWithLabel (str "S") $
  str $ show $ (st ^. secdSt) ^. s

drawE :: St -> Widget ()
drawE st = withBorderStyle ascii $
  borderWithLabel (str "E") $
  str $ show $ (st ^. secdSt) ^. e

drawC :: St -> Widget ()
drawC st = withBorderStyle ascii $
  borderWithLabel (str "C") $
  str $ show $ (st ^. secdSt) ^. c

drawD :: St -> Widget ()
drawD st = withBorderStyle ascii $
  borderWithLabel (str "D") $
  str $ show $ (st ^. secdSt) ^. d

drawInput :: St -> Widget ()
drawInput st = withBorderStyle ascii $
  borderWithLabel (str "input") $
  (E.renderEditor (str . unlines) True $ st ^. edit)

handleEvent :: St -> BrickEvent () e -> EventM () (Next St)
handleEvent st (T.VtyEvent event) =
  case event of
    V.EvKey V.KEsc []           -> M.halt st
    V.EvKey V.KEnter []         -> case parseCmds (unlines $ E.getEditContents $ st ^. edit) of
      Bad err -> M.continue $ st & edit %~ E.applyEdit clearZipper
      Ok cmds -> do
        let st' = st & (secdSt . c) .~ (foldl (\vec val -> Vec.snoc vec val)
                                              (st ^. (secdSt . c))
                                              cmds)
        let edit' = E.applyEdit clearZipper $ st ^. edit
        M.continue (st' & edit .~ edit')
    V.EvKey V.KRight [] -> do
      st' <- liftIO (execStateT (runExceptT evalCmds) (st ^. secdSt))
      M.continue $ St st' (st ^. edit)
    V.EvKey V.KDown []  -> do
      st' <- liftIO (execStateT (runExceptT evalNextCmd) (st ^. secdSt))
      M.continue $ St st' (st ^. edit)
    V.EvKey (V.KChar char) []   ->
      T.handleEventLensed st edit E.handleEditorEvent event
      >>= M.continue
    _                           -> M.continue st

theMap :: AttrMap
theMap = attrMap mempty []

app :: App St e ()
app = App { appDraw = drawUI
          , appChooseCursor = M.showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

initialSt :: St
initialSt = St initialState
  (E.editor () (Just 1) "")

main :: IO ()
main = defaultMain app initialSt >> return ()
