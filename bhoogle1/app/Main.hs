{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.List as Lst
import           Data.Time (UTCTime)
import qualified Data.Time as Tm
import qualified Data.Time.Format as Tm
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.Markup as BM
import           Brick.Markup ((@?))
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import           Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import qualified Hoogle as H


newtype Event = EventTick UTCTime

data Name = TypeSearch
          | TextSearch
          | ListResults
          deriving (Show, Eq, Ord)

data SortBy = SortNone
            | SortAsc
            | SortDec
            deriving (Eq)

data BrickState = BrickState { _stEditType :: !(BE.Editor Text Name)
                             , _stEditText :: !(BE.Editor Text Name)
                             , _stTime :: !UTCTime
                             , _stFocus :: !(BF.FocusRing Name)
                             , _stResults :: [Text]
                             , _stResultsList :: !(BL.List Name Text)
                             , _stSortResults :: SortBy
                             }

makeLenses ''BrickState

app :: B.App BrickState Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }


main :: IO ()
main = do
  chan <- BCh.newBChan 10

  -- Send a tick event every 1 seconds
  void . forkIO $ forever $ do
    t <- Tm.getCurrentTime
    BCh.writeBChan chan $ EventTick t
    threadDelay $ 1 * 1000000

  dt <- Tm.getCurrentTime

  let st = BrickState { _stEditType = BE.editor TypeSearch (Just 1) ""
                      , _stEditText = BE.editor TextSearch (Just 1) ""
                      , _stResultsList = BL.list ListResults Vec.empty 1
                      , _stTime = dt
                      , _stFocus = BF.focusRing [TypeSearch, TextSearch, ListResults]
                      , _stResults = []
                      , _stSortResults = SortNone
                      }
          
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st


handleEvent :: BrickState -> B.BrickEvent Name Event -> B.EventM Name (B.Next BrickState)
handleEvent st ev =

  case ev of
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        (K.KEsc, []) -> B.halt st
        _ ->
          case BF.focusGetCurrent $ st ^. stFocus of
            Just TypeSearch ->
              case k of
                K.KChar '\t' -> do
                  found <- liftIO $ searchHoogle (Txt.strip . Txt.concat $ BE.getEditContents (st ^. stEditType))
                  B.continue . updateResults $ st & stFocus %~ BF.focusNext
                                                  & stResults .~ found
                                                  & stSortResults .~ SortNone

                K.KBackTab ->do
                  found <- liftIO $ searchHoogle (Txt.strip . Txt.concat $ BE.getEditContents (st ^. stEditType))
                  B.continue  . updateResults $ st & stFocus %~ BF.focusPrev
                                                   & stResults .~ found
                                                   & stSortResults .~ SortNone

                K.KEnter -> do
                  found <- liftIO $ searchHoogle (Txt.strip . Txt.concat $ BE.getEditContents (st ^. stEditType))
                  B.continue . updateResults $ st & stResults .~ found
                                                  & stSortResults .~ SortNone

                _ -> do
                  r <- BE.handleEditorEvent ve $ st ^. stEditType
                  B.continue $ st & stEditType .~ r

            Just TextSearch ->
              case k of
                K.KChar '\t' -> B.continue $ st & stFocus %~ BF.focusNext
                K.KBackTab -> B.continue $ st & stFocus %~ BF.focusPrev
                _ -> do
                  r <- BE.handleEditorEvent ve $ st ^. stEditText
                  B.continue . updateResults $ st & stEditText .~ r

            Just ListResults ->
              case k of
                K.KChar '\t' -> B.continue $ st & stFocus %~ BF.focusNext
                K.KBackTab -> B.continue $ st & stFocus %~ BF.focusPrev
                K.KChar 's' ->
                  let sortDir = if (st ^. stSortResults) == SortAsc then SortDec else SortAsc in
                  let sorter = if sortDir == SortDec then (Lst.sortBy $ flip compare) else Lst.sort in
                  B.continue . updateResults $ st & stResults %~ sorter
                                                  & stSortResults .~ sortDir

                _ -> do
                  r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. stResultsList
                  B.continue $ st & stResultsList .~ r

            _ -> B.continue st

    (B.AppEvent (EventTick time)) ->
      B.continue $ st & stTime .~ time
      
    _ -> B.continue st


updateResults :: BrickState -> BrickState
updateResults st =
  let allResults = st ^. stResults in
  let filterText = Txt.toLower . Txt.strip . Txt.concat . BE.getEditContents $ st ^. stEditText in

  let results =
        if Txt.null filterText
        then allResults
        else filter (\t -> Txt.isInfixOf filterText $ Txt.toLower t) allResults
  in
  st & stResultsList .~ BL.list ListResults (Vec.fromList results) 1
  

drawUI :: BrickState -> [B.Widget Name]
drawUI st =
  [B.padAll 1 contentBlock] 

  where
    contentBlock =
      searchBlock
      <=>
      B.padTop (B.Pad 1) resultsBlock
      
    resultsBlock =
      B.txt ("Results: " <> (show . length $ st ^. stResultsList ^. BL.listElementsL) <> "/" <> (show . length $ st ^. stResults))
      <=>
      ( B.padAll 1 $
        BL.renderList (\_ e -> B.txt e) False (st ^. stResultsList)
      )
  
    searchBlock =
      ((title "Type: " <+> editor TypeSearch (st ^. stEditType)) <+> time (st ^. stTime))
      <=>
      (title "Text: " <+> editor TextSearch (st ^. stEditText))

    title t =
      B.hLimit 20 $
      B.txt t

    editor n e =
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e

    time t =
      B.padLeft (B.Pad 1) $
      B.hLimit 40 $
      B.str (Tm.formatTime Tm.defaultTimeLocale "%H-%M-%S" t)



theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr        , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr , V.black `B.on` V.yellow)
                              , (BL.listAttr        , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              ]


----------------------------------------------------------------------------------------------
searchHoogle :: Text -> IO [Text]
searchHoogle f = do
  d <- H.defaultDatabaseLocation 
  H.withDatabase d (\x -> pure $ formatResult <$> H.searchDatabase x (Txt.unpack f))
  

formatResult :: H.Target -> Text
formatResult t =
  let typ = clean $ H.targetItem t in
  let m = (clean . fst) <$> H.targetModule t in
  Txt.pack $ fromMaybe "" m <> " :: " <> typ
  

clean :: [Char] -> [Char]
clean = unescapeHTML . stripTags

-- | From hoogle source: https://hackage.haskell.org/package/hoogle-5.0.16/docs/src/General-Util.html
unescapeHTML :: [Char] -> [Char]
unescapeHTML ('&':xs)
    | Just x <- Lst.stripPrefix "lt;" xs = '<' : unescapeHTML x
    | Just x <- Lst.stripPrefix "gt;" xs = '>' : unescapeHTML x
    | Just x <- Lst.stripPrefix "amp;" xs = '&' : unescapeHTML x
    | Just x <- Lst.stripPrefix "quot;" xs = '\"' : unescapeHTML x
unescapeHTML (x:xs) = x : unescapeHTML xs
unescapeHTML [] = []
  
-- | From hakyll source: https://hackage.haskell.org/package/hakyll-4.1.2.1/docs/src/Hakyll-Web-Html.html#stripTags
stripTags :: [Char] -> [Char]
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs
