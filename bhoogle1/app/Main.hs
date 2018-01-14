{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}


module Main where

import           Protolude
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.List as Lst
import qualified Data.Time as Tm
import qualified Data.Text as Txt
import qualified Data.Vector as Vec
import           Brick ((<+>), (<=>))
import qualified Brick as B
import qualified Brick.BChan as BCh
import qualified Brick.Focus as BF
import qualified Brick.AttrMap as BA
import qualified Brick.Widgets.List as BL
import qualified Brick.Widgets.Edit as BE
import qualified Brick.Widgets.Border as BB
import qualified Brick.Widgets.Border.Style as BBS
import           Control.Concurrent (threadDelay, forkIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import qualified Hoogle as H


newtype Event = EventTick Tm.LocalTime

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
                             , _stTime :: !Tm.LocalTime
                             , _stFocus :: !(BF.FocusRing Name)
                             , _stResults :: [H.Target]
                             , _stResultsList :: !(BL.List Name H.Target)
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
  chan <- BCh.newBChan 5

  -- Send a tick event every 1 seconds
  void . forkIO $ forever $ do
    t <- getTime 
    BCh.writeBChan chan $ EventTick t
    threadDelay $ 1 * 1000000

  t <- getTime

  let st = BrickState { _stEditType = BE.editor TypeSearch (Just 1) ""
                      , _stEditText = BE.editor TextSearch (Just 1) ""
                      , _stResultsList = BL.list ListResults Vec.empty 1
                      , _stTime = t
                      , _stFocus = BF.focusRing [TypeSearch, TextSearch, ListResults]
                      , _stResults = []
                      , _stSortResults = SortNone
                      }
          
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st

  where
    getTime = do
      t <- Tm.getCurrentTime
      tz <- Tm.getCurrentTimeZone
      pure $ Tm.utcToLocalTime tz t


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
                                                  & stFocus %~ BF.focusSetCurrent ListResults

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
                  let sorter = if sortDir == SortDec then (Lst.sortBy $ flip compareType) else (Lst.sortBy compareType) in
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
        else filter (\t -> Txt.isInfixOf filterText . Txt.toLower $ formatResult t) allResults
  in
  st & stResultsList .~ BL.list ListResults (Vec.fromList results) 1
  

drawUI :: BrickState -> [B.Widget Name]
drawUI st =
  [B.padAll 1 contentBlock] 

  where
    contentBlock =
      (B.withBorderStyle BBS.unicode $ BB.border searchBlock)
      <=>
      B.padTop (B.Pad 1) resultsBlock
      
    resultsBlock =
      let total = show . length $ st ^. stResults in
      let showing = show . length $ st ^. stResultsList ^. BL.listElementsL in
      (B.withAttr "infoTitle" $ B.txt "Results: ") <+> B.txt (showing <> "/" <> total)
      <=>
      (B.padTop (B.Pad 1) $
       resultsContent <+> resultsDetail
      )

    resultsContent =
      BL.renderList (\_ e -> B.txt $ formatResult e) False (st ^. stResultsList)

    resultsDetail =
      B.padLeft (B.Pad 1) $
      B.hLimit 60 $
      vtitle "package:"
      <=>
      B.padLeft (B.Pad 2) (B.txt $ getSelectedDetail (\t -> maybe "" (Txt.pack . fst) (H.targetPackage t)))
      <=>
      vtitle "module:"
      <=>
      B.padLeft (B.Pad 2) (B.txt $ getSelectedDetail (\t -> maybe "" (Txt.pack . fst) (H.targetModule t)))
      <=>
      vtitle "docs:"
      <=>
      B.padLeft (B.Pad 2) (B.txt $ getSelectedDetail (Txt.pack . clean . H.targetDocs))
      <=>
      B.fill ' '
  
    searchBlock =
      ((htitle "Type: " <+> editor TypeSearch (st ^. stEditType)) <+> time (st ^. stTime))
      <=>
      (htitle "Text: " <+> editor TextSearch (st ^. stEditText))

    htitle t =
      B.hLimit 20 $
      B.withAttr "infoTitle" $
      B.txt t
      
    vtitle t =
      B.withAttr "infoTitle" $
      B.txt t

    editor n e =
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e

    time t =
      B.padLeft (B.Pad 1) $
      B.hLimit 20 $
      B.withAttr "time" $
      B.str (Tm.formatTime Tm.defaultTimeLocale "%H-%M-%S" t)

    getSelectedDetail fn =
      case BL.listSelectedElement $ st ^. stResultsList of
        Nothing -> ""
        Just (_, e) -> fn e


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr        , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr , V.black `B.on` V.yellow)
                              , (BL.listAttr        , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , ("infoTitle"        , B.fg V.cyan)
                              , ("time"             , B.fg V.yellow)
                              ]


----------------------------------------------------------------------------------------------
compareType :: H.Target -> H.Target -> Ordering
compareType a b =
  compare (formatResult a) (formatResult b)

  
searchHoogle :: Text -> IO [H.Target]
searchHoogle f = do
  d <- H.defaultDatabaseLocation 
  H.withDatabase d (\x -> pure $ H.searchDatabase x (Txt.unpack f))
  

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
