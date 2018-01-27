{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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



-- | Events that can be sent
-- | Here there is just one event for updating the time
newtype Event = EventUpdateTime Tm.LocalTime

-- | Names use to identify each of the controls
data Name = TypeSearch
          | TextSearch
          | ListResults
          deriving (Show, Eq, Ord)

-- | Sort order
data SortBy = SortNone
            | SortAsc
            | SortDec
            deriving (Eq)


-- | State of the brick app. Contains the controls and any other required state
data BrickState = BrickState { _stEditType :: !(BE.Editor Text Name) -- ^ Editor for the type to search for
                             , _stEditText :: !(BE.Editor Text Name) -- ^ Editor for a text search in the results
                             , _stTime :: !Tm.LocalTime              -- ^ The current time
                             , _stFocus :: !(BF.FocusRing Name)      -- ^ Focus ring - a circular list of focusable controls
                             , _stResults :: [H.Target]              -- ^ The last set of search results from hoohle
                             , _stResultsList :: !(BL.List Name H.Target) -- ^ List for the search results
                             , _stSortResults :: SortBy                   -- ^ Current sort order for the results
                             }

makeLenses ''BrickState


-- | Defines how the brick application will work / handle events
app :: B.App BrickState Event Name
app = B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pure
            , B.appAttrMap = const theMap
            }


main :: IO ()
main = do
  chan <- BCh.newBChan 5 -- ^ create a bounded channel for events

  -- Send a tick event every 1 seconds with the current time
  -- Brick will send this to our event handler which can then update the stTime field
  void . forkIO $ forever $ do
    t <- getTime 
    BCh.writeBChan chan $ EventUpdateTime t
    threadDelay $ 1 * 1000000

  -- Initial current time value
  t <- getTime

  -- Construct the initial state values
  let st = BrickState { _stEditType = BE.editor TypeSearch (Just 1) ""
                      , _stEditText = BE.editor TextSearch (Just 1) ""
                      , _stResultsList = BL.list ListResults Vec.empty 1
                      , _stTime = t
                      , _stFocus = BF.focusRing [TypeSearch, TextSearch, ListResults]
                      , _stResults = []
                      , _stSortResults = SortNone
                      }
          
  -- And run brick
  void $ B.customMain (V.mkVty V.defaultConfig) (Just chan) app st

  where
    -- | Get the local time
    getTime = do
      t <- Tm.getCurrentTime
      tz <- Tm.getCurrentTimeZone
      pure $ Tm.utcToLocalTime tz t


-- | Main even handler for brick events
handleEvent :: BrickState -> B.BrickEvent Name Event -> B.EventM Name (B.Next BrickState)
handleEvent st ev =
  case ev of
    -- Handle keyboard events
    --   k is the key
    --   ms are the modifier keys
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        -- Escape quits the app, no matter what control has focus
        (K.KEsc, []) -> B.halt st

        _ ->
          -- How to interpret the key press depends on which control is focused
          case BF.focusGetCurrent $ st ^. stFocus of
            Just TypeSearch ->
              case k of
                K.KChar '\t' -> do
                  -- Search, clear sort order, focus next
                  found <- doSearch st
                  B.continue . filterResults $ st & stFocus %~ BF.focusNext
                                                  & stResults .~ found
                                                  & stSortResults .~ SortNone

                K.KBackTab ->do
                  -- Search, clear sort order, focus prev
                  found <- doSearch st
                  B.continue  . filterResults $ st & stFocus %~ BF.focusPrev
                                                   & stResults .~ found
                                                   & stSortResults .~ SortNone

                K.KEnter -> do
                  -- Search, clear sort order, focus on results
                  --  This makes it faster if you want to search and navigate results without tabing through the text search box
                  found <- doSearch st
                  B.continue . filterResults $ st & stResults .~ found
                                                  & stSortResults .~ SortNone
                                                  & stFocus %~ BF.focusNext & stFocus %~ BF.focusNext
                                                  -- TODO with brick >= 0.33, rather than 2x focus next: & stFocus %~ BF.focusSetCurrent ListResults

                _ -> do
                  -- Let the editor handle all other events
                  r <- BE.handleEditorEvent ve $ st ^. stEditType
                  next <- liftIO . searchAhead doSearch $ st & stEditType .~ r 
                  B.continue next


            Just TextSearch ->
              case k of
                K.KChar '\t' -> B.continue $ st & stFocus %~ BF.focusNext -- Focus next
                K.KBackTab -> B.continue $ st & stFocus %~ BF.focusPrev   -- Focus previous
                _ -> do
                  -- Let the editor handle all other events
                  r <- BE.handleEditorEvent ve $ st ^. stEditText
                  B.continue . filterResults $ st & stEditText .~ r

            Just ListResults ->
              case k of
                K.KChar '\t' -> B.continue $ st & stFocus %~ BF.focusNext -- Focus next
                K.KBackTab -> B.continue $ st & stFocus %~ BF.focusPrev   -- Focus previous
                K.KChar 's' ->
                  -- Toggle the search order between ascending and descending, use asc if sort order was 'none'
                  let sortDir = if (st ^. stSortResults) == SortAsc then SortDec else SortAsc in
                  let sorter = if sortDir == SortDec then (Lst.sortBy $ flip compareType) else (Lst.sortBy compareType) in
                  B.continue . filterResults $ st & stResults %~ sorter
                                                  & stSortResults .~ sortDir

                _ -> do
                  -- Let the list handle all other events
                  -- Using handleListEventVi which adds vi-style keybindings for navigation
                  --  and the standard handleListEvent as a fallback for all other events
                  r <- BL.handleListEventVi BL.handleListEvent ve $ st ^. stResultsList
                  B.continue $ st & stResultsList .~ r

            _ -> B.continue st

    (B.AppEvent (EventUpdateTime time)) ->
      -- Update the time in the state
      B.continue $ st & stTime .~ time
      
    _ -> B.continue st

  where
    doSearch st' = 
      liftIO $ searchHoogle (Txt.strip . Txt.concat $ BE.getEditContents (st' ^. stEditType))


-- | Search ahead for type strings longer than 3 chars.
searchAhead :: (BrickState -> IO [H.Target]) -> BrickState -> IO BrickState
searchAhead search st =
  let searchText = Txt.strip . Txt.concat . BE.getEditContents $ st ^. stEditType in

  if Txt.length (Txt.filter (`notElem` [' ', '\t', '(', ')', '=']) searchText) > 3
  then do
    -- Search
    found <- search st
    pure . filterResults $ st & stResults .~ found
                              & stSortResults .~ SortNone
  else
    -- Just clear
    pure $ st & stResults .~ []
              & stResultsList %~ BL.listClear


-- | Filter the results from hoogle using the search text
filterResults :: BrickState -> BrickState
filterResults st =
  let allResults = st ^. stResults in
  let filterText = Txt.toLower . Txt.strip . Txt.concat . BE.getEditContents $ st ^. stEditText in

  let results =
        if Txt.null filterText
        then allResults
        else filter (\t -> Txt.isInfixOf filterText . Txt.toLower $ formatResult t) allResults
  in
  st & stResultsList .~ BL.list ListResults (Vec.fromList results) 1
  

-- | Draw the UI
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
      B.padLeft (B.Pad 2) (B.txtWrap . reflow $ getSelectedDetail (Txt.pack . clean . H.targetDocs))
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


-- | Reformat the text so that it can be wrapped nicely
reflow :: Text -> Text
reflow = Txt.replace "\0" "\n\n" . Txt.replace "\n" "" . Txt.replace "\n\n" "\0" 


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr        , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr , V.black `B.on` V.yellow)
                              , (BL.listAttr        , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr, V.blue `B.on` V.white)
                              , ("infoTitle"        , B.fg V.cyan)
                              , ("time"             , B.fg V.yellow)
                              ]


----------------------------------------------------------------------------------------------
-- | Compare two hoogle results for sorting
compareType :: H.Target -> H.Target -> Ordering
compareType a b =
  compare (formatResult a) (formatResult b)

  
-- | Search hoogle using the default hoogle database
searchHoogle :: Text -> IO [H.Target]
searchHoogle f = do
  d <- H.defaultDatabaseLocation 
  H.withDatabase d (\x -> pure $ H.searchDatabase x (Txt.unpack f))
  

-- | Format the hoogle results so they roughly match what the terminal app would show
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
