{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Verset -- replacing protolude until it works with GHC 9.12+
import           Control.Lens ((^.), (.~), (%~))
import           Control.Lens.TH (makeLenses)
import qualified Data.Map as Map
import qualified Data.List as Lst
import qualified Data.Time as Tm
import qualified Data.Text as Txt
import qualified Data.Text.Encoding as TxtE
import qualified Data.ByteString as BS
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
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Input.Events as K
import qualified Graphics.Vty.CrossPlatform
import qualified Graphics.Vty.Config
import           System.FilePath ((</>))
import qualified System.Directory as Dir
import qualified Hoogle as H
import qualified System.Process.Typed as PT
import qualified Data.ByteString.Lazy as BSL


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
data BrickState = BrickState { _stEditType :: !(BE.Editor Text Name)      -- ^ Editor for the type to search for
                             , _stEditText :: !(BE.Editor Text Name)      -- ^ Editor for a text search in the results
                             , _stTime :: !Tm.LocalTime                   -- ^ The current time
                             , _stFocus :: !(BF.FocusRing Name)           -- ^ Focus ring - a circular list of focusable controls
                             , _stResults :: [H.Target]                   -- ^ The last set of search results from hoohle
                             , _stResultsList :: !(BL.List Name H.Target) -- ^ List for the search results
                             , _stSortResults :: SortBy                   -- ^ Current sort order for the results
                             , _stDbPath :: FilePath                      -- ^ Hoogle DB path
                             , _yankCommand :: Text                       -- ^ Command to run to copy text to the clipboard
                             , _yankArgs :: Text                          -- ^ Args for the yank command
                             }

makeLenses ''BrickState


-- | Defines how the brick application will work / handle events
app :: B.App BrickState Event Name
app =
  B.App { B.appDraw = drawUI
            , B.appChooseCursor = B.showFirstCursor
            , B.appHandleEvent = handleEvent
            , B.appStartEvent = pass
            , B.appAttrMap = const theMap
            }


main :: IO ()
main = do
  -- Use the default hoogle DB. This may not exist because
  --  1) hoogle generate was never called
  --  2) the system hoogle is a different version from the package used here
  dbPath <- H.defaultDatabaseLocation
  Dir.doesFileExist dbPath >>= \case
    True -> runBHoogle dbPath
    False -> do
      putText ""
      putText "bhoogle error: "
      putText "   default hoogle database not found"
      putText $ "     at " <> Txt.pack dbPath
      putText "   You can create the database by installing hoogle and running"
      putText "     hoogle generate"
      putText ""


runBHoogle :: FilePath -> IO ()
runBHoogle dbPath = do
  chan <- BCh.newBChan 5 -- ^ create a bounded channel for events

  -- Send a tick event every 1 seconds with the current time
  -- Brick will send this to our event handler which can then update the stTime field
  void . forkIO $ forever $ do
    t <- getTime
    BCh.writeBChan chan $ EventUpdateTime t
    threadDelay $ 1 * 1000000

  -- Initial current time value
  t <- getTime

  -- Settings
  settings <- loadSettings

  -- Construct the initial state values
  let st = BrickState { _stEditType = BE.editor TypeSearch (Just 1) ""
                      , _stEditText = BE.editor TextSearch (Just 1) ""
                      , _stResultsList = BL.list ListResults Vec.empty 1
                      , _stTime = t
                      , _stFocus = BF.focusRing [TypeSearch, TextSearch, ListResults]
                      , _stResults = []
                      , _stSortResults = SortNone
                      , _stDbPath = dbPath
                      , _yankCommand = Map.findWithDefault "xclip" "yank" settings
                      , _yankArgs = Map.findWithDefault "" "yankArgs" settings
                      }

  -- And run brick
  let vtyBuilder = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- vtyBuilder

  void $ B.customMain initialVty vtyBuilder (Just chan) app st

  where
    -- | Get the local time
    getTime = do
      t <- Tm.getCurrentTime
      tz <- Tm.getCurrentTimeZone
      pure $ Tm.utcToLocalTime tz t


-- | Main even handler for brick events
handleEvent :: B.BrickEvent Name Event -> B.EventM Name BrickState ()
handleEvent ev =
  case ev of
    -- Handle keyboard events
    --   k is the key
    --   ms are the modifier keys
    (B.VtyEvent ve@(V.EvKey k ms)) ->
      case (k, ms) of
        -- Escape quits the app, no matter what control has focus
        (K.KEsc, []) -> B.halt

        _ -> do
          st' <- B.get
          -- How to interpret the key press depends on which control is focused
          case BF.focusGetCurrent $ st' ^. stFocus of
            Just TypeSearch ->
              case k of
                K.KChar '\t' -> do
                  -- Search, clear sort order, focus next
                  found <- liftIO $ doSearch st'
                  B.modify $ \st -> filterResults $ st & stFocus %~ BF.focusNext
                                                  & stResults .~ found
                                                  & stSortResults .~ SortNone

                K.KBackTab ->do
                  -- Search, clear sort order, focus prev
                  found <- liftIO $ doSearch st'
                  B.modify $ \st -> filterResults $ st & stFocus %~ BF.focusPrev
                                                   & stResults .~ found
                                                   & stSortResults .~ SortNone

                K.KEnter -> do
                  -- Search, clear sort order, focus on results
                  --  This makes it faster if you want to search and navigate results without tabing through the text search box
                  found <- liftIO $ doSearch st'
                  B.modify $ \st -> filterResults $ st & stResults .~ found
                                                  & stSortResults .~ SortNone
                                                  & stFocus %~ BF.focusNext & stFocus %~ BF.focusNext
                                                  -- TODO with brick >= 0.33, rather than 2x focus next: & stFocus %~ BF.focusSetCurrent ListResults

                _ -> do
                  -- Let the editor handle all other events
                  B.zoom stEditType $ BE.handleEditorEvent ev
                  st <- B.get
                  st2 <- liftIO $ searchAhead doSearch st
                  B.put st2


            Just TextSearch ->
              case k of
                K.KChar '\t' -> B.modify $ \st -> st & stFocus %~ BF.focusNext -- Focus next
                K.KBackTab -> B.modify $ \st -> st & stFocus %~ BF.focusPrev   -- Focus previous
                _ -> do
                  -- Let the editor handle all other events
                  B.zoom stEditText $ BE.handleEditorEvent ev
                  B.modify filterResults


            Just ListResults ->
              case k of
                K.KChar '\t' -> B.modify $ \st -> st & stFocus %~ BF.focusNext -- Focus next
                K.KBackTab -> B.modify $ \st -> st & stFocus %~ BF.focusPrev   -- Focus previous
                K.KChar 's' ->
                  -- Toggle the search order between ascending and descending, use asc if sort order was 'none'
                  let sortDir = if (st' ^. stSortResults) == SortAsc then SortDec else SortAsc in
                  let sorter = if sortDir == SortDec then Lst.sortBy (flip compareType) else Lst.sortBy compareType in
                  B.modify $ \st -> filterResults $ st & stResults %~ sorter
                                                     & stSortResults .~ sortDir
                K.KChar 'p' -> do
                  let selected = BL.listSelectedElement $ st' ^. stResultsList
                  B.suspendAndResume (yankPackage st' selected)
                K.KChar 'm' -> do
                  let selected = BL.listSelectedElement $ st' ^. stResultsList
                  B.suspendAndResume (yankModule st' selected)
                _ -> do
                  -- Let the list handle all other events
                  -- Using handleListEventVi which adds vi-style keybindings for navigation
                  --  and the standard handleListEvent as a fallback for all other events
                  B.zoom stResultsList $ BL.handleListEventVi BL.handleListEvent ve

            _ -> pass

    (B.AppEvent (EventUpdateTime time)) ->
      -- Update the time in the state
      B.modify $ \st -> st & stTime .~ time

    _ -> pass

  where
    doSearch :: BrickState -> IO [H.Target]
    doSearch st' =
      searchHoogle (st' ^. stDbPath) (Txt.strip . Txt.concat $ BE.getEditContents (st' ^. stEditType))


yank :: (H.Target -> Maybe Text) -> BrickState -> Maybe (Int, H.Target) -> IO BrickState
yank getText st selected =
  case getText <<$>> selected of
    Just (_, Just s') -> do
      let cmd = (st ^. yankCommand) <> " " <> (st ^. yankArgs)
      PT.runProcess_ $ PT.setStdin (PT.byteStringInput (BSL.fromStrict . TxtE.encodeUtf8 $ s')) (PT.shell (Txt.unpack cmd))
      pure st

    _ ->
      pure st


yankPackage :: BrickState -> Maybe (Int, H.Target) -> IO BrickState
yankPackage st selected =
  yank (\t -> Txt.pack . fst <$> H.targetPackage t) st selected


yankModule :: BrickState -> Maybe (Int, H.Target) -> IO BrickState
yankModule st selected =
  yank (\t -> Txt.pack . fst <$> H.targetModule t) st selected


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
        else filter (Txt.isInfixOf filterText . Txt.toLower . formatResult) allResults
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
      (B.withAttr (B.attrName "infoTitle") $ B.txt "Results: ") <+> B.txt (showing <> "/" <> total)
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
      B.withAttr (B.attrName "infoTitle") $
      B.txt t

    vtitle t =
      B.withAttr (B.attrName "infoTitle") $
      B.txt t

    editor n e =
      B.vLimit 1 $
      BE.renderEditor (B.txt . Txt.unlines) (BF.focusGetCurrent (st ^. stFocus) == Just n) e

    time t =
      B.padLeft (B.Pad 1) $
      B.hLimit 20 $
      B.withAttr (B.attrName "time") $
      B.str (Tm.formatTime Tm.defaultTimeLocale "%H-%M-%S" t)

    getSelectedDetail fn =
      case BL.listSelectedElement $ st ^. stResultsList of
        Nothing -> ""
        Just (_, e) -> fn e


-- | Reformat the text so that it can be wrapped nicely
reflow :: Text -> Text
reflow = Txt.replace "\n" " " . Txt.replace "\n\n" "\n" . Txt.replace "\0" "\n"


theMap :: BA.AttrMap
theMap = BA.attrMap V.defAttr [ (BE.editAttr           , V.black `B.on` V.cyan)
                              , (BE.editFocusedAttr    , V.black `B.on` V.yellow)
                              , (BL.listAttr           , V.white `B.on` V.blue)
                              , (BL.listSelectedAttr   , V.blue `B.on` V.white)
                              , (B.attrName "infoTitle", B.fg V.cyan)
                              , (B.attrName "time"     , B.fg V.yellow)
                              ]


getFiles :: FilePath -> IO [FilePath]
getFiles p = do
  entries <- (p </>) <<$>> Dir.listDirectory p
  filterM Dir.doesFileExist entries

----------------------------------------------------------------------------------------------
-- | Compare two hoogle results for sorting
compareType :: H.Target -> H.Target -> Ordering
compareType a b =
  compare (formatResult a) (formatResult b)


-- | Search hoogle using the default hoogle database
searchHoogle :: FilePath -> Text -> IO [H.Target]
searchHoogle path f =
  H.withDatabase path (\x -> pure $ H.searchDatabase x (Txt.unpack f))


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

----------------------------------------------------------------------------------------------------

loadSettings :: IO (Map Text Text)
loadSettings = do
  p <- getSettingsFilePath

  Dir.doesFileExist p >>= \case
    True -> do
      cfgLines1 <- Txt.lines . TxtE.decodeUtf8 <$> BS.readFile p
      let cfgLines2 = Txt.strip <$> cfgLines1
      let cfgLines3 = filter (not . Txt.isPrefixOf "#") cfgLines2
      let cfg1 = Txt.breakOn "=" <$> cfgLines3
      let cfg2 = filter (not . Txt.null . snd) cfg1
      let cfg3 = (\(k, v) -> (Txt.strip k, Txt.strip . Txt.drop 1 $ v)) <$> cfg2
      pure $ Map.fromList cfg3
    False ->
      pure Map.empty

saveSettings :: Map Text Text -> IO ()
saveSettings settings = do
  p <- getSettingsFilePath
  let ss = Txt.intercalate "\n" $ (\(k, v) -> k <> "=" <> v) <$> Map.toList settings
  BS.writeFile p . TxtE.encodeUtf8 $ ss


getSettingsFilePath :: IO FilePath
getSettingsFilePath = do
  p <- getSettingsRootPath
  pure $ p </> "bhoogle.conf"


getSettingsRootPath :: IO FilePath
getSettingsRootPath = do
  p <- Dir.getXdgDirectory Dir.XdgConfig "bhoogle"
  Dir.createDirectoryIfMissing True p
  pure p
