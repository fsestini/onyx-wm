{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Onyx.Core where

import MacOS

import Onyx.Prelude hiding (at)
import qualified ExceptExtra as E (throwError)
import Data.Map (Map)
import qualified Data.Map as M
import ExceptExtra (MonadError(..), ExceptT, logAndContinue,runExceptT)
import Data.Typeable (Typeable, cast)
import Utils
import Data.List.NonEmptyZipper
import Data.List.NonEmpty (NonEmpty(..))
import Lens.Micro.Platform

--------------------------------------------------------------------------------
-- Tiles

data TiledWindow = TW
  { _twWindow :: Window
  , _twFloatRect :: Rect
  }

twWid :: TiledWindow -> WindowID
twWid = windowID . _twWindow

data TileMode = OptIn [String] | OptOut [String]

--------------------------------------------------------------------------------
-- WM messages and layouts

class Typeable a => Message a
data SomeMessage = forall a . Message a => SomeMessage a

castMsg :: Message a => SomeMessage -> Maybe a
castMsg (SomeMessage m) = cast m

class Layout l where
  render :: Rect -> l -> [TiledWindow] -> [(Window, Rect)]
  handleMsg :: SomeMessage -> l -> [TiledWindow] -> Onyx (Maybe (l, [TiledWindow]))

data SomeLayout = forall l . Layout l => SL l

--------------------------------------------------------------------------------

data OnyxConfig = OnyxConfig
  { _wmcTileMode :: TileMode
  , _wmcPadding :: Double
  , _wmcLayouts :: NonEmpty SomeLayout
  }

data OnyxState = OnyxState
  { _wmsSpaceLayouts :: Map SpaceID (NonEmptyZipper SomeLayout)
  , _wmsTiled :: [TiledWindow]
  }

wmsTiled :: Lens' OnyxState [TiledWindow]
wmsTiled = lens _wmsTiled (\s tws -> s { _wmsTiled = tws })

wmsSpaceLayouts :: Lens' OnyxState (Map SpaceID (NonEmptyZipper SomeLayout))
wmsSpaceLayouts = lens _wmsSpaceLayouts (\s x -> s { _wmsSpaceLayouts = x })

wmcPadding :: Lens' OnyxConfig Double
wmcPadding = lens _wmcPadding (\c x -> c { _wmcPadding = x })

wmcLayouts :: Lens' OnyxConfig (NonEmpty SomeLayout)
wmcLayouts = lens _wmcLayouts (\c x -> c { _wmcLayouts = x })

wmcTileMode :: Lens' OnyxConfig TileMode
wmcTileMode = lens _wmcTileMode (\c x -> c { _wmcTileMode = x })

data WMError
  = WinNotManaged WindowID
  | NoDisplayIdx Int
  | UnknownSpace SpaceID
  | NoWindowsInSpace
  | OtherError
  | MacError MacError
  deriving (Show)

newtype Onyx a = Onyx { unOnyx :: ReaderT OnyxConfig (StateT OnyxState Mac) a }
  deriving ( Functor , Applicative , Monad
           , MonadReader OnyxConfig , MonadState OnyxState, MonadMac, MonadIO
           )

newtype EOnyx a = EOnyx { unEOnyx :: ExceptT WMError Onyx a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadMac,
            MonadReader OnyxConfig, MonadState OnyxState)

eToM :: EOnyx a -> Onyx (Maybe a)
eToM = fmap (either (const Nothing) Just) . runExceptT . unEOnyx

instance MonadError MacError EOnyx where
  throwError = EOnyx . E.throwError . MacError

instance MonadError WMError EOnyx where
  throwError = EOnyx . E.throwError

liftOnyx :: Onyx a -> EOnyx a
liftOnyx = EOnyx . lift

data Direction = DirLeft | DirRight | DirUp | DirDown | DirNext | DirPrev

data StdMessage = FocusTowards Window Direction
                | InsertNew TiledWindow
                | SwapTowards Window Direction
                deriving Typeable
instance Message StdMessage

runOnyx :: OnyxConfig -> Onyx a -> Mac a
runOnyx c m = fmap fst (runStateT (runReaderT (unOnyx m) c) (OnyxState M.empty []))

justLog :: EOnyx () -> Onyx ()
justLog = logAndContinue () . unEOnyx

--------------------------------------------------------------------------------

isWindowVisible :: Window -> EOnyx Bool
isWindowVisible w =
  not <$> liftM2 (||) (isWindowMinimized w) (isAppHidden (windowParent w))

isWindowTileable :: Window -> EOnyx Bool
isWindowTileable w =
  and <$> sequence [isWindowStandard w, isWindowMovable w, isWindowResizable w]

overWorkspace
  :: Space
  -> (forall l. Layout l => l -> [TiledWindow] -> EOnyx (Maybe (l, [TiledWindow])))
  -> EOnyx ()
overWorkspace sp f = withWorkspace sp $ \l tws -> do
  m <- f l tws
  flip maybeM m $ \(newL, newTws) -> do
    wmsSpaceLayouts . at (spcID sp) . _Just . current .= SL newL
    wmsTiled %= overSubstring ((`elem` (fmap twWid tws)) . twWid) (const newTws)

workspace :: Space -> EOnyx (SomeLayout, [TiledWindow])
workspace sp = do
  liftOnyx (checkS (spcID sp))
  allTiled <- use wmsTiled
  tiledInSpace <- liftIO (filterM (spaceHasWindow sp . _twWindow) allTiled)
  visibleTiles <- filterM (isWindowVisible . _twWindow) tiledInSpace
  mly <- preuse (wmsSpaceLayouts . at (spcID sp) . _Just . current)
  maybe (throwError (UnknownSpace (spcID sp))) (pure . (,visibleTiles)) mly

withWorkspace
  :: Space
  -> (forall l. Layout l => l -> [TiledWindow] -> EOnyx ())
  -> EOnyx ()
withWorkspace sp f = workspace sp >>= \(SL l, tws) -> f l tws

checkS' :: SpaceID -> NonEmpty SomeLayout -> Onyx ()
checkS' sid l =
  wmsSpaceLayouts . at sid %= Just . fromMaybe (fromNonEmpty l)

checkS :: SpaceID -> Onyx ()
checkS sid = view wmcLayouts >>= checkS' sid
