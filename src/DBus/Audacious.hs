{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
--------------------------------------------------
-- | 
-- Module: Dbus.Audacious
-- 
-- Dbus interface to Audacious (which does not conform exactly, or
-- solely, to the MPRIS standard). Note that currently only making
-- method calls is possible; listening for signals is not yet
-- available.
-- 
-- Note that 'player' and /all/ the functions in the 'Aud' monad may
-- raise exceptions. A specialized 'try' is provided in this module
-- which will only catch 'ConnectionError's, so that one may do
-- something like:
-- 
-- > try $ player >>= runAud ...
-- 
-- allowing other exceptions to propagate through.
-- 
-- The official dbus interface to audacious returns or accepts numeric
-- types sometimes as signed and sometimes as unsigned integers. It is
-- not always obvious /why/ that should be the case. (Several methods
-- differ solely in returning signed or unsigned integers.) This
-- module uniformly returns and accepts signed integers. The practical
-- effect of this should, generally, be nil: a playlist is unlikely to
-- have 2^31 entries in it, nor is a track likely be that many
-- milliseconds long, etc. On the other hand, audacious will return
-- 2^32 - 1 to indicate error or \"no response\" from methods like
-- QueueGetQueuePos (if, for instance, the queue has length 3, and one
-- asks for the position of the track at queue position 4). But, even
-- if it is practically never going to occur, it is /theoretically/
-- possibel that a queue should have that many entries.
-- 
-- All functions here return -1 to indicate such an error, where
-- relevant; such functions also have equivalents, ending in \"M\", that
-- call 'throwError' in the event of an error.
-- 
-- Two examples. To remove everything from the current playlist's
-- playqueue, opening the enqueued files in a new playlist instead,
-- and playing them there:
-- 
-- > p <- player
-- > withPlayer p $ do 
-- >     qps <- allQueuePos
-- >     fns <- mapM songFilename qps
-- >     mapM_ queueRemove qps
-- >     setShuffle False
-- >     openListToTemp fns
-- >     jump 0 -- last two lines not strictly necessary
-- >     play
-- 
-- To stop playing after the current track and all queued tracks have
-- played, doing this in a different thread:
-- 
-- > p <- player
-- > withPlayer p $ do
-- >    t <- (+) <$> remaining <*> (fmap sum (allQueuePos >>= mapM songFrames))
-- >    let micros = 1000 * fromIntegral t
-- >    liftIO $ forkIO (threadDelay micros >> withPlayer p stop) 
-- 
-- Note that the names of the functions exposed by this module do not
-- always directly correspond to the names of the methods exposed
-- over dbus, in large part because the latter seem to be highly
-- non-systematic. Some derived functions are included.
--------------------------------------------------
module DBus.Audacious
    (
     -- * access to the player
     player,

     -- * structures returned by queries
     PlayerStatus(..), PlayerInfo(..),

     -- * running the computations
     runAud, runAud', withPlayer, Aud,

     -- * toggles
     toggleShuffle, toggleStopAfter, toggleAutoAdvance, 

     -- * manipulating playback | 
     next, advance, unadvance, playPause, prev, pause, stop, play, seek, 

     -- * setting playback settings

     setRepeat, setShuffle, setVolume, setChannelVolume, 
     alterChannelVolume, alterVolume, setStopAfter, setAutoAdvance,
     

     -- * appearance/window visibility
     setAot, showMainWin, showPrefsBox, showAboutBox,
     showJtfBox, showFileBrowser,

     -- * query the playlist
     getMetadata, getMetadataString, getMetadataAtString, getMetadataAt, 
     getCurrentTrack, playlistLength, getActivePlaylistName,

     -- * query the player

     getStatus, getCaps, getVolume, getChannelVolume, mainWinVisible,
     playing, paused, stopped, repeating, autoAdvance, shuffle, stopAfter,
     info, status, time, balance, songFilename, songTitle, songFrames, 
     songLength, songTuple, remaining,

      -- * manipulate the tracklist

      addTrack, delTrack, jump, playlistAdd, add, addUrl,
      addList, openList, openListToTemp, clear, playlistInsUrlString,

      -- * Queue information/manipulation
       
      queueClear, queueAdd, queueRemove, queueLength,
      isQueued, queueGetQueuePos, queueGetListPos, 
      enqueueToTemp, allQueuePos,

      -- * Equalizer
      equalizerActivate, getEq, getEqPreamp, getEqBand, setEq, setEqPreamp, 
      setEqBand,

      -- * Global things

      audVersion, quit, eject,

      -- * wrapping exceptions
      try, tryS

     )
        where
            
import qualified Data.Map as M
import Network.URI
import Data.Maybe (fromJust, fromMaybe)
import Data.String
import Data.Int (Int32)
import Data.Word (Word32)
import System.IO
import Data.List (isPrefixOf)
import Control.Monad
import Control.Arrow ((***), (+++))
import Control.Category ((>>>))
import Control.Applicative
import qualified Control.Exception as E
import System.FilePath
import System.Directory
import Control.Monad.Reader
import Control.Monad.Error
import Numeric (readDec)
import DBus.Client.Simple hiding (throwError)
import DBus.Connection (ConnectionError)
import DBus.Types
import qualified DBus.Address
import System.Posix.Env (putEnv, getEnv)



audmain, mediaplayer :: InterfaceName
mediaplayer = fromString "org.freedesktop.MediaPlayer"
audmain = fromString "org.atheme.audacious"

data Player = Player {mainwin :: Proxy, playerobj :: Proxy, tracklist :: Proxy }

newtype AudR r a = AudR (ReaderT r IO a) deriving (MonadIO, Monad, Functor, Applicative)
type Aud = AudR Player
instance MonadReader r (AudR r) where -- can't be derived automatically
    ask = AudR $ ReaderT return
    local f (AudR m) = AudR $ ReaderT $ \r -> runReaderT m (f r)

-- The advantage of the above monad stack is that *within* Aud
-- computations, one needn't thread a Player object through; that is,
-- as in the examples above, one can just do "jump 0 >> play" rather
-- than "jump p 0 >> play p", which is ugly. However, if one isn't
-- going to call "player" frequently to get a fresh Player object, and
-- if one uses the Aud monad in many places, one will still have to
-- thread the player object around to pass it into
-- runAud/withPlayer. This can be made easier using another layer of
-- Reader (or State). However, an alternate approach would be to do
-- something like this:
-- 
-- thePlayer :: IORef (Maybe Player)
-- thePlayer = unsafePerformIO $ newIORef Nothing
-- getPlayer :: IO Player -- as "player" below
-- player :: IO Player
-- player = do
--   mp <- readIORef p
--   case mp of
--     Just pl -> return pl
--     Nothing -> do     
--             pl <- getPlayer
--             writeIORef p $ Just pl
--             return pl
-- 
-- then "ask" in realMakecall below would be replaced by "player".
-- It would then be possible to keep Aud as a distinct type, though
-- there would be no reason to have it be the type it presently is (it
-- could just be a newtype around IO), and to replace runAud with
-- something with the signature of runAud', since there would be no
-- reason to pass in the Player object, which is stashed in a global
-- IORef. Or Aud could just be replaced with IO directly.
-- While perhaps neater, though, I find the use of the global there
-- kind of icky.

-- | 'runAud' runs a computation with the provided 'Player', returning
-- the result in the IO monad.
runAud :: Aud a -> Player -> IO a
runAud (AudR m) = runReaderT m

-- | 'runAud'' runs the computation after getting a 'Player' object
-- by calling 'player'. This results in greater local network traffic
-- than stashing a 'Player' and using 'runAud' or 'withPlayer'.
runAud' :: Aud a -> IO a
runAud' a = player >>= runAud a

-- | 'withPlayer' is 'runAud' with the arguments reversed.
withPlayer :: Player -> Aud a -> IO a
withPlayer = flip runAud

idFile :: IO (Maybe FilePath)
idFile = do
  let varfn = "/var/lib/dbus/machine-id"
      etcfn = "/etc/machine-id"
  var <- doesFileExist varfn
  etc <- doesFileExist etcfn
  case (etc,var) of
    (True,_) -> return $ Just etcfn
    (_,True) -> return $ Just varfn
    otherwise -> return Nothing

ensureEnv :: IO ()
ensureEnv = do
  s <- DBus.Address.getSession
  case s of
    Just _ -> return ()
    Nothing ->  do
          x <- liftM (maybe "" (('-':).drop 1)) (getEnv "DISPLAY")
          idfn <- fromMaybe "/dev/null" <$> idFile
          id <- withFile idfn ReadMode hGetLine
          sessiondir <- fmap (</> ".dbus" </> "session-bus") getHomeDirectory
          e <- doesDirectoryExist sessiondir
          when e $ do
                  sessions <- getDirectoryContents sessiondir
                  forM_ sessions (\fn -> when ((id ++ x) `isPrefixOf` fn) $
                                         putEnvs (sessiondir </> fn))
    where 
      putEnvs :: FilePath -> IO ()
      putEnvs fn = do
          h <- openFile fn ReadMode
          contents <- fmap lines $ hGetContents h
          let varlines = filter ("DBUS_SESSION" `isPrefixOf`) contents
          mapM_ putEnv varlines
          hClose h

-- | 'player' returns a player structure, necessary for all other
-- operations. 
-- 
-- It will try to ensure that the environment is correctly configured;
-- if DBUS_SESSION_BUS_ADDRESS isn't set, it will look in
-- \/var\/lib\/dbus\/machine-id and ~\/.dbus\/session-bus to try to find the
-- correct values. If it fails, it will throw an exception.
player :: IO Player
player = do
  ensureEnv
  client <- connectSession
         --- XXX these shouldn't really be literals here.

  m <- proxy client (fromString "org.atheme.audacious") (fromString "/org/atheme/audacious")
  p <- proxy client (fromString "org.mpris.audacious") (fromString "/Player")
  t <- proxy client (fromString "org.mpris.audacious") (fromString "/TrackList")
  return $ Player m p t

-- omitted: PositionGet (equiv to Time)
data PlayerCall = Next | Prev | Pause | Stop | Play | VolumeGet 
                | Repeat | VolumeSet | PositionSet | GetStatus | GetCaps 
                | GetMetadata
                  deriving Show
-- omitted: GetLength (equivalent to Length)
data TracklistCall = GetCurrentTrack | GetMetadataAt  | AddTrack
                   | DelTrack | Loop | Random
                     deriving Show
-- omitted: Play, Pause, Stop (redundant); Position (equivalent to
-- GetCurrentTrack); Seek (equivalent to PositionSet---note that the
-- function exposed is, however, named "seek"); Delete (equivalent to
-- DelTrack)
data MainCall = Version | MainWinVisible | ShowMainWin | GetTupleFields 
              | Quit | Eject | Status | Info | Time | ChannelVolume
              | Playing | Paused | Stopped | SetChannelVolume 
              | Balance | Advance | Reverse | Length | SongTitle 
              | SongFilename | SongLength | SongFrames | StopAfter
              | Jump | SongTuple | Add | AddUrl | AddList | OpenList
              | OpenListToTemp  | Clear | AutoAdvance  
              | ToggleAutoAdvance | IsRepeat | Shuffle | ToggleShuffle
              | ToggleStopAfter | ShowPrefsBox | ShowAboutBox | ShowJtfBox 
              | ShowFileBrowser | PlayPause | QueueGetListPos 
              | GetPlayqueueLength | ToggleAot | PlaylistInsUrlString 
              | PlaylistAdd | PlayqueueAdd | PlayqueueRemove | PlayqueueClear
              | PlayqueueIsQueued | PlaylistEnqueueToTemp | GetEq | GetEqPreamp
              | GetEqBand | SetEq | SetEqPreamp | SetEqBand| QueueGetQueuePos
              | EqualizerActivate | GetActivePlaylistName
                deriving Show

data Call = PCall PlayerCall | TCall TracklistCall | MCall MainCall

class (Show a) => IsCall a where
    toCall :: a -> Call
    toMemberName :: a -> MemberName
    toMemberName = fromString . show
instance IsCall PlayerCall where
    toCall = PCall
instance IsCall TracklistCall where
    toCall = TCall
    -- redundant name, but different meaning
    toMemberName GetMetadataAt = fromString "GetMetadata"
    toMemberName x = fromString $ show x
instance IsCall MainCall where
    toCall = MCall
    -- redundant names, but different meanings
    toMemberName ChannelVolume = fromString "Volume"
    toMemberName SetChannelVolume = fromString "SetVolume"
    toMemberName IsRepeat = fromString "Repeat"
    toMemberName x = fromString $ show x

data PlayerStatus = PlayerPlaying | PlayerPaused | PlayerStopped 
                    deriving (Show, Eq)
data PlayerInfo = PlayerInfo { rate :: Int32, freq :: Int32, nch :: Int32 } 
                  deriving (Show, Eq)

--- some functions to make defining the calls nicer

docall :: Call -> Player -> MemberName -> [Variant] -> IO [Variant]
docall (PCall c) p s args = call (playerobj p) mediaplayer s args
docall (MCall c) p s args = call (mainwin p) audmain s args
docall (TCall c) p s args = call (tracklist p) mediaplayer s args

extract1 :: IsVariant a => [Variant] -> Aud a
extract1 = return.fromJust.fromVariant.(!!0)

extractmany :: IsVariant a => [Variant] -> Aud [a]
extractmany = return.foldr (\v acc -> fromJust (fromVariant v):acc) []

realMakecall :: IsCall c => (IO [Variant] -> IO b) -> c -> [Variant] -> Aud b
realMakecall f s args = do
  p <- ask
  liftIO $ f $ docall (toCall s) p (toMemberName s) args

makecall :: IsCall a => a -> [Variant] -> Aud [Variant]
makecall = realMakecall id

trycall :: IsCall a => a -> [Variant] -> Aud (Either ConnectionError [Variant])
trycall = realMakecall try


--- almost all actual calls defined in terms of callNoarg through send1get1.
callNoarg :: IsCall a => a -> Aud [Variant]
callNoarg s = makecall s []

ignoreResult :: (IsCall a) => a -> [Variant] -> Aud ()
-- the point-free version is too cute to resist.
ignoreResult = (void .) . makecall 

send :: (IsCall a) => a -> Aud ()
send = void . callNoarg 

send1 :: (IsVariant a, IsCall c) => c -> a -> Aud ()
send1 s arg = ignoreResult s [toVariant arg]
get1 :: (IsVariant a, IsCall c) => c -> Aud a
get1 s = callNoarg s >>= extract1

send1get1 :: (IsVariant i, IsVariant o, IsCall c) => c ->  i -> Aud o
send1get1 s arg = makecall s [toVariant arg] >>= extract1

--manipulating playback:
toggleShuffle :: Aud ()
-- | 'toggleShuffle' turns shuffling on or off.
toggleShuffle = send ToggleShuffle

toggleStopAfter  :: Aud ()
-- | 'toggleStopAfter' turns the stop-after-current-song feature on or off.
toggleStopAfter = send ToggleStopAfter

next  :: Aud ()
advance  :: Aud ()
-- | 'next' and 'advance' both move the current playlist to the next entry.
next = send Next
advance = send Advance

-- | 'prev' and 'unadvance' both move the current playlist to the
-- previous entry.
unadvance :: Aud ()
prev :: Aud ()
unadvance = send Reverse
prev = send Prev

-- | 'playPause' toggles the play/pause state
playPause :: Aud ()
playPause = send PlayPause

pause  :: Aud ()
-- | 'pause' pauses playback (but will not shift the player into being
-- paused if its current state is being stopped)
pause = send Pause

stop :: Aud ()
-- | 'stop' stops playback.
stop = send Stop

play :: Aud ()
-- | 'play' starts playback.
play = send Play

toggleAutoAdvance :: Aud ()
-- | 'toggleAutoAdvance' turns on or off the \"playlist advance\"
-- feature (turning it on is like setting \"stop after this song\" after
-- every song---you have to start playback explicitly each time, and
-- the playlist won't move to a new entry by itself).
toggleAutoAdvance = send ToggleAutoAdvance

-- | 'seek' seeks to the specified time in milliseconds:
-- 
-- > seek 4000
-- 
-- will move to the fourth second of the current track. (Seeking
-- beyond the length of the current track will advance the playlist.)
seek :: Int32 -> Aud ()
seek = send1 PositionSet

--settings
setShuffle :: Bool -> Aud ()
setRepeat  :: Bool -> Aud ()
-- | 'setRepeat' turns looping (going back to the beginning of the
-- playlist after reaching the end) on. You can  query
-- whether looping is enabled with 'repeating'.
-- to return 'False').
setRepeat = send1 Loop

setByToggle :: Aud Bool -> Aud () -> Bool -> Aud ()
setByToggle q toggle enable = q >>= \enabled -> when (enable /= enabled) toggle

setStopAfter :: Bool -> Aud ()
setStopAfter = setByToggle stopAfter toggleStopAfter

setAutoAdvance :: Bool -> Aud ()
setAutoAdvance = setByToggle autoAdvance toggleAutoAdvance

-- | 'setShuffle' turns shuffling (random playlist advance) on or
-- off. You can query whether shuffling is enabled with 'shuffle'.
setShuffle = send1 Random

-- | 'setChannelVolume' will set the left and right volume channels
-- independently.
setChannelVolume :: Int32 -> Int32 -> Aud ()
setChannelVolume leftvol rightvol = ignoreResult SetChannelVolume [toVariant leftvol, toVariant rightvol]

-- | 'setVolume' will set both channels to the provided volume
-- (0--100), /except if 'setChannelVolume' has already set the
-- channels to different values, in which case, in which case the
-- right 'setVolume' sets the right channel to the provided volume,
-- and the left channel to something else, not necessarily what you
-- want. 
setVolume :: Int32 -> Aud ()
setVolume = send1 VolumeSet

alterChannelVolume :: (Int32 -> Int32) -> (Int32 -> Int32) -> Aud (Int32, Int32)
-- | 'alterChannelVolume' changes the volume in each channel based on
-- their current values, returning the new values. The following, for
-- instance, will increment the left channel's volume by three, and
-- divide the right's by two:
-- 
-- > (newleft, newright) <- alterChannelVolume (+ 3) (`quot` 2)
alterChannelVolume lf rf = fmap (lf *** rf) getChannelVolume >>= uncurry setChannelVolume >> getChannelVolume

alterVolume :: (Int32 -> Int32) -> Aud (Int32, Int32)
-- | 'alterVolume' changes both channels by the same function,
-- returning the new volume in both channels.
alterVolume volf = alterChannelVolume volf volf

-- appearance/window visibility
setAot :: Bool -> Aud ()
showAboutBox :: Bool -> Aud ()
showFileBrowser :: Bool -> Aud ()
showPrefsBox :: Bool -> Aud ()
showJtfBox:: Bool -> Aud ()
showMainWin :: Bool -> Aud ()
-- | 'setAot' controls whether audacious is always on top. There does
-- not seem to be a way to programmatically find out its current state
-- without setting it.
setAot = send1 ToggleAot

-- | 'showMainWin' sets display of the main window.
showMainWin = send1 ShowMainWin
-- | 'showPrefsBox' sets display of the preferences pane.
showPrefsBox = send1 ShowPrefsBox
-- | 'showAboutBox' sets display of the about window.
showAboutBox = send1 ShowAboutBox
-- | 'showJtfBox' displays (or hides) the jump-to-file window.
showJtfBox = send1 ShowJtfBox
-- | 'showFileBrowser' displays (or hides) the file browswer.
showFileBrowser = send1 ShowFileBrowser

-- query the playlist
-- | 'getMetadata' returns a map of the metadata of the currently
-- playing track. The metadata is (conveniently) always either a
-- 'String' or an 'Int32', we can just use 'Either'.  An alternative,
-- 'getMetadataString', is available, which converts all values in the
-- map to 'String's.
getMetadata :: Aud (M.Map String (Either Int32 String))
getMetadata = fmap (M.map simpleVariantDecode) $ get1 GetMetadata

getMetadataString :: Aud (M.Map String String)
-- | Equivalent to 'getMetadata', but with integral values converted
-- to strings.
getMetadataString = fmap (M.map (either show id)) getMetadata

getCurrentTrack :: Aud Int32
-- | 'getCurrentTrack' returns the position in the playlist of the
-- current track as an 'Int32'.
getCurrentTrack = get1 GetCurrentTrack
getCurrentTrackM :: (MonadError String m) => Aud (m Int32)
getCurrentTrackM = do
  c <- getCurrentTrack
  return $ if c < 0 then throwError "getCurrentTrack" else return c

-- | 'getMetadataAt' gives the metadata of the track at the indicated
-- index. The following are equivalent:
-- 
-- > getCurrentTrack >>= getMetadataAt
-- 
-- and
-- 
-- > getMetadata
-- 
-- As with 'getMetadata', there is an alternative 'getMetadataAtString'.
getMetadataAt :: Int32 -> Aud (M.Map String (Either Int32 String))
getMetadataAt = fmap (M.map simpleVariantDecode) . send1get1 GetMetadataAt

getMetadataAtString :: Int32 -> Aud (M.Map String String)
getMetadataAtString = getMetadataAt >>> fmap (M.map $ either show id)

-- query the player
-- | 'getStatus' returns the player's status as a four-tuple of
-- 'Int32', with the following meanings:
-- 
-- * The first is 0, 1 or 2 indicating playing, paused, or stopped;
-- 
-- * The second is 0 or 1 indicating linear or random play;
-- 
-- * The third is 0 or 1 indicating that it will go to the next
-- element/stay on the current element (presumably to be controlled by
-- the currently ineffective 'setRepeat');
-- 
-- * The fourth is 0/1 according to whether the player will loop.
getStatus :: Aud (Int32, Int32, Int32, Int32)
getStatus = get1 GetStatus

-- | 'getCaps' returns the capabilities of the player. See
-- <http://xmms2.org/wiki/MPRIS#GetCaps>. 
getCaps :: Aud Int32
getCaps = get1 GetCaps

-- | 'getVolume' gives the volume in the right channel.
getVolume :: Aud Int32
getVolume = get1 VolumeGet

-- | 'getChannelVolume' gives the volume in the left and right channels.
getChannelVolume :: Aud (Int32, Int32)
getChannelVolume = do
  [leftvol, rightvol] <- callNoarg ChannelVolume >>= extractmany
  return (leftvol, rightvol)

-- | 'info' gives the rate, frequency, and number of channels
-- for the current track.
info :: Aud PlayerInfo
info = do
  [rate, freq, nch] <- callNoarg Info >>= extractmany
  return PlayerInfo {rate, freq, nch}

-- | 'getTupleFields' gives a list of fields that can (theoretically!)
-- be used with 'songTuple'. Note that not /every/ song will actually
-- be able to give a meaningful value for /every/ one of these fields.
getTupleFields :: Aud [String]
getTupleFields = get1 GetTupleFields

-- | 'status' gives the status of the player as a 'PlayerStatus'
status :: Aud PlayerStatus
status = do
  playerstatus <- (callNoarg Status >>= extract1) :: Aud String
  return $ case playerstatus of
-- Supposedly these are the only three responses audacious will ever
-- issue, so this is---supposedly---safe.
             "playing" -> PlayerPlaying
             "paused" -> PlayerPaused
             "stopped" -> PlayerStopped

-- | 'time' returns the position in playback of the current
-- track in milliseconds.
time :: Aud Int32
time = fmap w2i $ get1 Time

playlistLength :: Aud Int32
balance :: Aud Int32
-- | 'balance' gives the volume balance of the player.
balance = get1 Balance
-- | 'playlistLength' returns the number of entries in the current playlist.
playlistLength = get1 Length


songTitle :: Int32 -> Aud String
songFilename :: Int32 -> Aud String
-- | 'songTitle' gives the display title of the indicated track---that
-- is, how the track appears in the playlist, not the 'title' metadata
-- associated with the track.
songTitle = send1get1 SongTitle . i2w
-- | 'songFilename' gives the URI of the indicated track. (Thus
-- despite its name and the name of the DBus method, what it returns
-- is not suitable right away for path manipulation; it will be
-- escaped and prefixed with a URI scheme.)
songFilename = send1get1 SongFilename . i2w

songLength :: Int32 -> Aud Int32
songFrames :: Int32 -> Aud Int32
-- | 'songLength' returns the length of the indicated track in seconds.
songLength = send1get1 SongLength . i2w
-- | 'songFrames' returns the length of the indicated track in
-- milliseconds. 
songFrames = send1get1 SongFrames . i2w

remaining :: Aud Int32
-- | 'remaining' returns the number of milliseconds remaining in the
-- current track.
remaining = do
  p <- getCurrentTrackM
  case p of
    Left _ -> return 0
    Right pos -> (-) <$> songFrames pos <*> time

-- | 'songTuple' queries the song at the indicated index for the data
-- associated with the indicated string as a Variant:
-- 
-- >>> position >>= flip songTuple "bitrate"
-- Variant 224
-- 
-- It could also return a String wrapped in a Variant, or (NB!) raise
-- an exception ('ConnectionError') if the requested information is
-- not available. 'trySongTuple' wraps such an error in a 'Left' constructor.
songTuple ::  Int32 -> String -> Aud Variant
songTuple pos name = do 
  reply <- makecall SongTuple [toVariant $ i2w pos, toVariant name]
  return $ fromJust (fromVariant $ head reply)

trySongTuple :: Int32 -> String -> Aud (Either ConnectionError Variant)
trySongTuple pos name = fmap (id +++ (!!0)) $ trycall SongTuple [toVariant $ i2w pos, toVariant name]

stopAfter :: Aud Bool
shuffle :: Aud Bool
autoAdvance :: Aud Bool
repeating :: Aud Bool
stopped :: Aud Bool
paused :: Aud Bool
playing :: Aud Bool
mainWinVisible :: Aud Bool
    
-- | 'mainWinVisible' indicates whether the main window is visible.
mainWinVisible = get1 MainWinVisible
-- | 'playing' indicates whether the player is playing.
playing = get1 Playing
-- | 'paused' indicates whether the player is paused.
paused = get1 Paused
-- | 'stopped' indicates whether the player is stopped.
stopped = get1 Stopped
-- | 'repeating' currently seems to return 'False'.
repeating = get1 IsRepeat
-- | 'autoAdvance' indicates whether the playlist will advance
-- automatically (see also 'toggleAutoAdvance')
autoAdvance = get1 AutoAdvance
-- | 'shuffle' indicates whether the player is currently in shuffle
-- mode (see also 'setRandom').
shuffle = get1 Shuffle
-- | 'stopAfter' indicates whether the player will stop after the
-- current song.
stopAfter = get1 StopAfter

-- | 'getActivePlaylistName' returns the name of the currently active
-- playlist.
getActivePlaylistName :: Aud String
getActivePlaylistName = get1 GetActivePlaylistName


-- * manipulate the tracklist
-- | 'addTrack' will add a track to the end of the currently active
-- playlist, and, if its third parameter is 'True', start playing if
-- the player was currently paused. NB! It will not necessarily start
-- playing the currently added track!
-- 
-- 'add' is equivalent to 'addTrack' without the complication of the
-- boolean parameter. 
addTrack :: FilePath -> Bool -> Aud ()
addTrack fn startplaying = do
  fn' <- liftIO $ escFile fn
  ignoreResult AddTrack [toVariant fn', toVariant startplaying]

-- | 'delTrack' deletes the track at the given position.
delTrack :: Int32 -> Aud ()
delTrack = send1 DelTrack

jump :: Int32 -> Aud ()
-- | 'jump' moves the player to the indicated position, and also stops
-- the player.
jump = send1 Jump . i2w

-- | 'playlistAdd' and 'addUrl' (which may be equivalent?) add a URI
-- to the end of the playlist.
playlistAdd:: String -> Aud ()
playlistAdd = (liftIO.escFile) >=> send1 PlaylistAdd


add :: FilePath -> Aud ()
addUrl:: FilePath -> Aud ()
-- | 'addUrl': see 'playlistAdd'
addUrl = (liftIO.escFile) >=> send1 AddUrl
-- | 'add': see 'addTrack'
add fn = do
  f <- liftIO $ escFile fn 
  ignoreResult Add [toVariant f]
openListToTemp :: [FilePath] -> Aud ()
openList :: [FilePath] -> Aud ()
addList :: [FilePath] -> Aud ()
addMany :: (IsCall a) => a -> [FilePath] -> Aud ()
addMany c fns = do 
  fs <- mapM (liftIO.escFile) fns
  ignoreResult c [toVariant fs]
-- | 'addList' adds multiple files to the end of the playlist at once.
addList = addMany AddList
-- | 'openList' first clears the current playlist, then adds multiple
-- files to it, and then starts playing.
openList = addMany OpenList
-- | 'openListToTemp' works as does 'openList', except instead of
-- replacing the current playlist, it opens a new playlist.
openListToTemp = addMany OpenListToTemp

-- | 'playlistInsUrlString' inserts a URI into the current playlist at
-- the position indicated.
playlistInsUrlString :: String -> Int32 -> Aud ()
playlistInsUrlString f pos = liftIO (escFile f) >>= \uri -> ignoreResult PlaylistInsUrlString [toVariant uri, toVariant $ i2w pos]

clear :: Aud ()
-- | 'clear' erases the current playlist.
clear = send Clear

-- Equalizer
getEq ::  Aud (Double, [Double])
getEq = do
  reply <- callNoarg GetEq
  return (fromJust.fromVariant $ reply !! 0, fromJust.fromVariant $ reply !! 1)
getEqPreamp ::  Aud Double
getEqPreamp = get1 GetEqPreamp
getEqBand :: Int32 -> Aud Double
getEqBand = send1get1 GetEqBand
setEq :: Double -> [Double] -> Aud ()
setEq preamp bands = ignoreResult SetEq [toVariant preamp, toVariant bands]
setEqPreamp :: Double -> Aud ()
setEqPreamp = send1 SetEqPreamp
setEqBand :: Int32 -> Double -> Aud ()
setEqBand band value = ignoreResult SetEqBand [toVariant band, toVariant value]
equalizerActivate ::  Bool -> Aud ()
equalizerActivate = send1 EqualizerActivate

-- * queue info/manip
queueClear :: Aud ()
-- | 'queueClear' clears the queue of songs to be played.
queueClear = send PlayqueueClear
queueRemove ::  Int32 -> Aud ()
queueAdd :: Int32 -> Aud ()
-- | 'queueAdd' adds the track at the indicated position to the
-- queue to be played.
queueAdd = send1 PlayqueueAdd
-- | 'queueRemove' removes the track at the indicated position in
-- the playlist (not the indicated position in the queue!) from the
-- queue, if it was enqueued. If not, it has no effect.
queueRemove = send1 PlayqueueRemove
-- | 'queueLength' returns the number of tracks in the queue. 
queueLength :: Aud Int32
queueLength = get1 GetPlayqueueLength
-- | 'queueIsQueued' gives indicates whether the track at the
-- given position in the playlist is on the playqueue.
isQueued ::  Int32 -> Aud Bool
isQueued = send1get1 PlayqueueIsQueued
queueGetListPos :: Int32 -> Aud Int32
queueGetQueuePos :: Int32 -> Aud Int32
--q :: Int32 -> Aud Int32
--q = w2i . s
-- | 'queueGetListPos', called with argument /n/, gives the /playlist/
-- position for the song with /queue/ position /n/. If the queue has
-- fewer than /n/ entries, it returns -1.
queueGetListPos = fmap w2i . send1get1 QueueGetListPos . i2w
queueGetListPosM :: (MonadError String m) => Int32 -> Aud (m Int32)
queueGetListPosM i = do
  p <- queueGetListPos i
  return $ if p < 0 then throwError "queueGetListPos" else return p
-- | 'queueGetQueuePos', called with argument /n/, gives the /queue/
-- position for the song with /playlist/ position /n/. If the song at
-- playlist position /n/ isn't on the queue, it returns -1
queueGetQueuePos = fmap w2i . send1get1 QueueGetQueuePos . i2w
queueGetQueuePosM :: (MonadError String m) => Int32 -> Aud (m Int32)
queueGetQueuePosM i = do
  p <- queueGetQueuePos i
  return $ if p < 0 then throwError "queueGetQueuePos" else return p
                        
enqueueToTemp :: String -> Aud ()
-- | 'enqueueToTemp' seems to function equivalently to
-- 'openListToTemp', except: it expects a URI, it expects one of them,
-- and it doesn't start playing automatically.
enqueueToTemp = send1 PlaylistEnqueueToTemp

allQueuePos :: Aud [Int32]
-- | 'allQueuePos' gives the playlist positions of all enqueued
-- tracks.
allQueuePos = queueLength >>= \l -> forM [0 .. l -1 ] queueGetListPos

-- global things

quit :: Aud ()
eject :: Aud ()
-- | 'quit' causes the player to quit.
quit = send Quit
-- | 'eject' does not open the CD tray; it opens a file open dialogue.
eject = send Eject
audVersion :: Aud String
audVersion = get1 Version

--- utilities

-- in Aud because it needs to be made absolute.
-- This relies on FilePath ~ String.
escFile :: FilePath -> IO String
escFile f 
    | isAbsoluteURI f = return f
    | isAbsolute f = return $ uriEncode $ "file://"++f
    | otherwise = do 
  cwd <- liftIO getCurrentDirectory
  return $ uriEncode ("file://"++(cwd </> f))
                    
uriEncode = escapeURIString isUnescapedInURI

-- XXX May explode! Incomplete pattern match!
simpleVariantDecode :: Variant -> Either Int32 String
simpleVariantDecode v = case variantType v of
                          TypeString -> Right $ fromJust $ fromVariant v
                          TypeInt32 -> Left $ fromJust $ fromVariant v
                          TypeWord32 -> Left $ w2i $ fromJust $ fromVariant v

try :: IO a -> IO (Either ConnectionError a)
-- | 'try' runs the passed computation; if it raises a ConnectionError
-- the exception is wrapped in Left; otherwise, if it completes
-- successfully, the result is wrapped in Right.
try = E.try
tryS :: IO a -> IO (Either String a)
-- | 'tryS' is equivalent to 'try', but the value in Left will be a
-- string representation of the exception.
tryS action = fmap (show +++ id) $ try action

i2w :: Int32 -> Word32
-- | convert from an 'Int32' to a 'Word32'. Only one of the
-- 'Int32'-returning DBus methods exposed here returns a negative
-- number: if the currently active playlist is empty,
-- 'getCurrentTrack' will return -1. So mostly this should be
-- perfectly safe. If a negative number is passed in, 'maxBound ::
-- Word32', a value no signed int could have, is returned.
i2w i 
    | i < 0 = maxBound
    | otherwise = fromIntegral i

w2i :: Word32 -> Int32
-- | Convert from a 'Word32' to an 'Int32'. If the word is too long to
-- fit in an int, a negative number is returned to signal error.
w2i w = let i = fromIntegral w in
        if i < 0 then -1 else i
