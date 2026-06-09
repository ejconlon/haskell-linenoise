{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Just an FFI layer over the C library.
module Linenoise.FFI
  ( EditResult (..)
  , EditSession
  , InputResult (..)
  , addHistory
  , clearScreen
  , feedEditSession
  , freeEditSession
  , getInputLine
  , hideEditSession
  , historyLoad
  , historySave
  , printKeycodes
  , setCompletion
  , setMultiline
  , showEditSession
  , startEditSession
  , stopEditSession
  , stifleHistory
  , withEditSession
  , withHiddenEditSession
  )
where

import Control.Exception (bracket, bracket_)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign (FunPtr, Ptr, Storable (..), fromBool, nullPtr)
import Foreign.C.Error (eAGAIN, eNOENT, getErrno, resetErrno)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CChar, CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)

-- | Opaque C @struct linenoiseState@ pointer target.
data LinenoiseState

-- | Active non-blocking linenoise edit session.
--
-- The session owns the C edit state, input buffer, and copied prompt. Prefer
-- 'withEditSession' so raw terminal mode is stopped and memory is released.
data EditSession = EditSession
  { editSessionState :: !(Ptr LinenoiseState)
  , editSessionBuffer :: !(Ptr CChar)
  , editSessionPrompt :: !CString
  , editSessionStopped :: !(IORef Bool)
  }

foreign import ccall "linenoise.h linenoise"
  linenoise :: CString -> IO CString

foreign import ccall "linenoise.h linenoiseFree"
  linenoiseFree :: CString -> IO ()

foreign import ccall "linenoise.h linenoiseEditStart"
  linenoiseEditStart :: Ptr LinenoiseState -> CInt -> CInt -> Ptr CChar -> CSize -> CString -> IO CInt

foreign import ccall "linenoise.h linenoiseEditFeed"
  linenoiseEditFeed :: Ptr LinenoiseState -> IO CString

foreign import ccall "linenoise.h linenoiseEditStop"
  linenoiseEditStop :: Ptr LinenoiseState -> IO ()

foreign import ccall "linenoise.h linenoiseHide"
  linenoiseHide :: Ptr LinenoiseState -> IO ()

foreign import ccall "linenoise.h linenoiseShow"
  linenoiseShow :: Ptr LinenoiseState -> IO ()

foreign import ccall "linenoise.h &linenoiseEditMore"
  linenoiseEditMorePtr :: Ptr CString

foreign import ccall "linenoise.h linenoiseHistoryAdd"
  linenoiseHistoryAdd :: Ptr CChar -> IO CInt

foreign import ccall "linenoise.h linenoiseHistorySetMaxLen"
  linenoiseHistorySetMaxLen :: CInt -> IO CInt

foreign import ccall "linenoise.h linenoiseHistorySave"
  linenoiseHistorySave :: CString -> IO ()

foreign import ccall "linenoise.h linenoiseHistoryLoad"
  linenoiseHistoryLoad :: CString -> IO ()

foreign import ccall "linenoise.h linenoiseClearScreen"
  linenoiseClearScreen :: IO ()

foreign import ccall "linenoise.h linenoiseSetMultiLine"
  linenoiseSetMultiLine :: CInt -> IO ()

foreign import ccall "linenoise.h linenoisePrintKeyCodes"
  linenoisePrintKeyCodes :: IO ()

foreign import ccall "linenoise.h linenoiseSetCompletionCallback"
  linenoiseSetCompletionCallback :: FunPtr CompleteFunc -> IO ()

foreign import ccall "linenoise.h linenoiseAddCompletion"
  linenoiseAddCompletion :: Completion -> CString -> IO ()

foreign import ccall "wrapper"
  makeFunPtr :: CompleteFunc -> IO (FunPtr CompleteFunc)

data CompletionType = CompletionType CSize (Ptr (Ptr CChar))
  deriving (Show, Eq)

type Completion = Ptr CompletionType

instance Storable CompletionType where
  sizeOf _ = 8
  alignment = sizeOf
  peek ptr = do
    a <- peekByteOff ptr 0
    b <- peekByteOff ptr 4
    pure (CompletionType a b)
  poke = error "no poke"

-- Completion C callback
type CompleteFunc = (CString -> Completion -> IO ())

-- Make a completion function pointer.
makeCompletion :: (ByteString -> IO [ByteString]) -> (CString -> Completion -> IO ())
makeCompletion f buf lc = do
  line <- BSU.unsafePackCString buf
  comps <- f line
  for_ comps (\c -> unless (BS.null c) (BSU.unsafeUseAsCString c (linenoiseAddCompletion lc)))

-- | Result of getInputLine.
data InputResult a
  = -- | ctrl+c
    InterruptResult
  | -- | ctrl+d
    EofResult
  | LineResult !a -- Possibly empty line.
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Result of feeding one input event to a non-blocking edit session.
data EditResult a
  = -- | Editing is still in progress; feed more input.
    MoreResult
  | -- | Editing finished with interrupt, EOF, or a completed line.
    DoneResult !(InputResult a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | Run the prompt, yielding a string.
getInputLine :: ByteString -> IO (InputResult ByteString)
getInputLine prompt = do
  ptr <- BSU.unsafeUseAsCString prompt linenoise
  if ptr == nullPtr
    then do
      errno <- getErrno
      if errno == eAGAIN
        then resetErrno >> pure InterruptResult
        else pure EofResult
    else do
      line <- BS.packCString ptr
      linenoiseFree ptr
      pure (LineResult line)

-- | Start a non-blocking edit session with the given prompt.
--
-- The returned session must eventually be passed to 'stopEditSession' and
-- 'freeEditSession'. Prefer 'withEditSession' for scoped use.
startEditSession :: ByteString -> IO EditSession
startEditSession prompt = do
  state <- mallocBytes linenoiseStateBytes
  buffer <- mallocBytes editBufferBytes
  prompt' <- copyByteString prompt
  stopped <- newIORef False
  result <- linenoiseEditStart state (-1) (-1) buffer (fromIntegral editBufferBytes) prompt'
  if result == -1
    then do
      free state
      free buffer
      free prompt'
      fail "linenoiseEditStart failed"
    else
      pure
        EditSession
          { editSessionState = state
          , editSessionBuffer = buffer
          , editSessionPrompt = prompt'
          , editSessionStopped = stopped
          }

-- | Stop editing and restore normal terminal mode.
--
-- This operation is idempotent for a given 'EditSession'.
stopEditSession :: EditSession -> IO ()
stopEditSession EditSession {editSessionState = state, editSessionStopped = stopped} = do
  alreadyStopped <- readIORef stopped
  unless alreadyStopped $ do
    linenoiseEditStop state
    writeIORef stopped True

-- | Free memory owned by an edit session.
--
-- Call this only after 'stopEditSession'. Prefer 'withEditSession'.
freeEditSession :: EditSession -> IO ()
freeEditSession EditSession {editSessionState = state, editSessionBuffer = buffer, editSessionPrompt = prompt} = do
  free state
  free buffer
  free prompt

-- | Run an action with a managed non-blocking edit session.
--
-- The session is stopped and freed when the action exits, including on
-- exceptions.
withEditSession :: ByteString -> (EditSession -> IO a) -> IO a
withEditSession prompt = bracket (startEditSession prompt) release
 where
  release session = stopEditSession session >> freeEditSession session

-- | Feed one input event to a non-blocking edit session.
--
-- Returns 'MoreResult' until the user completes a line, interrupts, or sends
-- EOF.
feedEditSession :: EditSession -> IO (EditResult ByteString)
feedEditSession EditSession {editSessionState = state} = do
  ptr <- linenoiseEditFeed state
  more <- peek linenoiseEditMorePtr
  if ptr == more
    then pure MoreResult
    else
      if ptr == nullPtr
        then do
          errno <- getErrno
          if errno == eAGAIN
            then resetErrno >> pure (DoneResult InterruptResult)
            else
              if errno == eNOENT
                then resetErrno >> pure (DoneResult EofResult)
                else pure (DoneResult EofResult)
        else do
          line <- BS.packCString ptr
          linenoiseFree ptr
          pure (DoneResult (LineResult line))

-- | Hide the current prompt and edit buffer.
--
-- Use this before writing asynchronous output to the terminal.
hideEditSession :: EditSession -> IO ()
hideEditSession = linenoiseHide . editSessionState

-- | Redraw the current prompt and edit buffer.
--
-- Use this after writing asynchronous output to the terminal.
showEditSession :: EditSession -> IO ()
showEditSession = linenoiseShow . editSessionState

-- | Run an action while the current prompt and edit buffer are hidden.
--
-- This is the primitive for prompt-safe asynchronous terminal output. Callers
-- that can write concurrently should still serialize access with a lock.
withHiddenEditSession :: EditSession -> IO a -> IO a
withHiddenEditSession session = bracket_ (hideEditSession session) (showEditSession session)

-- | Add to current history.
addHistory :: ByteString -> IO ()
addHistory bs =
  unless (BS.null bs) $ BSU.unsafeUseAsCString bs $ \str -> do
    _ <- linenoiseHistoryAdd str
    pure ()

-- | Limit the maximum history length.
stifleHistory :: Int -> IO ()
stifleHistory len = do
  _ <- linenoiseHistorySetMaxLen $ fromIntegral len
  pure ()

-- | Save history to a file.
historySave :: FilePath -> IO ()
historySave fname = do
  str <- newCString fname
  linenoiseHistorySave str

-- | Load history from a file.
historyLoad :: FilePath -> IO ()
historyLoad fname = do
  str <- newCString fname
  linenoiseHistoryLoad str

-- | Clear the screen.
clearScreen :: IO ()
clearScreen = linenoiseClearScreen

-- | Enable/Disable multiline input.
setMultiline :: Bool -> IO ()
setMultiline = linenoiseSetMultiLine . fromBool

-- | Print keycodes.
printKeycodes :: IO ()
printKeycodes = linenoisePrintKeyCodes

-- | Set the current completion function.
setCompletion :: (ByteString -> IO [ByteString]) -> IO ()
setCompletion f = do
  cb <- makeFunPtr (makeCompletion f)
  linenoiseSetCompletionCallback cb

copyByteString :: ByteString -> IO CString
copyByteString bs =
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    out <- mallocBytes (len + 1)
    copyBytes out ptr len
    pokeByteOff out len (0 :: CChar)
    pure out

linenoiseStateBytes :: Int
linenoiseStateBytes = 4096

editBufferBytes :: Int
editBufferBytes = 1024 * 1024 + 1
