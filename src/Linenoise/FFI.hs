{-# LANGUAGE ForeignFunctionInterface #-}

-- | Just an FFI layer over the C library.
module Linenoise.FFI
  ( addHistory
  , clearScreen
  , getInputLine
  , historyLoad
  , historySave
  , printKeycodes
  , setCompletion
  , setMultiline
  , stifleHistory
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BSU
import Data.Foldable (forM_)
import Foreign
import Foreign.C.String
import Foreign.C.Types (CChar, CInt (..), CSize)

foreign import ccall "linenoise.h linenoise"
  linenoise :: CString -> IO CString
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
  forM_ comps (`BSU.unsafeUseAsCString` linenoiseAddCompletion lc)

-- | Run the prompt, yielding a string.
getInputLine :: ByteString -> IO (Maybe ByteString)
getInputLine =
  flip BSU.unsafeUseAsCString $ \str -> do
    ptr <- linenoise str
    maybePeek BSU.unsafePackCString ptr

-- | Add to current history.
addHistory :: ByteString -> IO ()
addHistory =
  flip BSU.unsafeUseAsCString $ \str -> do
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
