module WordNet.DB where

import Data.List.NonEmpty (NonEmpty(..))
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.Ptr (nullPtr)
import Data.Text (Text, pack, unpack)
import Control.Exception (Exception, throw)
import Data.Dynamic (Typeable)

foreign import ccall "wn_init_wordnet" init_wordnet :: IO Bool
foreign import ccall "morphstr"        morphstr     :: CString -> Int -> IO CString

initialise :: IO Bool
initialise = init_wordnet

ensureInit :: IO ()
ensureInit = do
    isOpen <- initialise
    if not isOpen then
        throw CannotInitialise
    else
        pure ()

data POS = Any | Noun | Verb | Adj | Adv deriving (Show, Enum)

morph1 :: Text -> POS -> IO (NonEmpty Text)
morph1 w p = addFirst <$> morph w p
    where
        addFirst []     = w :| []
        addFirst (x:xs) = x :| xs

morph :: Text -> POS -> IO [Text]
morph w Any = concat <$> mapM (morph w) [Noun, Verb, Adj, Adv]
morph w pos = do
    s <- newCString $ unpack w
    results <- morph' s $ fromEnum pos
    mapM ((pack <$>) . peekCString) results

morph' :: CString -> Int -> IO [CString]
morph' w i = do
    _      <- ensureInit
    result <- morphstr w i
    if result == nullPtr then
        pure []
    else do
        rest <- morph' nullPtr i
        pure $ result : rest

data WordNetException
    = CannotInitialise
    deriving (Show, Typeable)

instance Exception WordNetException
