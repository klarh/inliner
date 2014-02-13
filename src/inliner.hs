{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Control.Applicative((<$>))
import Data.ByteString as B
import Data.ByteString.Lazy as BL
import Data.ByteString.Base64 as B64
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map.Strict as M
import Filesystem.Path.CurrentOS (fromText)
import Network.HTTP.Conduit (simpleHttp)
import Network.Mime (defaultMimeLookup)
import System.Console.CmdArgs
import System.FilePath.Posix (joinPath, takeDirectory)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TI
import System.IO (FilePath(..), IOMode(..), stdout, openFile, hClose)
import qualified Text.HTML.DOM as HTML
import Text.XML

data Options = Options {inFile :: String,
                        outFile :: String} deriving (Data, Typeable, Show)

defopts = Options {inFile = System.Console.CmdArgs.def &= typFile,
                   outFile = System.Console.CmdArgs.def}

localName name = Name name Nothing Nothing

toDataUri::T.Text->B.ByteString->T.Text
toDataUri mime inp = T.concat ["data:", mime, ";base64,", decodeUtf8 . B64.encode $ inp]

fetchPath::T.Text->T.Text->IO B.ByteString
fetchPath prefix path = do
  case "http://" `T.isPrefixOf` path of
    True -> B.concat . BL.toChunks <$> simpleHttp (T.unpack path)
    _ -> B.readFile (joinPath $ T.unpack <$> [prefix, path])

inlineDataUri::T.Text->Element->IO Element
inlineDataUri prefix elt = do
  let attrs = elementAttributes elt
      needsInline = M.member "src" attrs && M.notMember "noinline" attrs
  case needsInline of
    True -> do
      conts <- fetchPath prefix (attrs M.! "src")
      let mime = decodeUtf8 . defaultMimeLookup $ attrs M.! "src"
          newSrc = toDataUri mime conts
      return elt {elementAttributes = M.insert "src" newSrc attrs}
    _ -> return elt

inlineContents::T.Text->Element->IO Element
inlineContents prefix elt = do
  let attrs = elementAttributes elt
      needsInline = M.member "src" attrs && M.notMember "noinline" attrs
  case needsInline of
    True -> do
      conts <- decodeUtf8 <$> fetchPath prefix (attrs M.! "src")
      let newNode = NodeContent conts
          nodes = newNode:elementNodes elt
          attrs' = M.delete "src" attrs
      return elt {elementNodes = nodes, elementAttributes = attrs'}
    _ -> return elt

isLinkedStylesheet::Element->Bool
isLinkedStylesheet elt = name && href && rel && typ
  where
    attrs = elementAttributes elt
    name = (T.toLower . nameLocalName . elementName $ elt) == "link"
    href = "href" `M.member` attrs
    rel = M.findWithDefault "" "rel" attrs == "stylesheet"
    typ = M.findWithDefault "" "type" attrs == "text/css"

inlineLink::T.Text->Element->IO Element
inlineLink prefix elt = do
  let attrs = elementAttributes elt
  case isLinkedStylesheet elt of
    True -> do
      conts <- decodeUtf8 <$> fetchPath prefix (attrs M.! "href")
      let newNode = NodeContent conts
          newElt = Element "style" M.empty [newNode]
      return newElt
    _ -> return elt

inline::T.Text->Node->IO Node
inline prefix (NodeElement elt) = do
  self <- inline' prefix elt
  return (NodeElement self)
inline _ node = return node

inline'::T.Text->Element->IO Element
inline' prefix elt = do
  let name = nameLocalName . elementName $ elt

  case T.toLower name of
    "img" -> inlineDataUri prefix elt
    "script" -> inlineDataUri prefix elt
    "link" -> inlineLink prefix elt
    _ -> do
      children <- mapM (inline prefix) $ elementNodes elt
      return elt {elementNodes = children}

main = do
  (Options inFile outFile) <- cmdArgs defopts

  input <- HTML.readFile (fromText . T.pack $ inFile)
  root <- inline' (T.pack . takeDirectory $ inFile) (documentRoot input)
  let output = input {documentRoot = root}

  target <- case outFile of
    "" -> return stdout
    f -> openFile f WriteMode

  TI.hPutStr target (renderText Text.XML.def output)
  hClose target
