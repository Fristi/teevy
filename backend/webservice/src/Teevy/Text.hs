module Teevy.Text where

import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString as B

lazyTextToStrictByteString :: TL.Text -> B.ByteString
lazyTextToStrictByteString = TE.encodeUtf8 . TL.toStrict

strictByteStringToLazyText :: B.ByteString -> TL.Text
strictByteStringToLazyText = TL.fromStrict . TE.decodeUtf8