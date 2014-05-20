{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value.
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can use the conversions to 'Builder's when embedding JSON messages as
-- parts of a protocol.
module Data.Aeson.Encode
    ( encode

    -- * Encoding to Builders
    , encodeToByteStringBuilder
    ) where

import Data.Char (ord)
import Data.Aeson.Types (ToJSON(..))
import Data.Aeson.Types.Internal (JsonBuilder(..), IStream(..))
import Data.Aeson.Encode.ByteString (valueToByteStringBuilder)
import Data.Monoid (mempty, mappend)
import Data.Scientific (Scientific, coefficient, base10Exponent)
import Data.Word (Word8)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString.Builder.Scientific (scientificBuilder)
import Data.ByteString.Builder
import Data.ByteString.Builder.Prim ((>*<), (>$<))
import qualified Data.ByteString.Builder.Prim  as P

toBuilder :: JsonBuilder -> Builder
toBuilder (JsonBuilder g) = interpret (g IEnd)
  where
    interpret :: IStream -> Builder
    interpret is = case is of
      INull             is' -> nullB               <> interpret is'

      ITrue             is' -> trueB               <> interpret is'
      IFalse            is' -> falseB              <> interpret is'

      IDoubleQuote      is' -> char8          '"'  <> interpret is'

      IChar        c    is' -> char           c    <> interpret is'
      IString      cs   is' -> string         cs   <> interpret is'
      IText        t    is' -> text           t    <> interpret is'

      IInt         i    is' -> intDec         i    <> interpret is'
      IInt8        i8   is' -> int8Dec        i8   <> interpret is'
      IInt16       i16  is' -> int16Dec       i16  <> interpret is'
      IInt32       i32  is' -> int32Dec       i32  <> interpret is'
      IInt64       i64  is' -> int64Dec       i64  <> interpret is'

      IWord        w    is' -> wordDec        w    <> interpret is'
      IWord8       w8   is' -> word8Dec       w8   <> interpret is'
      IWord16      w16  is' -> word16Dec      w16  <> interpret is'
      IWord32      w32  is' -> word32Dec      w32  <> interpret is'
      IWord64      w64  is' -> word64Dec      w64  <> interpret is'

      IFloat       f    is' -> floatDec       f    <> interpret is'
      IDouble      d    is' -> doubleDec      d    <> interpret is'

      IInteger     i    is' -> integerDec     i    <> interpret is'
      IScientific  s    is' -> fromScientific s    <> interpret is'

      IComma            is' -> char8          ','  <> interpret is'

      IBeginArray       is' -> char8          '['  <> interpret is'
      IEndArray         is' -> char8          ']'  <> interpret is'

      IBeginObject      is' -> char8          '{'  <> interpret is'
      IEndObject        is' -> char8          '}'  <> interpret is'

      IColon            is' -> char8          ':'  <> interpret is'

      IValue       v    is' -> valueToByteStringBuilder v <> interpret is'

      -- Fused:
      IBeginObject_IDoubleQuote is' -> fixed2 ('{','"') <> interpret is'
      IComma_IDoubleQuote       is' -> fixed2 (',','"') <> interpret is'

      IEnd -> mempty

nullB :: Builder
nullB = fixed4 ('n',('u',('l','l')))
{-# INLINE nullB #-}

trueB :: Builder
trueB = fixed4 ('t',('r',('u','e')))
{-# INLINE trueB #-}

falseB :: Builder
falseB = fixed5 ('f',('a',('l',('s','e'))))
{-# INLINE falseB #-}

fixed2 :: (Char, Char) -> Builder
fixed2 = P.primFixed (P.char8 >*< P.char8)
{-# INLINE fixed2 #-}

fixed4 :: (Char, (Char, (Char, Char))) -> Builder
fixed4 = P.primFixed (P.char8 >*< P.char8 >*< P.char8 >*< P.char8)
{-# INLINE fixed4 #-}

fixed5 :: (Char, (Char, (Char, (Char, Char)))) -> Builder
fixed5 = P.primFixed (P.char8 >*< P.char8 >*< P.char8 >*< P.char8 >*< P.char8)
{-# INLINE fixed5 #-}

char :: Char -> Builder
char = P.primBounded escape
{-# INLINE char #-}

string :: String -> Builder
string = P.primMapListBounded escape
{-# INLINE string #-}

escape :: P.BoundedPrim Char
escape =
    P.condB (== '\\'  ) (bounded2 ('\\','\\')) $
    P.condB (== '\"'  ) (bounded2 ('\\','"' )) $
    P.condB (>= '\x20') P.charUtf8           $
    P.condB (== '\n'  ) (bounded2 ('\\','n' )) $
    P.condB (== '\r'  ) (bounded2 ('\\','r' )) $
    P.condB (== '\t'  ) (bounded2 ('\\','t' )) $
    P.condB isAngle     (P.liftFixedToBounded hexEscape) $
    (P.liftFixedToBounded hexEscape) -- fallback for chars < 0x20
  where
    isAngle c = c == '<' || c == '>'

    hexEscape :: P.FixedPrim Char
    hexEscape = (\c -> ('\\',('u', fromIntegral (ord c)))) >$<
              P.char8 >*< P.char8 >*< P.word16HexFixed

    bounded2 :: (Char, Char) -> P.BoundedPrim Char
    bounded2 tup = P.liftFixedToBounded $
      const tup >$< P.char8 >*< P.char8
    {-# INLINE bounded2 #-}

{-# INLINE text #-}
text :: T.Text -> Builder
text = TE.encodeUtf8BuilderEscaped escapeAscii

-- TODO: Merge with escape
escapeAscii :: P.BoundedPrim Word8
escapeAscii =
    P.condB (== c2w '\\'  ) (bounded2 ('\\','\\')) $
    P.condB (== c2w '\"'  ) (bounded2 ('\\','"' )) $
    P.condB (>= c2w '\x20') (P.liftFixedToBounded P.word8) $
    P.condB (== c2w '\n'  ) (bounded2 ('\\','n' )) $
    P.condB (== c2w '\r'  ) (bounded2 ('\\','r' )) $
    P.condB (== c2w '\t'  ) (bounded2 ('\\','t' )) $
    -- P.condB isAngle         (P.liftFixedToBounded hexEscape) $
    (P.liftFixedToBounded hexEscape) -- fallback for chars < 0x20
  where
    c2w = fromIntegral . ord

    -- isAngle c = c == '<' || c == '>'

    hexEscape :: P.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) >$<
        P.char8 >*< P.char8 >*< P.word16HexFixed

    bounded2 :: (Char, Char) -> P.BoundedPrim Word8
    bounded2 tup = P.liftFixedToBounded $
      const tup >$< P.char8 >*< P.char8
    {-# INLINE bounded2 #-}

fromScientific :: Scientific -> Builder
fromScientific s
    | e < 0     = scientificBuilder s
    | otherwise = integerDec (coefficient s * 10 ^ e)
  where
    e = base10Exponent s

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
encode :: ToJSON a => a -> L.ByteString
encode = toLazyByteString . encodeToByteStringBuilder
{-# INLINE encode #-}

encodeToByteStringBuilder :: ToJSON a => a -> Builder
encodeToByteStringBuilder = toBuilder . toJSON
{-# INLINE encodeToByteStringBuilder #-}

(<>) :: Builder -> Builder -> Builder
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
