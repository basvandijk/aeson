{-# LANGUAGE CPP #-}

-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011, 2012 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
      Value(..)
    , Array
    , Object
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    -- * Type conversion
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , modifyFailure
    , ToJSON(..)
    , JsonBuilder

#ifdef GENERICS
    -- ** Generic JSON classes
    , GFromJSON(..)
    , GToJSON(..)
    , genericToJSON
    , genericParseJSON
#endif

    -- * Inspecting @'Value's@
    , withObject
    , withText
    , withArray
    , withNumber
    , withScientific
    , withBool

    -- * Constructors
    , jsonNull

      -- ** Booleans
    , jsonTrue
    , jsonFalse

    -- ** Arrays
    , emptyArray
    , singletonArray
    , many1Array
    , CommaPrefixedElements
    , element

      -- ** Objects
    , emptyObject
    , singletonObject
    , many1Object
    , JsonFirstProperty, jsonFirstProperty
    , CommaPrefixedProperties, commaPrefixedProperty
    , Property, (.=)
    , Pair
    , object

    -- * Accessors
    , (.:)
    , (.:?)
    , (.!=)

    -- * Generic and TH encoding configuration
    , Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject
    ) where

import Data.Aeson.Types.Instances
import Data.Aeson.Types.Internal

#ifdef GENERICS
import Data.Aeson.Types.Generic ()
#endif
