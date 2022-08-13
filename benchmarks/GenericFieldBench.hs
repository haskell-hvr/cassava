{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

module GenericFieldBench
  ( genericFieldBench
  ) where

import Control.DeepSeq
import Criterion
import Data.Csv
import Data.Proxy
import Data.Typeable
import Generic.U2
import Generic.U4
import Generic.U8
import Generic.U16
import Generic.U32


genericFieldBench :: Benchmark
genericFieldBench = bgroup "genericField"
  [ bgroup "parseField: ok"
    [ mkParseSuccessBench (Proxy @U2) (Proxy @U2Generic) (Proxy @U2GenericStripPrefix)
    , mkParseSuccessBench (Proxy @U4) (Proxy @U4Generic) (Proxy @U4GenericStripPrefix)
    , mkParseSuccessBench (Proxy @U8) (Proxy @U8Generic) (Proxy @U8GenericStripPrefix)
    , mkParseSuccessBench (Proxy @U16) (Proxy @U16Generic) (Proxy @U16GenericStripPrefix)
    , mkParseSuccessBench (Proxy @U32) (Proxy @U32Generic) (Proxy @U32GenericStripPrefix)
    ]
  , bgroup "parseField: fail"
    [ mkParseFailBench (Proxy @U2) (Proxy @U2Generic) (Proxy @U2GenericStripPrefix)
    , mkParseFailBench (Proxy @U4) (Proxy @U4Generic) (Proxy @U4GenericStripPrefix)
    , mkParseFailBench (Proxy @U8) (Proxy @U8Generic) (Proxy @U8GenericStripPrefix)
    , mkParseFailBench (Proxy @U16) (Proxy @U16Generic) (Proxy @U16GenericStripPrefix)
    , mkParseFailBench (Proxy @U32) (Proxy @U32Generic) (Proxy @U32GenericStripPrefix)
    ]
  , bgroup "toField"
    [ mkToFieldBench (Proxy @U2) (Proxy @U2Generic) (Proxy @U2GenericStripPrefix)
    , mkToFieldBench (Proxy @U4) (Proxy @U4Generic) (Proxy @U4GenericStripPrefix)
    , mkToFieldBench (Proxy @U8) (Proxy @U8Generic) (Proxy @U8GenericStripPrefix)
    , mkToFieldBench (Proxy @U16) (Proxy @U16Generic) (Proxy @U16GenericStripPrefix)
    , mkToFieldBench (Proxy @U32) (Proxy @U32Generic) (Proxy @U32GenericStripPrefix)
    ]
  ]

type IsBench a = (Bounded a, Enum a, FromField a, ToField a, NFData a)

mkParseSuccessBench
  :: (IsBench a, Typeable a, IsBench generic, IsBench genericWithPrefix)
  => Proxy a
  -> Proxy generic
  -> Proxy genericWithPrefix
  -> Benchmark
mkParseSuccessBench px pxGen pxGenPfx = bgroup (show $ typeRep px)
  [ mkB "manual" px
  , mkB "generic" pxGen
  , mkB "generic with prefix" pxGenPfx
  ]
  where
    {-
      NB: this all is about sum representations.
      Manual instance tries to parse constructors from left to right,
      so parsing the string matching the first constructor is the best case,
      while parsing the last matcher is the worst case.
      Generic representation is, however, not that flat (one can check that by
      exploring 'Rep' of U32) and is more like a balanced binary tree with root
      being somewhere around U32_16 constructor (rough estimation).
      To level this discrepency and compare parsing efficiency more accurately
      we parse the whole range @[minBound..maxBound]@ of possible values for a type.
      This corresponds to the situation where data values are uniformly distributed.
    -}
    mkB
      :: (Bounded a, Enum a, FromField a, ToField a, NFData a)
      => String -> Proxy a -> Benchmark
    mkB name p = env (pure $ map toField $ genEnum p) $ bench name . nf (go p)
    go :: (FromField a) => Proxy a -> [Field] -> [a]
    go p = map $ ((\(Right x) -> x `asProxyTypeOf` p) . parse)

mkParseFailBench
  :: (IsBench a, Typeable a, IsBench generic, IsBench genericWithPrefix)
  => Proxy a
  -> Proxy generic
  -> Proxy genericWithPrefix
  -> Benchmark
mkParseFailBench px pxg pxgp = bgroup (show $ typeRep px)
  [ bench "manual" $ whnf (\s -> parse s `asProxyEither` px) mempty
  , bench "generic" $ whnf (\s -> parse s `asProxyEither` pxg) mempty
  , bench "generic with prefix" $ whnf (\s -> parse s `asProxyEither` pxgp) mempty
  ]

asProxyEither :: Either String a -> Proxy a -> Either String a
asProxyEither = const

mkToFieldBench
  :: (IsBench a, Typeable a, IsBench generic, IsBench genericWithPrefix)
  => Proxy a
  -> Proxy generic
  -> Proxy genericWithPrefix
  -> Benchmark
mkToFieldBench px pxg pxgp = bgroup (show $ typeRep px)
  [ mkB "manual" px
  , mkB "generic" pxg
  , mkB "generic with prefix" pxgp
  ]
  where
    mkB :: (Bounded a, Enum a, ToField a) => String -> Proxy a -> Benchmark
    mkB name = bench name . nf (map toField) . genEnum

parse :: (FromField a) => Field -> Either String a
parse = runParser . parseField

genEnum :: (Bounded a, Enum a) => Proxy a -> [a]
genEnum _ = [minBound..maxBound]
