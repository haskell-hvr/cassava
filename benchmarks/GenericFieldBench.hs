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
import Generic.Either
import Generic.U2
import Generic.U4
import Generic.U8
import Generic.U16
import Generic.U32


genericFieldBench :: Benchmark
genericFieldBench = bgroup "genericField"
  [ bgroup "parseField:ok"
    [ mkParseSuccessBench (genRange @U2)
    , mkParseSuccessBench (genRange @U2Generic)
    , mkParseSuccessBench (genRange @U2GenericStripPrefix)
    , mkParseSuccessBench (genRange @U4)
    , mkParseSuccessBench (genRange @U4Generic)
    , mkParseSuccessBench (genRange @U4GenericStripPrefix)
    , mkParseSuccessBench (genRange @U8)
    , mkParseSuccessBench (genRange @U8Generic)
    , mkParseSuccessBench (genRange @U8GenericStripPrefix)
    , mkParseSuccessBench (genRange @U16)
    , mkParseSuccessBench (genRange @U16Generic)
    , mkParseSuccessBench (genRange @U16GenericStripPrefix)
    , mkParseSuccessBench (genRange @U32)
    , mkParseSuccessBench (genRange @U32Generic)
    , mkParseSuccessBench (genRange @U32GenericStripPrefix)
    , mkParseSuccessBench manualEither0
    , mkParseSuccessBench genericEither0
    , mkParseSuccessBench manualEither1
    , mkParseSuccessBench genericEither1
    , mkParseSuccessBench manualEither2
    , mkParseSuccessBench genericEither2
    , mkParseSuccessBench manualEither3
    , mkParseSuccessBench genericEither3
    ]
  , bgroup "parseField:fail"
    [ mkParseFailBench (Proxy @U2)
    , mkParseFailBench (Proxy @U2Generic)
    , mkParseFailBench (Proxy @U2GenericStripPrefix)
    , mkParseFailBench (Proxy @U4)
    , mkParseFailBench (Proxy @U4Generic)
    , mkParseFailBench (Proxy @U4GenericStripPrefix)
    , mkParseFailBench (Proxy @U8)
    , mkParseFailBench (Proxy @U8Generic)
    , mkParseFailBench (Proxy @U8GenericStripPrefix)
    , mkParseFailBench (Proxy @U16)
    , mkParseFailBench (Proxy @U16Generic)
    , mkParseFailBench (Proxy @U16GenericStripPrefix)
    , mkParseFailBench (Proxy @U32)
    , mkParseFailBench (Proxy @U32Generic)
    , mkParseFailBench (Proxy @U32GenericStripPrefix)
    , mkParseFailBench (Proxy @ManualEither0)
    , mkParseFailBench (Proxy @GenericEither0)
    , mkParseFailBench (Proxy @ManualEither1)
    , mkParseFailBench (Proxy @GenericEither1)
    , mkParseFailBench (Proxy @ManualEither2)
    , mkParseFailBench (Proxy @GenericEither2)
    , mkParseFailBench (Proxy @ManualEither3)
    , mkParseFailBench (Proxy @GenericEither3)
    ]
  , bgroup "toField"
    [ mkToFieldBench (genRange @U2)
    , mkToFieldBench (genRange @U2Generic)
    , mkToFieldBench (genRange @U2GenericStripPrefix)
    , mkToFieldBench (genRange @U4)
    , mkToFieldBench (genRange @U4Generic)
    , mkToFieldBench (genRange @U4GenericStripPrefix)
    , mkToFieldBench (genRange @U8)
    , mkToFieldBench (genRange @U8Generic)
    , mkToFieldBench (genRange @U8GenericStripPrefix)
    , mkToFieldBench (genRange @U16)
    , mkToFieldBench (genRange @U16Generic)
    , mkToFieldBench (genRange @U16GenericStripPrefix)
    , mkToFieldBench (genRange @U32)
    , mkToFieldBench (genRange @U32Generic)
    , mkToFieldBench (genRange @U32GenericStripPrefix)
    , mkToFieldBench manualEither0
    , mkToFieldBench genericEither0
    , mkToFieldBench manualEither1
    , mkToFieldBench genericEither1
    , mkToFieldBench manualEither2
    , mkToFieldBench genericEither2
    , mkToFieldBench manualEither3
    , mkToFieldBench genericEither3
    ]
  ]

type IsBench a = (FromField a, ToField a, NFData a, Typeable a)

{-
  Manual instance tries to parse constructors from left to right,
  so parsing the string matching the first constructor is the best case,
  while parsing the last matcher is the worst case.
  Generic representation is, however, not that flat (one can check that by
  exploring 'Rep' of U32) and is more like a balanced binary tree with root
  being somewhere around U32_16 constructor (rough estimation).
  To level this discrepency and compare parsing efficiency more accurately
  we parse some range (@[minBound..maxBound]@ for enum) of possible values for a type.
  This corresponds to the situation where data values are uniformly distributed.
-}
mkParseSuccessBench :: (IsBench a) => [a] -> Benchmark
mkParseSuccessBench xs = env (pure $ map toField xs) $
  bench (show $ typeRep xs) . nf (map $ (\(Right x) -> x `asProxyTypeOf` xs) . parse)

mkParseFailBench :: (IsBench a) => Proxy a -> Benchmark
mkParseFailBench px = bench (show $ typeRep px) $
  nf (\s -> parse s `asProxyEither` px) mempty
  where
    asProxyEither :: Either String a -> Proxy a -> Either String a
    asProxyEither x _ = x

mkToFieldBench :: (IsBench a) => [a] -> Benchmark
mkToFieldBench xs = env (pure xs) $ bench (show $ typeRep xs) . nf (map toField)

parse :: (FromField a) => Field -> Either String a
parse = runParser . parseField

genRange :: (Bounded a, Enum a) => [a]
genRange = take 32 $ cycle [minBound..maxBound]

manualEither0 :: [ManualEither0]
manualEither0 = take 32 $ cycle
  [ LManual 1
  , RManual '!'
  ]

genericEither0 :: [GenericEither0]
genericEither0 = take 32 $ cycle
 [ LGeneric 1
 , RGeneric '!'
 ]

manualEither1 :: [ManualEither1]
manualEither1 = take 32 $ cycle
  [ LManual $ LManual 1
  , LManual $ RManual '!'
  , RManual $ LManual 1
  , RManual $ RManual '!'
  ]

genericEither1 :: [GenericEither1]
genericEither1 = take 32 $ cycle
  [ LGeneric $ LGeneric 1
  , LGeneric $ RGeneric '!'
  , RGeneric $ LGeneric 1
  , RGeneric $ RGeneric '!'
  ]

manualEither2 :: [ManualEither2]
manualEither2 = take 32 $ cycle
  [ LManual $ LManual $ LManual 1
  , LManual $ LManual $ RManual '!'
  , LManual $ RManual $ LManual 1
  , LManual $ RManual $ RManual '!'
  , RManual $ LManual $ LManual 1
  , RManual $ LManual $ RManual '!'
  , RManual $ RManual $ LManual 1
  , RManual $ RManual $ RManual '!'
  ]

genericEither2 :: [GenericEither2]
genericEither2 = take 32 $ cycle
  [ LGeneric $ LGeneric $ LGeneric 1
  , LGeneric $ LGeneric $ RGeneric '!'
  , LGeneric $ RGeneric $ LGeneric 1
  , LGeneric $ RGeneric $ RGeneric '!'
  , RGeneric $ LGeneric $ LGeneric 1
  , RGeneric $ LGeneric $ RGeneric '!'
  , RGeneric $ RGeneric $ LGeneric 1
  , RGeneric $ RGeneric $ RGeneric '!'
  ]

manualEither3 :: [ManualEither3]
manualEither3 = take 32 $ cycle
  [ LManual $ LManual $ LManual $ LManual 1
  , LManual $ LManual $ LManual $ RManual '!'
  , LManual $ LManual $ RManual $ LManual 1
  , LManual $ LManual $ RManual $ RManual '!'
  , LManual $ RManual $ LManual $ LManual 1
  , LManual $ RManual $ LManual $ RManual '!'
  , LManual $ RManual $ RManual $ LManual 1
  , LManual $ RManual $ RManual $ RManual '!'
  , RManual $ LManual $ LManual $ LManual 1
  , RManual $ LManual $ LManual $ RManual '!'
  , RManual $ LManual $ RManual $ LManual 1
  , RManual $ LManual $ RManual $ RManual '!'
  , RManual $ RManual $ LManual $ LManual 1
  , RManual $ RManual $ LManual $ RManual '!'
  , RManual $ RManual $ RManual $ LManual 1
  , RManual $ RManual $ RManual $ RManual '!'
  ]

genericEither3 :: [GenericEither3]
genericEither3 = take 32 $ cycle
  [ LGeneric $ LGeneric $ LGeneric $ LGeneric 1
  , LGeneric $ LGeneric $ LGeneric $ RGeneric '!'
  , LGeneric $ LGeneric $ RGeneric $ LGeneric 1
  , LGeneric $ LGeneric $ RGeneric $ RGeneric '!'
  , LGeneric $ RGeneric $ LGeneric $ LGeneric 1
  , LGeneric $ RGeneric $ LGeneric $ RGeneric '!'
  , LGeneric $ RGeneric $ RGeneric $ LGeneric 1
  , LGeneric $ RGeneric $ RGeneric $ RGeneric '!'
  , RGeneric $ LGeneric $ LGeneric $ LGeneric 1
  , RGeneric $ LGeneric $ LGeneric $ RGeneric '!'
  , RGeneric $ LGeneric $ RGeneric $ LGeneric 1
  , RGeneric $ LGeneric $ RGeneric $ RGeneric '!'
  , RGeneric $ RGeneric $ LGeneric $ LGeneric 1
  , RGeneric $ RGeneric $ LGeneric $ RGeneric '!'
  , RGeneric $ RGeneric $ RGeneric $ LGeneric 1
  , RGeneric $ RGeneric $ RGeneric $ RGeneric '!'
  ]
