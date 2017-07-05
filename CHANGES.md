## Version 0.5.1.0

 * Add `FromField`/`ToField` instances for `Scientific` (#143,#144)

## Version 0.5.0.0

### Semantic changes

 * Don't unecessarily quote spaces with `QuoteMinimal` (#118,#122,#86)
 * Fix semantics of `foldl'` (#102)
 * Fix field error diagnostics being mapped to `endOfInput` in `Parser` monad. (#99)
 * Honor `encIncludeHeader` in incremental API (#136)

### Other changes

 * Support GHC 8.2.1
 * Use factored-out `Only` package
 * Add `FromField`/`ToField` instance for `ShortText`
 * Add `MonadFail` and `Semigroup` instance for `Parser`
 * Add `Semigroup` instance for incremental CSV API `Builder` & `NamedBuilder`
 * Port to `ByteString` builder & drop dependency on `blaze-builder`

## Version 0.4.5.1

 * Restore GHC 7.4 support (#124)

## Version 0.4.5.0

 * Support for GHC 8.0 added; support for GHC 7.4 dropped

 * Fix defect in `Foldable(foldr)` implementation failing to skip
   unconvertable records (#102)

 * Documentation fixes

 * Maintainer changed

## Version 0.4.4.0

 * Added record instances for larger tuples.

 * Support attoparsec 0.13.

 * Add field instances for short bytestrings.

## Version 0.4.3.0

 * Documentation overhaul with more examples.

 * Add Data.Csv.Builder, a low-level bytestring builder API.

 * Add a high-level builder API to Data.Csv.Incremental.

 * Generalize the default FromNamedRecord/ToNamedRecord instances.

 * Improved support for deriving instances using GHC.Generics.

 * Added some control over quoting.

## Version 0.4.2.4

 * Support attoparsec 0.13.

## Version 0.4.2.3

 * Support GHC 7.10.

## Version 0.4.2.2

 * Support blaze-builder 0.4.

 * Make sure inlining doesn't prevent rules from firing.

 * Fix incorrect INLINE pragmas.

## Version 0.4.2.1

 * Support deepseq-1.4.

## Version 0.4.2.0

 * Minor performance improvements.

 * Add 8 and 9 tuple instances for From/ToRecord.

 * Support text-1.2.

## Version 0.4.1.0

 * Ignore whitespace when converting numeric fields.

 * Accept \r as a line terminator.

 * Support attoparsec-0.12.
