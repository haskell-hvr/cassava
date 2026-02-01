# Cassava Benchmark Suite

Benchmarks for [cassava](https://github.com/hvr/cassava), a high-performance CSV parsing library for Haskell.

## Overview

This benchmark suite compares CSV parsing and encoding performance across different scenarios:

- **Positional decoding** — parsing CSV records by column index
- **Named decoding** — parsing CSV records by header name
- **Encoding** — serializing Haskell records to CSV
- **Streaming** — lazy/streaming CSV parsing
- **Wide CSV** — performance with many columns (500 columns)
- **Long CSV** — performance with many rows (1,000,000 rows)
- **Comparison** — benchmarking against `lazy-csv` library

## Benchmarks

The suite includes two benchmark targets:

| Target | Description |
|--------|-------------|
| `benchmark-iut` | Benchmarks the local cassava implementation (implementation under test) |
| `benchmark-ref` | Benchmarks the reference cassava from Hackage |

## Data Files

The benchmarks use the following CSV files:

| File | Description |
|------|-------------|
| `presidents.csv` | US Presidents data without header (positional parsing) |
| `presidents_with_header.csv` | US Presidents data with header (named parsing) |
| `random_r5000_c500_s42.csv` | Wide CSV: 5,000 rows × 500 columns |
| `random_r1000000_c10_s42.csv` | Long CSV: 1,000,000 rows × 10 columns |

### Generating Random CSV Files

Random CSV files are generated using the `Generate.hs` cabal script:

```bash
# Usage: cabal run Generate.hs -- <rows> <columns> <seed>

# Generate wide CSV (5000 rows, 500 columns, seed 42)
cabal run Generate.hs -- 5000 500 42

# Generate long CSV (1000000 rows, 10 columns, seed 42)
cabal run Generate.hs -- 1000000 10 42
```

The generator creates CSV files with randomized integer, double, and string columns. The seed ensures reproducible benchmarks across runs.

### Build

```bash
cabal build all
```

### Run Benchmarks

```bash
# Run implementation-under-test benchmarks
cabal bench benchmark-iut

# Run reference benchmarks
cabal bench benchmark-ref
```

### Generate HTML Report

```bash
cabal bench benchmark-iut --benchmark-options="--output=report-iut.html"
cabal bench benchmark-ref --benchmark-options="--output=report-ref.html"
```

### Compare Specific Benchmarks

```bash
# Run only positional decode benchmarks
cabal bench benchmark-iut --benchmark-options="-m prefix positional/decode"

# Run only wide/long benchmarks
cabal bench benchmark-iut --benchmark-options="-m prefix wide"
cabal bench benchmark-iut --benchmark-options="-m prefix long"
```
