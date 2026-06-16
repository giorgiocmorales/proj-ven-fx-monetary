# Venezuela Economy Data Pipeline

R-first repository for collecting Venezuelan macroeconomic data from official and secondary sources, preserving source-level extracts, and preparing a base layer for consultancy reporting and later forecast modules.

## Current Scope

- `FX`: official BCV and alternative sources, with a combined final dataset.
- `Prices`: BCV CPI source extract.
- `Monetary`: BCV monetary base and liquidity source extracts.
- `Oil`: OPEC production and price tables plus IMF WEO oil price benchmarks.
- `Real activity`: BCV quarterly and annual GDP workbooks plus IMF/WB GDP and per-capita indicators.
- `External`: BCV balance of payments, BCV reserves, and IMF/WB reserve proxies.
- `Fiscal`: IMF/WB public expenditure and fiscal balance indicators.

## Structure

- `scripts/`: flat script layout with one script per dataset/source extract plus domain runners.
- `data/raw/`: latest downloaded source file only.
- `data/processed/`: source-level processed outputs.
- `data/final/`: combined or estimated datasets.
- `registry/series_registry.csv`: minimal dataset registry.

## Naming

- Script pattern: `<domain>_<nn>_<variable>_<source>.R`
- The sequence number is unique within each sector.
- Variable names should stay short.
- Examples: `prices_01_cpi_bcv.R`, `external_02_fx_ind_bcv.R`, `real_02_gdp_a_s_bcv.R`

## Runners

- Install R dependencies on a fresh machine with:

```r
source("scripts/install_dependencies.R")
```

- `scripts/run_prices.R`
- `scripts/run_monetary.R`
- `scripts/run_oil.R`
- `scripts/run_external.R`
- `scripts/run_real.R`
- `scripts/run_fiscal.R`
- `scripts/run_all.R`

## Metadata Catalogs

- `scripts/catalog_imf_wb.R`: refreshes World Bank indicator metadata and IMF dataflow, dimension, and code catalogs into `registry/`.

## Notes

- Source-only extracts are the priority in this phase.
- Combined estimates, house adjustments, forecast diagnostics, and scheduled runs are intentionally deferred.
