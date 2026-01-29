# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

babynamer provides real-time access to U.S. baby name data from the Social Security Administration. Single-function package that downloads, parses, and enriches SSA baby name datasets.

## Build/Test Commands

```bash
Rscript -e "devtools::document()"   # Regenerate docs
Rscript -e "devtools::load_all()"   # Load for development
Rscript -e "devtools::test()"       # Run tests
Rscript -e "devtools::check()"      # Full R CMD check
```

## Architecture

### Single Export Pattern

One exported function with internal memoized helpers:

```
us_baby_names(type, include_features)
    └── .babynames()           # Router/dispatcher
        ├── .national()        # National SSA data (memoized)
        ├── .state()           # State-level data (memoized)
        └── .territory()       # Territory data (memoized)
```

### Data Sources

All sourced from SSA zip files:
- `national`: `ssa.gov/oact/babynames/names.zip`
- `state`: `ssa.gov/oact/babynames/state/namesbystate.zip`
- `territory`: `ssa.gov/oact/babynames/territory/namesbyterritory.zip`

### Download Mechanism (CRITICAL)

SSA uses Akamai CDN which blocks programmatic requests. The package uses `httr2` with browser-like headers to bypass this:

```r
.download_ssa_file() # Helper with Sec-Fetch-* headers
```

**Required headers for SSA downloads:**
- `Sec-Fetch-Dest: document`
- `Sec-Fetch-Mode: navigate`
- `Sec-Fetch-Site: none`
- `Sec-Fetch-User: ?1`

If downloads break in the future, check if SSA has changed their CDN protection.

### Memoization

Internal functions (`.national`, `.state`, `.territory`) are wrapped with `memoise()` to cache expensive downloads within an R session. Cache clears when session ends.

### Output Schema

Returns tibble with columns varying by type:
- Common: `type`, `decade`, `year`, `date`, `sex`, `rank`, `name`, `count`, `pct_*`
- When `include_features=TRUE`: adds `count_distinct_years`, `year_recent`, `year_first`, `year_most_popular_*`, `top_rank`, `worst_rank`, `count_characters`
- State data adds: `state` column
- Territory data adds: `territory` column

Multiple types return nested tibble with `data` list-column.
