# Brazil credit segmentation project

## Current status
- Main script builds Brazil directed-credit series successfully.
- Germany/KfW part downgraded to illustrative context only.
- BNDES raw file was too large, so I created a preprocessed `bndes_annual.csv`.
- Figure 1A works and is interpretable.
- Figure 1B likely has a GDP/unit mismatch and needs fixing.
- Figure 2 works mechanically but KfW series still needs more annual values.

## Last thing I did
- Replaced the giant live BNDES download with a local preprocessed CSV.
- Confirmed the script can read `bndes_annual.csv` with an absolute path.
- Removed the KfW scraping logic.

## Open problems
- Need to fix Brazil directed credit / GDP ratio.
- Need to decide whether to keep Germany/KfW in the project at all.
- Need to verify why directed-credit data only becomes non-missing around 2007.

## Next actions
1. Fix Figure 1B by checking units and/or finding an official BCB % GDP series.
1. Come up with a specification, empirical strategy. Probably LP, but I don't know what that entails

## Important file locations
- Main script: `brazil_germany_credit_comparison.R`
- BNDES preprocessing script: `preprocess_bndes_disbursements.R`
- Processed BNDES file: `bndes_annual.csv`

## Notes to self
- I erased the giant BNDES csv file
- Use the project folder as working directory in RStudio.
- Ignore raw HTML / downloaded junk in git.