[README.md](https://github.com/user-attachments/files/27188409/README.md)
# Fertility in the Knowledge Economy

Code for "Fertility in the Knowledge Economy: Returns to Experience, Rising Costs, and Youth Precariousness" by Aidar Tuleshov.

## Files

- `00_clean_data.R` — Loads raw IPUMS USA extract, constructs analysis samples (wage, fertility, employment), and saves as .rds files.
- `01_analysis.R` — Estimates year-by-year Mincer regressions, pooled regressions with decade interactions, structural break tests, education/industry/occupation heterogeneity, within-between decompositions, fertility timing analysis, and Baumol's cost disease figures.

## Data

The raw data is an IPUMS USA extract (not included due to size and license). To replicate, request an extract from [IPUMS USA](https://usa.ipums.org/) with the following setup.

### Samples

| Sample | Years |
|--------|-------|
| Decennial Census (1%) | 1940, 1950, 1960, 1970, 1980, 1990, 2000 |
| American Community Survey (1-year) | 2001–2024 |

### Variables

| Variable | Description |
|----------|-------------|
| YEAR | Survey year |
| AGE | Age |
| SEX | Sex |
| EMPSTAT | Employment status |
| INCWAGE | Wage and salary income |
| INCWAGE_CPIU_2010 | Wage income in constant 2010 dollars (CPI-U adjusted) |
| PERWT | Person weight |
| EDUC | Educational attainment |
| RACE | Race |
| HISPAN | Hispanic origin |
| MARST | Marital status |
| STATEFIP | State (FIPS code) |
| IND1990 | Industry, 1990 basis (harmonized) |
| OCC | Occupation (Census 2010 codes, consistent 2005+) |
| FERTYR | Gave birth in past 12 months (ACS only) |
| NCHLT5 | Number of own children under 5 in household |

### Additional data

- `bls_cpi.csv` — CPI-U series from the Bureau of Labor Statistics (All items SA0, Medical care SAM, Education SAE1, Shelter SAH1). Downloaded manually from [BLS](https://www.bls.gov/cpi/).

## Requirements

R with packages: `tidyverse`, `haven`, `fixest`, `beepr`
