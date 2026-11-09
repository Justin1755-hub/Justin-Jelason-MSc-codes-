# Carboxylesterase genes (*Coeae1f* and *Coeae6g*) — analysis scripts

These scripts extend the MSc analysis of *Cyp6p3*, *Cyp6m2* and *Cyp6aa1* to two
carboxylesterase genes, *Coeae1f* and *Coeae6g*, measured by qPCR in the same
60 cDNA pools (Kisumu and Tiassale colonies of *Anopheles gambiae*; age,
blood-feeding status and time-of-day experiments).

The steps and the statistics are the same as for the three cytochrome P450 genes:
Shapiro–Wilk tests and Q–Q plots for normality, general linear models with
`drop1(..., test = "F")`, pairwise Welch t-tests, and box plots with significance
bars.

## Files

| File | What it does |
| --- | --- |
| `00_Coeae_deltaCt.R` | Reads the six AriaMx plate exports, assigns wells to samples, averages the two technical replicates and calculates ΔCt. Writes `gene_expression_data_5genes.xlsx`. |
| `01_t_tests_Coeae.R` | Pairwise Welch t-tests for all six group comparisons in each experiment. Writes `t_tests_Coeae.csv`. |
| `02_GLM_Coeae.R` | General linear models: interaction model first, then the main-effects model, with F tests from `drop1`. |
| `03_normality_Coeae.R` | Shapiro–Wilk tests and Q–Q plots. |
| `04_boxplots_Coeae.R` | Box plots by age, blood-feeding status and time of day. Writes `boxplots_Coeae1f.png` and `boxplots_Coeae6g.png`. |
| `gene_expression_data_5genes.xlsx` | ΔCt values for all five genes, produced by `00_Coeae_deltaCt.R`. |

## How to run

```r
install.packages(c("readxl", "ggplot2", "ggsignif", "patchwork", "data.table", "writexl"))

# set the working directory to the folder holding these files, then:
source("00_Coeae_deltaCt.R")   # only needed to rebuild the xlsx from raw Ct files
source("01_t_tests_Coeae.R")
source("02_GLM_Coeae.R")
source("03_normality_Coeae.R")
source("04_boxplots_Coeae.R")
```

`gene_expression_data_5genes.xlsx` is included, so scripts 01–04 run on their own.
Script `00` additionally needs the original `gene_expression_data.xlsx` and a folder
`extra_qPCRs/` holding the six AriaMx exports:

```
extra_qPCRs/eric1 - Tabular Results.txt       extra_qPCRs/eric2 - Tabular Results.txt
extra_qPCRs/eric3-bf - Tabular Results.txt    extra_qPCRs/eric5-bf - Tabular Results.txt
extra_qPCRs/eric4-time - Tabular Results.txt  extra_qPCRs/eric6-time - Tabular Results.txt
```

## Notes on the data

- ΔCt is calculated as **mean Ct of the housekeeping genes (*S7*, *Ef*) minus the mean Ct
  of the gene of interest**, so that a higher ΔCt means higher expression. This is the same
  direction as the ΔCt values used for the *Cyp6* genes.
- Each sample was run on two plates, one plate per technical replicate. One housekeeping
  well (*Ef*) failed to amplify and the remaining replicate was used for that sample.
- Trailing spaces in the condition labels of the original spreadsheet ("5 Days ") are removed,
  so that all 5-day samples form a single group.

## Citation

Jelason JA, Lucas ER, Donnelly MJ. Transcriptomic surveillance: could the expression of
metabolic genes be a reliable tool for monitoring insecticide resistance in malaria
mosquitoes? (manuscript in preparation).
