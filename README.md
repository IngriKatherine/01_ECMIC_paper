# How Does Female Leadership Relate to Wage-Productivity Gaps in Colombia?

## Overview

This repository contains the code, data, and analysis for the research paper "How Does Female Leadership Relate to Wage-Productivity Gaps in Colombia?" by Ingri Katherine Quevedo Rocha.

The study examines whether female leadership in Colombian manufacturing firms is associated with lower labor monopsony power, measured through wage-productivity gaps (labor markdowns). Using firm-level data from the Colombian Manufacturing Survey (EAM) spanning 2000-2019, the analysis investigates how gender composition of firm ownership relates to firms' exercise of wage-setting power in labor markets characterized by frictions.

## Research Question

Do firms with greater female ownership exhibit lower labor markdowns than firms primarily owned by men? The paper explores whether female leaders adopt management practices that place greater weight on worker welfare and equitable compensation, potentially mitigating wage distortions arising from labor market monopsony.

## Data Sources

- **Primary Data**: Colombian Manufacturing Survey (EAM - Encuesta Anual Manufacturera) from the National Department of Statistics of Colombia (DANE), 2000-2019
- **Industry Classifications**: CIIU to ISIC-4 mapping
- **Macroeconomic Variables**: Additional economic indicators
- **External Datasets**: CompeteLAC dataset for comparative markdown estimates

## Methodology

The analysis employs production function methods to estimate labor markdowns:

1. **Data Cleaning**: Process raw EAM survey data across multiple years
2. **Exploratory Data Analysis**: Examine data distributions and patterns
3. **Production Function Estimation**: Use Cobb-Douglas revenue production functions to recover labor elasticities
4. **Markdown Calculation**: Compare marginal revenue product of labor to actual wages
5. **Regression Analysis**: Relate markdowns to female ownership shares, controlling for firm and industry characteristics

## Repository Structure

```
├── code/                 # Analysis scripts
│   ├── 101_clean_EAM.R              # Data cleaning (R)
│   ├── 102_markdowns.R              # Markdown calculations (R)
│   ├── 104_EDA.R                    # Exploratory data analysis (R)
│   ├── 105_estimation.R             # Main estimation (R)
│   ├── *.do                         # Stata scripts for markdown estimation
│   └── examples.py                  # Python examples
├── data/
│   ├── raw/                         # Raw EAM survey data (1995-2023)
│   ├── proc/                        # Processed datasets
│   ├── input/                       # Additional input data
│   └── temp/                        # Temporary files
├── draft/               # Working drafts and references
├── figures/             # Generated figures
├── latex/               # LaTeX source for paper
│   ├── 01_paper.tex                # Main paper
│   ├── 01_VT_presentation.tex       # Presentation slides
│   └── *.bib                        # Bibliography files
├── output/              # Analysis results and tables
├── references/          # Reference materials
└── tables/              # Generated tables
```

## Software Requirements

- **R** (with packages: dplyr, tidyverse, ggplot2, haven, ezids, arrow)
- **Stata** (for markdown estimation routines)
- **Python** (optional, for examples)
- **LaTeX** (for paper compilation)

## Key Scripts

### R Scripts
- `101_clean_EAM.R`: Loads and cleans EAM data across multiple years
- `102_markdowns.R`: Processes markdown-related calculations
- `104_EDA.R`: Exploratory data analysis and visualization
- `105_estimation.R`: Main econometric analysis

### Stata Scripts
- `901_markdownsCompeteLAC.do`: Comparative markdown analysis
- `902_*.do`: Various markdown estimation procedures using different methodologies (DLW, ACF, CS, CAL methods)

## Data Processing Pipeline

1. **Raw Data Import**: Load EAM survey files (CSV format) for each year
2. **Data Cleaning**: Standardize variable names, handle missing data, merge across years
3. **Industry Classification**: Map CIIU codes to ISIC-4 classifications
4. **Variable Construction**: Create ownership variables, production inputs, outputs
5. **Markdown Estimation**: Apply production function methods to calculate wage-productivity gaps
6. **Regression Analysis**: Estimate relationships between female ownership and markdowns

## Key Variables

- **Labor Markdowns**: Wage-productivity gaps calculated as (MRP_L - Wage) / MRP_L
- **Female Ownership**: Share of firm ownership held by women
- **Production Inputs**: Labor (wage bill), capital, materials
- **Industry Controls**: 4-digit ISIC industry classifications
- **Firm Characteristics**: Size, age, productivity measures

## Results Summary

Preliminary findings suggest a negative association between female ownership shares and labor markdowns, indicating that firms with greater female leadership may exercise less wage-setting power. The relationship is examined across different estimation methodologies and robustness checks.

## References

Key references include:
- De Loecker et al. (2020) - "The Rise of Market Power and the Macroeconomic Implications"
- Levinsohn & Petrin (2003) - Production function estimation methods
- Yeh (2022) - Monopsony measurement methodology
- Inter-American Development Bank (2025) - CompeteLAC dataset

## Contact

**Author**: Ingri Katherine Quevedo Rocha
**Institution**: George Washington University
**Email**: [Contact information]

## License

This research is part of an academic project. Please cite appropriately if using any materials.

---

*Last updated: April 2026*