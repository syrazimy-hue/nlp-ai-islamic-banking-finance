# nlp-ai-islamic-banking-finance
Data and R scripts for an NLP-based analysis of the AI landscape in Islamic banking and finance
# NLP-Based Analysis of the AI Landscape in Islamic Banking and Finance

This repository contains the dataset and R script used for the study titled **“A NLP-Based Analysis of the AI Landscape in Islamic Banking and Finance.”**
The repository is prepared to support transparency, reproducibility, and verification of the text-analysis workflow used in the study.

## Repository Status

This repository is linked to a manuscript that is currently **under submission**.
If the journal uses double-blind review, author-identifying information should be removed or anonymized until the review process is complete.

## Repository Contents
```text
.
├── data/
    └── data_cleaned_final
    └── analysis_main_script
│
├── README.md
├── LICENSE
└── CITATION.cff
```

## Dataset Description
The dataset is stored in:
```text
data/data_cleaned_final.csv
```

The dataset contains cleaned abstract-level metadata used for the NLP analysis.

| Column   | Description                                             |
| -------- | ------------------------------------------------------- |
| PaperID  | Unique identifier assigned to each paper in the dataset |
| Abstract | Cleaned abstract text used as input for NLP analysis    |

## Analysis Script

The main analysis script is stored in:

```text
scripts/analysis_main_script.R
```

The script contains the R code used to process the abstract text and conduct the NLP-based analysis.

## Software Requirements

The analysis was conducted using R and RStudio.

Required R packages may include:

Please check the `analysis_main.R` file for the exact packages used in the analysis.

## How to Reproduce the Analysis

1. Download or clone this repository.
2. Open the project folder in RStudio.
3. Open the file:

```text
scripts/analysis_main.R
```

4. Install the required R packages if they are not already installed.
5. Run the script from top to bottom.
6. The script will use the cleaned dataset stored in the `data/` folder.

## Data Availability Note

The dataset contains paper identifiers and abstract text used for NLP analysis.

If the abstract text was obtained from licensed databases or publisher platforms, redistribution may be subject to database or publisher terms of use. Users should ensure that any reuse of the abstract text complies with the relevant data-source policies.

## Citation

If you use this repository, please cite it using the citation information provided in the `CITATION.cff` file.

Citation file will be added after the paper is submitted.


## Contact

For questions about this repository, please contact:
hazmyrapi.my@gmail.com

