# Analysis of non-linear air pollution time series

This repository contains the code used for creating graphics in the research paper titled "Analysis of non-linear air pollution time series" authored by Kelyan Hangard.

## Scripts Overview

### 1. Spatio_temporal_EDA.R

This script performs a spatio-temporal Exploratory Data Analysis (EDA) on the pollutant concentration data, particularly NO2 and PM2.5. It also has the capability to handle other pollutant concentration data. It produces all plots found in "Appendix A First spatio-temporal EDA" of the paper.

**Input data requirements**: Series format data with no missing values. For transforming raw data to series format, refer to the 'Construct serie data' section in `pollutant_exploration.ipynb`.

### 2. semivariogram_plots.R

This script computes and plots the semivariograms of NO2 and PM2.5 concentration data, utilizing the STRbook library. It generates semivariograms found in the "Variography" subsection under "Results & Discussion" section in the paper.

**Input data requirements**: Series format data with no missing values. For transforming raw data to series format, refer to the 'Construct serie data' section in `pollutant_exploration.ipynb`.

### 3. pollutant_exploration.ipynb

This comprehensive script includes data importing, cleaning, exploration, and a series of information theory measures calculation using the FishPy package. It covers all plots in the "Data collection and exploratory data analysis" section and some plots in the first Appendix of the paper. It also provides all figures under the "3.2 Information theory" subsection in the "3 Results and discussion" section.

**Input data requirements**: Each csv concentration pollutant file should be placed inside a folder named after the pollutant (ex NO2). In each csv file, the first column should be Datetime, second unit (ex ug/cm^3), followed by the pollutant time series corresponding to each station.

### 4. eof.R

This script computes the empirical orthogonal function decomposition on the SEP and FIM time series of NO2 concentration data and PM2.5 concentration data. It can also be applied to other time series data.

**Input data requirements**: Series format data. For transforming raw data to series format, refer to the 'Construct serie data' section in `pollutant_exploration.ipynb`.

## Getting Started

1. Clone the repository.
2. Install the necessary dependencies.
3. Run each script as necessary.
