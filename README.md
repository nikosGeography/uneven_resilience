# Uneven urban resilience across economic sectors revealed by satellite nighttime lights
This repository contains the data and R code used to analyse uneven urban resilience across economic sectors using satellite-derived nighttime light data. The repository includes sector-level datasets organised by city, as well as scripts for the figures and tables found in the paper.

## Usage
The analytical workflow is implemented in `R`. After cloning the repository, users can reproduce the results by running the scripts in the `r_code/` directory in the order indicated by their filenames. The scripts rely on the directory structure and file naming conventions of the `data/` folder as provided.

## Contents
r_code/: R scripts implementing the analytical workflow and generating the figures and tables reported in the paper.
- mca.R: Multiple correspondence analysis.
- midpoint_classification.R: Sector classification procedure.
- continents_table.R: Table construction (Table 1 in the paper).
- r2_appendix.R: Supplementary analysis (Table A1 in the paper).
- table2_appendix.R: Appendix tables (Table A2 in the paper).

data/: Sector-level datasets organised by city.
- data/<city>/lu/<sector>/*.csv: City-level sectoral data used in the analysis.

## Usage
1. Clone this repository to your local machine.
2. Ensure that `R` and the required `R` packages are installed.
3. Run the scripts in the `r_code/` directory to reproduce the analysis.
4. The scripts read input data from the `data/` directory and generate the figures and tables reported in the paper.

## Citation
If you use the data or code from this repository in your research, please cite the associated paper:
