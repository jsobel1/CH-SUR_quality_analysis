# CH-SUR quality analysis
Analysis scripts and materials for the evaluation of the CH‑SUR hospital surveillance system for influenza and COVID‑19 in Switzerland (2018–2023).

Please see the manuscript "Overview and evaluation of a nationwide hospital-based surveillance system for Influenza and COVID-19 in Switzerland (CH-SUR): 2018-2023"
by Jonathan Aryeh Sobel, Marie-Céline Zanella, Rebecca Grant, Camille Beatrice Valera, Mária Suveges, Laura Urbini, Khaled Mostaguir, Sara Botero, Ursina Roder, Davide Bosetti, Rami Sommerstein, Ulrich Heininger, Petra Zimmermann, Peter W Schreiber, Domenica Flury, Anita Niederer-Loher, Philipp Jent, Alexia Cusini, Didier Pittet, Stephan Harbarth, Anne Iten*, Olivia Keiser* and the CH-SUR Collaborative Network.

Here’s a brief description for each of the main project files provided:

- **`parameters_COVID.R`**  
  Defines all global settings for the COVID‑19 analysis: loads required R packages, sets up helper operators (`%notin%`), lists participating centres, assigns the disease label (`"COVID"`), and specifies date parameters (export/clean dates, start/end of analysis period, week‑level variables).

- **`parameters_FLU.R`**  
  Analogous to `parameters_COVID.R`, but configured for influenza (`disease_nm <- "FLU"`). It loads the same packages and helpers, lists centres, and sets the seasonal analysis period for influenza.

- **`data_load_prep.R`**  
  Loads the cleaned CH‑SUR datasets (`hospdat_new_vars` and `hospdat_recoded`), defines utility functions (e.g. `dedup_last`, `dedup_first`, `filter_dags`), recodes COVID‑19 waves and influenza seasons, harmonizes centre labels, and derives new variables (e.g. severity categories, immunosuppression flags, complication and comorbidity counts).

- **`delay_analysis.R`**  
  Carries out the quantitative timeliness evaluation: sources `parameters_COVID.R` (and later `parameters_FLU.R`) and `data_load_prep.R`; loads necessary plotting and summary libraries; generates epidemic‑curve plots; computes delays between hospital admission and data entry for both COVID‑19 and influenza; produces summary tables, violin/box plots by wave/season, and completeness heatmaps; and exports results as CSV and/or PDF.

- **`exploration_evaluation_survey.Rmd`**  
  An R Markdown document that conducts the qualitative survey analysis: imports survey responses, cleans and tabulates stakeholder ratings (importance, reliability, adaptability), runs text‑mining (word‑cloud) on open‑ended feedback, and renders summary tables and visualizations to assess strengths and improvement areas for CH‑SUR.

# Data availability
Data Availability Statement: The datasets presented in this repo are not available because they are part of the Swiss surveillance system. Motivated requests to access the datasets should be directed to the Swiss Federal Office of Public Health (FOPH). The dataset include all variables described in our Supplementary File 1 (codebook version 2023).
