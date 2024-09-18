# R Bootcamp Project
Carlos Leon, Kristian Zutter

Thank you for taking the time to read this overview of our project structure.

All work has been documented on GitHub, under the repository: https://github.com/cgleonr/RB01_Project/

The structure of the project is as follows:

# SCRIPTS:
All scripts created and used are under `RB01/scripts`. The primary work has been conducted in the file:
- `EDA_all_dataframes.R`

This file contains the majority of the exploratory data analysis (EDA) and data preparation steps.

# DATA:
- `datasets/` contains the **raw datasets** used for the project.
- `clean_datasets/` contains the **cleaned and prepared datasets** that were used for our analysis.

# .Rmd FILE AND FINAL OUTPUT:
The `.Rmd` and the `.html` output files can both be found under the `Rmd_housing/` folder:
- The `.Rmd` file is ready to be knit if all dependencies are installed.
- The `.html` file is the knitted output (using `rmdformats::readthedown`). 

**Important**:
- If viewing the `.html` file directly from GitHub, it must be downloaded and saved locally for proper rendering in a browser.
