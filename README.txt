# R Bootcamp Project
Carlos Leon, Kristian Zutter

Thank you for taking the time to read this overview of our project structure.

All work has been documented on GitHub under the repository:  
https://github.com/cgleonr/RB01_Project/

# Project Structure:

## 1. SCRIPTS:
All scripts created and used are located under the `RB01/scripts` directory. However, most of the work was done in a single file:
- `EDA_all_dataframes.R`: This script contains the main exploratory data analysis and data manipulation tasks.

## 2. DATA:
- `datasets/`: Contains the raw datasets used in the project. These datasets are unprocessed and reflect the original data collected for analysis.
- `clean_datasets/`: Contains the cleaned and prepared datasets used for analysis and visualization. These datasets have been transformed, filtered, and prepared based on project needs.

## 3. `.Rmd` FILE AND FINAL OUTPUT:
Both the `.Rmd` file and the final knitted HTML output are located in the `Rmd_housing` directory.
- `.Rmd` File: The `.Rmd` file is ready to be knitted into an HTML document, provided all dependencies are installed.
- HTML File: The HTML file was knitted using `rmdformats::readthedown`. If accessing from GitHub, please save the HTML file locally before opening it in a browser to ensure all features work properly.

---

We hope you find our project structure clear and well-organized. Feel free to reach out via the GitHub repository if you have any questions or feedback!
