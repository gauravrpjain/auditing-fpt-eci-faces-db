## Description

The code files here are to run the analysis for the paper - '1.34 billion missing pieces? Examining the accuracy of facial processing tools on Indian faces'.

There are data files and code files to run the analysis and generate the analysis tables and plot for the paper. The code is done in R.

## Installation and Running

To run the code you need to have R 4.00 or newer version installed. The code should also work fine for R 3.6 but it has not been tested for any version older than R 4.0.

You can run the program using the make file which also checks for missing libraries and installs them. If you wish to run the individual code files yourself, you will need the following libraries to be installed.
1. tidyverse
2. xtable
3. stargazer
4. tikzDevice

Libraries 2-4 are required only to print tables, plots and regression output in Latex.

To run the code you can run either the make file or directly the Rscript.

```bash
Rscript setup.R
```

The run will generate output in terms of plots, tables and data dump which will all be saved in the Output folder.
The dump will be printed in a text file named "Error_analysis.txt" in the Output folder.
The Outputs folder have two folder one for Plots and Tables. All tables (except for regression table) are printed in both CSV and Latex, and all plots are saved in PNG and Latex.
It will also produce an intermediate output file called combined_df.Rda in the main folder.

## Files Needed

The code files  are as following
1. The code to get API response from the 4 tools: Amazon_run.inpyb (Jupyter notebook), FRT_R_Function.R (R script for tools other than Amazon)
2. 2_Results_Analysis: The code analyses the results from facial processing of the images and provides overall errors
3. 3_Regression: The code adds image properties and models the errors
4. 4_additional_plots: Outputs the plots

There are 6 data files needed to run the code -
1. Four API response files saved in folder "API_response" - microsoft_result.Rda; faex_output.Rda; Facepp_results.Rda; amazon_test_results_v20210222.csv
2. Two datasets - one self described information (Cleaned_data_no_PII.Rda) and one image quality dataset (image_df.Rda).
