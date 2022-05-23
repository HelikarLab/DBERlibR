# DBERlibR
### An R Package for Automated Test Data Analysis

DBERlibR is an R package developed by HelikarLab at the University of Nebraska-Lincoln for automated test data analyses. Some of the most frequently used statistical techniques are developed into functions to clean the data, merge/bind multiple data sets (as necessary), check assumption(s) for a specific statistical technique (as necessary), run the main test data analysis, and export the analysis output(s) automatically and all at once. The output(s) contain(s) a summary of the results for the convenience of users. Users need to prepare the data file as instructed (refer to Tutorial available at https://helikarlabpersonal.github.io/DBERlibR/tutorial.html), save it in the folder designated as a working directory in RStudio, load all dependencies, and type a function in the R console to conduct a specific data analysis.

# Functions

### loadpackages()
This function will download all dependencies (other packages required for using DBERlibR) that don’t exist in your RStudio and then automatically load them. You need to run this function whenever you open RStudio to use DBERlibR (you don’t need to repeat this function unless you close and re-open RStudio).

### itemanalysis(csv_data)
This function automatically reads and cleans the data (e.g., converting missing values to “0”), calculates difficulty and discriminant scores, and exports the results to an Excel file, which is saved in the working directory. The Excel file name follows the data file name used for item analysis; for example, if you’ve input “data_treat_pre.csv” in the function, then the output Excel file name contains “treat_pre.” The function also generates plots in the jpeg file format to visualize the results; the jpeg file name follows the data file name as well so that you can easily find the results of the item analysis you’ve just run. 

### pairedsamples()
This function automatically cleans the data sets (e.g., converting missing values to "0), merges pre-post Data Sets, runs the (parametric) Paired Samples T-test and (nonparametric) Wilcoxon Signed-Rank test, and then exports outputs to help you examine the difference between pre-post scores. 

### independentsamples()
This function automatically cleans the data sets (e.g., converting missing values to "0), binds treatment-control group Data Sets, runs the Independent Samples T-test (parametric) and Mann–Whitney U test (nonparametric), and then exports outputs to help you examine the difference between the groups. R scripts and their outputs are as follows (just pay attention to the outputs since the codes are automatically run back-end by the function).

### onewayancova()
This function automatically merges pre-post data sets, binds treatment-control data sets, and runs scripts to check assumptions of one-way ANCOVA, runs the main One-way ANCOVA, and then exports all outputs for you all at once. Please make sure to name data files accurately (i.e., “data_treat_pre.csv”, “data_treat_post.csv”, “data_ctrl_pre.csv”," data_ctrl_post.csv") and have them saved in the working directory. 
