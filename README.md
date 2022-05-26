# DBERlibR
### An R Package for Automated Test Data Analysis

DBERlibR is an R package developed by HelikarLab at the University of Nebraska-Lincoln for automated test data analyses. Some of the most frequently used statistical techniques are developed into functions to clean the data, merge/bind multiple data sets (as necessary), check assumption(s) for a specific statistical technique (as necessary), run the main test data analysis, and export the analysis output(s) automatically and all at once. The output(s) contain(s) a summary of the results for the convenience of users. Users need to prepare the data file as instructed (refer to Tutorial available at https://helikarlab.github.io/DBERlibR/tutorial.html), save it in the folder designated as a working directory in RStudio, load all dependencies, and type a function in the R console to conduct a specific data analysis.

# Functions

### itemanalysis(csv_data)
This function automatically reads and cleans the data (e.g., converting missing values to “0”), calculates difficulty and discriminant scores, and exports the results to an Excel file, which is saved in the working directory. The Excel file name follows the data file name used for item analysis; for example, if you’ve input “data_treat_pre.csv” in the function, then the output Excel file name contains “treat_pre.” The function also generates plots in the jpeg file format to visualize the results; the jpeg file name follows the data file name as well so that you can easily find the results of the item analysis you’ve just run. 

### pairedsamples()
This function automatically cleans the data sets (e.g., converting missing values to "0), merges pre-post Data Sets, runs the (parametric) Paired Samples T-test and (nonparametric) Wilcoxon Signed-Rank test, and then exports outputs to help you examine the difference between pre-post scores. 

### independentsamples()
This function automatically cleans the data sets (e.g., converting missing values to "0), binds treatment-control group Data Sets, runs the Independent Samples T-test (parametric) and Mann–Whitney U test (nonparametric), and then exports outputs to help you examine the difference between the groups. R scripts and their outputs are as follows (just pay attention to the outputs since the codes are automatically run back-end by the function).

### onewayancova()
This function automatically merges pre-post data sets, binds treatment-control data sets, and runs scripts to check assumptions of one-way ANCOVA, runs the main One-way ANCOVA, and then exports all outputs for you all at once. Please make sure to name data files accurately (i.e., “data_treat_pre.csv”, “data_treat_post.csv”, “data_ctrl_pre.csv”," data_ctrl_post.csv") and have them saved in the working directory. 

### onewayrepeatedanova()
This function can be used when you collect data from the same students repeatedly at three different time points (e.g., pre-test, post-test, and second post-test) and you want to examine the significance of the changes over time. The function automatically merges pre, post, and post2 data sets, runs the One-way repeated measures ANOVA with assumptions check, and then displays/exports outputs for you all at once. You need to name data files accurately (i.e., “data_treat_pre.csv”, “data_treat_post.csv”, and “data_treat_post2.csv”) and have them saved in the working directory.

### demogroupdiff(csv_data)
This function automatically combines demographic variables to a data set, runs the analysis of variance (ANOVA) with assumptions check to examine demographic sub-group differences, and then displays/exports outputs for you all at once. You should have both test data (“data_treat_pre.csv”, “data_treat_post.csv”, or “data_treat_post2.csv”) and demographic data (“demographic_data.csv”) files in the working directory. You need to name data files accurately and have them saved in the working directory.
