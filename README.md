# DBERlibR
### An R Package for Automated Assessment Data Analysis

DBERlibR is an R package developed by Helikar Lab at the University of Nebraska-Lincoln for automated assessment data analyses. Some of the most frequently used statistical techniques are developed into functions to clean the data, merge/bind multiple data sets (as necessary), check assumption(s) for a specific statistical technique (as necessary), run the main assessment data analysis, and print the outputs in R console. The outputs contain a sample interpretation of the results for the convenience of users. Users need to prepare the data file as instructed.

# Functions

### item_analysis(score_csv_data, m_cutoff = 0.15)
This function automatically reads and cleans the data (e.g., converting missing values to “0”), calculates difficulty and discriminant scores, and prints the results in the R console and Plots panel. 

### paired_samples(pre_csv_data, post_csv_data, m_cutoff = 0.15)
This function automatically reads and cleans the data sets (e.g., converting missing values to "0), merges pre-post data sets, runs the (parametric) paired samples T-test and (nonparametric) Wilcoxon signed-rank test, and then prints outputs in the R console and Plots panel to help users examine the difference between pre-post scores. 

### independent_samples(treat_csv_data, ctrl_csv_data, m_cutoff = 0.15)
This function automatically reads and cleans the data sets (e.g., converting missing values to "0), binds treatment-control group data sets, runs the independent samples t-test (parametric) and Mann–Whitney U test (nonparametric), and then prints the results in the R console and Plots panel to help users examine the difference between the groups. 

### one_way_ancova( treat_pre_csv_data, treat_post_csv_data, ctrl_pre_csv_data, ctrl_post_csv_data, m_cutoff = 0.15)
This function can be used to analyze the difference between two groups (e.g., intervention vs. control group) with a covariate (e.g., pre-test scores) controlled. The function automatically merges pre-post data sets, binds treatment-control data sets, runs scripts to check assumptions of one-way ANCOVA, runs the main one-way ANCOVA and post hoc analyses, and then displays all outputs in the R console and Plots panel for users all at once. 

### one_way_repeated_anova(treat_pre_csv_data, treat_post_csv_data, treat_post2_csv_data, m_cutoff = 0.15)
This function can be used when you collect data from the same students repeatedly at three different time points (e.g., pre-test, post-test, and post2-test) and you want to examine the significance of the changes over time. The function automatically merges pre, post, and post2 data sets, runs the one-way repeated measures ANOVA with assumptions check, and then displays the outputs in the R console and Plots panel for users all at once.

### demo_group_diff(score_csv_data, group_csv_data, m_cutoff = 0.15, group_name)
This function automatically combines demographic variables to a data set, runs the analysis of variance (ANOVA) with assumptions check to examine demographic sub-group differences, and then displays the outputs in the R console and Plots panel for users all at once.
