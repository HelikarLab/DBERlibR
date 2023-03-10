% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DBERlibR.R
\name{demogroupdiff}
\alias{demogroupdiff}
\title{Demographic Group Differences}
\usage{
demogroupdiff(csv_data)
}
\arguments{
\item{csv_data}{This function requires a csv data file name: "data_treat_pre.csv", "data_treat_post.csv".}
}
\description{
This function automatically combines demographic variables to a data set, runs the analysis of variance (ANOVA) with assumptions check to examine demographic sub-group differences, and then exports outputs for you all at once. Please make sure to name data files accurately and have them saved in the working directory. Find the output files "subgroupdifferences_outputs.txt" instantly generated by this function in the working directory.
}
