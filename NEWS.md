## version 0.1.0

New release - features 
- item_analysis(score_csv_data, m_cutoff = 0.15)
- paired_samples(pre_csv_data, post_csv_data, m_cutoff = 0.15)
- independent_samples(treat_csv_data, ctrl_csv_data, m_cutoff = 0.15)
- one_way_ancova(treat_pre_csv_data, treat_post_csv_data, ctrl_pre_csv_data, ctrl_post_csv_data, m_cutoff = 0.15)
- one_way_repeated_anova(treat_pre_csv_data, treat_post_csv_data, treat_post2_csv_data, m_cutoff = 0.15)
- demo_group_diff(score_csv_data, group_csv_data, m_cutoff = 0.15, group_name)

### Automated analysis of assessment data for discipline-based education research.

- Users need to set the folder where their data to import resides as the working directory if they include only data file name(s) in the function.
- Alternatively, users can provide a path to their data in the function.

### NEWS.md setup

- added NEWS.md creation with newsmd


## version 0.1.1

"+ file LICENSE" deleted from the Description: License. 

## version 0.1.2

- Wrote package names in single quotes in the title and description field
- Added a reference on the methods used in the package to the description field 
- Added \value to .Rd files
- Deleted options(warn=-1)
- Used message() to write information messages
- Deleted the code to install packages in the vignette

## version 0.1.3

- Fixed DOIs as <doi:10.prefix/suffix> in the description field as requested

