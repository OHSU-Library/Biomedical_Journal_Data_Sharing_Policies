# Biomedical Journal Data Sharing Policies

Raw data and analysis of data sharing policies of 318 biomedical journals.  The study authors manually reviewed the author instructions and editorial policies to analyze the earch journal's data sharing requirements and characteristics. The data sharing policies were ranked using a rubric to determine if data sharing was required, recommended, or not addressed at all.  The data sharing method and licensing recommendations were examined, as well any mention of reproducibility or similar concepts.  The data was analyzed for patterns relating to publishing volume, Journal Impact Factor, and the publishing model (open access or subscription) of each journal.

## Rubric and Raw Data

The rubric and raw data is found in [Data-Sharing-Policies_2016-1-18.xlsx](Data-Sharing-Policies_2016-1-18.xlsx).

We evaluated journals included in Thomson Reuter’s InCites 2013 Journal Citations Reports (JCR) classified within the following World of Science schema categories: Biochemistry and Molecular Biology, Biology, Cell Biology, Crystallography, Developmental Biology, Biomedical Engineering, Immunology, Medical Informatics, Microbiology, Microscopy, Multidisciplinary Sciences, and Neurosciences. These categories were selected to capture the journals publishing the majority of peer-reviewed biomedical research. The original data pull included 1,166 journals, collectively publishing 213,449 articles. We filtered this list to the journals in the top quartiles by impact factor (IF) or number of articles published 2013. Additionally, the list was manually reviewed to exclude short report and review journals, and titles determined to be outside the fields of basic medical science or clinical research. The final study set included 318 journals, which published 130,330 articles in 2013. The study set represented 27% of the original Journal Citation Report list and 61% of the original citable articles. Prior to our analysis, the 2014 Journal Citations Reports was released. After our initial analyses and first preprint submission, the 2015 Journal Citations Reports was released. While we did not use the 2014 or 2015 data to amend the journals in the study set, we did employ data from all three reports in our analyses. In our data pull from JCR, we included the journal title, International Standard Serial Number (ISSN), the total citable items for 2013, 2014, and 2015, the total citations to the journal for 2013/14/15, the impact factors for 2013/14/15, and the publisher.

## Analysis

All statistical analyses were performed with [R version 3.2.1](https://cran.r-project.org/). 

Analysis files are in the folder [analysis-code](analysis-code). Use [raw git] (https://rawgit.com/) to view HTML output files. 

The **R code** for the analysis can be found in the R markdown file [analysis-code/analysis-datasharing.Rmd](analysis-code/analysis-datasharing.Rmd)

You can view the **html output** of the analysis [on rawgit](https://rawgit.com/OHSU-Ontology-Development-Group/DataSharingPolicies/master/analysis-code/analysis-datasharing.html).
