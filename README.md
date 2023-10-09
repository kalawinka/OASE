# OASE

## Data Description: 

The folder row_data contains unaltered answers to the three surveys. Columns containing information revealing the survey participants' personal data and identity were removed from these datasets. 
Eco, ocean and soc in the names of all files stand for economics, oceanography and sociology. 

The folder analysis contains data and R-script used for the data analysis and creating plots.
Files comparison.csv, deposited.csv, and not_deposited.csv are the helper data with labels for question groups on publishing behaviour, motivations for depositing and not depositing preprints for the analysis and plots building script in R.
Files eco_cleaned.csv, soc_cleaned.csv and ocean_cleaned.csv contain cleaned data used for the analysis.   

The folder cleaning contains cleaning scripts in Python used to clean and prepare data for the analysis.  Our questionnaires contained free-text answer possibilities to the questions concerning career status and institution type. That has led to an increased number of different category types in these fields, making it difficult to conduct an analysis. We normalized these answers and concatenated some of them into one category. The file [discipline]_career_normalization.csv contains normalization for career status. The file [discipline]_institution_normalization.csv contains normalization for institution type.
