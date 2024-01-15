This readme describes the survey data and cleaning/analysis scripts of our quantitative online survey which we performed in our project OASE.
The results have been published as Biesenbender et al. 2024, see reference below.

"If you use this data and scripts, please cite it as below."

Biesenbender, K., Smirnova, N., Mayr, P., & Peters, I. (2024). The Emergence of Preprints: Comparing Publishing Behaviour in the Global South and the Global North. Online Information Review. https://doi.org/10.1108/OIR-04-2023-0181 


# Data Description: 

### raw_data

The folder **raw_data** contains unaltered answers to the three surveys. Columns containing information revealing the survey participants' personal data and identity were removed from these datasets. 

**Eco**, **ocean** and **soc** in the names of all files stand for economics, oceanography and sociology. 

### analysis

The folder **analysis** contains data and R-script used for the data analysis and creating plots.
Files **comparison.csv**, **deposited.csv**, and **not_deposited.csv** are the helper data with labels for question groups on publishing behaviour, motivations for depositing and not depositing preprints for the analysis and plots building script in R.
Files **eco_cleaned.csv**, **soc_cleaned.csv** and **ocean_cleaned.csv** contain cleaned data used for the analysis.   

### cleaning
The folder **cleaning** contains cleaning scripts in Python used to clean and prepare data for the analysis.  Our questionnaires contained free-text answer possibilities to the questions concerning career status and institution type. That has led to an increased number of different category types in these fields, making it difficult to conduct an analysis. We normalized these answers and concatenated some of them into one category. The file **[discipline]_career_normalization.csv** contains normalization for career status. The file **[discipline]_institution_normalization.csv** contains normalization for institution type.

# Acknowledgment
This work is funded by the German Federal Ministry of Education and Research (BMBF) project Open Access Effects – The Influence of Structural and Author-specific Factors on the Impact of OA (OASE), grant numbers 16PU17005A and 16PU17005B. 
