### US Food Environments Clustering and Analysis

### SYNOPSIS
R project for clustering and analysis of Food Environments based on USDA's 2020 Food Environmental Atlast (FEA) and Food Access Research Atlas (FARA), as well as 2018 Census data on labor participation rates and educational attainment.

Analysis includes importing of raw data, cleaning, EDA (variable correlations, boxplots, chloropleth mapping), Unsupervised Learning (Bayesian PCA for dimensionality reduction, K-means clustering), Statistical Testing of Clusters vs. Outcomes, Supervised Learning (Random Forests).

### ABSTRACT
A central theme in the discussion of “food environments” is the idea of a “food desert,” generally defined as an area that is both low-income and low-food-access. Some theories argue that this leads to an absence of affordable, accessible nutrition, which negatively impacts health. However, recent research calls this causation into question, with claims that choices, rather than lack of nutritional access, drive poor health outcomes such as diabetes and obesity.  In this analysis, the USDA’s 2020 Food Environmental Atlas and 2018 US Census Data were examined to determine whether clustering areas by common food environment can reveal how underlying factors and health outcomes vary beyond food desert classification.  Bayesian PCA and K-means clustering were performed, by county and stratified by metro status. This yielded six total food environment clusters.  Clusters in each group showed statistically significant differences in food desert status and diabetes rate and while each cluster could be found throughout the country, geographical trends were evident.  Random forest modeling was also used to compare variable importance in prediction of these two attributes.  Findings indicated that demographics (age, race, educational attainment) are more important to predicting health outcome than food desert status, and labor participation is more important to predicting food desert status than to health outcome. SNAP benefit participation strongly predicts both outcomes.  Overall, results indicated that while food environment, food desert status, and diabetes rate are related, certain factors associated with food desert status are more correlated with poor health outcomes than others.  
Keywords: food environment, food desert, USDA, clustering, BPCA, biplot, chloropleth, random forest

## SELECTED MAPS
(More images available in "pics"!)

<img src="https://github.com/stephc027/food-environments-2020/blob/main/pics/FDmap.png?raw=true" width="500" height="250">
<img src="https://github.com/stephc027/food-environments-2020/blob/main/pics/metro.png?raw=true" width="500" height="250">
<img src="https://github.com/stephc027/food-environments-2020/blob/main/pics/nonmetro.png?raw=true" width="500" height="250">

Winner, undergrad/graduate division of 2020 New England Statistical Society NextGen data science day poster competition.
Full article is pending journal review, link to publication to follow.
