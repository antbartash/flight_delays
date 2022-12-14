# Flight delays analysis
The aim of the project is to analyse delays of the US domestic flights using the R language. 

Since depature delays are on average longer than arrival delays and generate higher costs, the target variable describes departure delays. The flight is considered to be delayed if the difference between actual and schedulled time of depature is higher than 15 minutes.

## Data sources
The data used in the project was provided by the Bureau of Transportation Statistics, National Centers for Environmental Information, and Federal Aviation Administration. <br>

## Predicting delays
In order to predict flight delays, decision trees, random forests and AdaBoost models were built. Comparison of the final model (models with tuned hyperparameters) can be found in final_model.R file.

Predictions made with the final models can be found in the predictions folder. The plots folder contains Power BI visualisations of the features used in the analysis, as well as visualisations concerning hyperparameters tuning, models and their quality.
