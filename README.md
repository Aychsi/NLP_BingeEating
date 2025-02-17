# Binge Eating Prediction Project

This repository contains a comprehensive R-based pipeline for predicting binge eating behaviors using data from CBT+ RCT records. The project leverages machine learning (ensemble methods, SVM, logistic regression, XGBoost), natural language processing (NLP), and rigorous data cleaning/optimization routines to model both loss-of-control (LOC) eating and compensatory behaviors (CompBx).

## Repository Structure

### Data Cleaning & Preparation
- **CompareData.R**  
  Compares and merges multiple data sources to verify consistency across datasets.

- **DataClean.R & DataCleanCompBX.R**  
  Clean and structure the LOC and CompBx datasets (e.g., converting dates, computing time features, and creating categorical predictors).

- **functions.R**  
  Contains helper functions for data preparation, balancing, model training, and performance evaluation.

### Modeling & Ensemble Methods
- **PredictLOC[51].R, RandomSplit.R, RandomSplitCompBx.R, SplitData.R, TimeElapsedEntryDelay.R, training.R, XGBoost_Combined.R, XGBoost.R**  
  Perform data splitting, feature engineering, and model training for LOC prediction using various algorithms and ensemble techniques.

- **NLP_CompBx.R & NLP.R**  
  Apply NLP (using udpipe) to extract informative, food-related features from text fields (e.g., `MS_FoodEaten`) to enhance prediction of binge eating and compensatory behaviors.

- **OptimizeGroupIndModel.R & OptimizePandSplit.R**  
  Optimize the weighting between group-level and individual-level prediction models as well as the data-splitting strategy based on time features.

- **PredictCompBx_AllPredictors_Group.R, PredictCompBx_ensemble_CombinedModels.R, PredictLOC_AllPredictors_Group.R, PredictLOC_Copy.R, PredictLOC_ensemble_CombinedModel[68].R, PredictLOC_ensemble_GroupLevelModel[22].R**  
  Build and evaluate ensemble models that combine predictions from group-level and individual-level approaches.

- **GroupModels.R**  
  Provides routines to build and save group-level prediction models by aggregating data across participants.

### Run Script
- **run.R**  
  Serves as an entry point to execute the full training and evaluation process (including both SVM and logistic regression pipelines) on the most recent data splits.

## Requirements

- **R (>= 3.6)**
- The following R packages (available via CRAN):
  - `dplyr`, `ROSE`, `caret`, `e1071`, `caretEnsemble`, `randomForest`, `xgboost`, `glmnet`, `udpipe`, `stringr`, `VIM`, `data.table`, `caTools`, `plyr`, `party`, `RWeka`

*Note:* Adjust file paths in the scripts to match your local data directories. Data files are not included due to privacy restrictions.

## Usage

1. **Data Preparation:**  
   Run the data cleaning scripts (e.g., `DataClean.R`, `DataCleanCompBX.R`) to generate clean, structured datasets.

2. **Feature Extraction:**  
   Execute the NLP scripts (`NLP.R`, `NLP_CompBx.R`) to extract food-related features that enhance model performance.

3. **Model Training & Optimization:**  
   Use the training and optimization scripts (e.g., `PredictLOC[51].R`, `training.R`, `XGBoost_Combined.R`, `OptimizeGroupIndModel.R`, `OptimizePandSplit.R`) to split the data, train ensemble models, and optimize the weighting between group-level and individual-level predictions.

4. **Prediction & Evaluation:**  
   Run the ensemble prediction scripts (e.g., `PredictCompBx_AllPredictors_Group.R`, `PredictLOC_AllPredictors_Group.R`, `PredictLOC_ensemble_CombinedModel[68].R`, `PredictLOC_ensemble_GroupLevelModel[22].R`) to generate predictions and assess performance using confusion matrices and other metrics.

5. **Full Pipeline Execution:**  
   Run the `run.R` file to execute the complete prediction pipeline from data preparation to evaluation.

## Contributing

Contributions, improvements, and bug fixes are welcome. Please fork the repository and submit pull requests. For major changes, open an issue first to discuss your ideas.

## License

[Insert license information here.]
