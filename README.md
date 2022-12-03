# Re-Purchase Prediction Model

### Overview:

This report is aiming at providing suggestions on targeting existing customers for a re-purchase campaign for an automotive manufacturer. Exploratory Data Analysis (EDA) is performed on data set, both linear model and tree-based model are trained for selection and customers that are likely to re-purchase are predicted based on the random forest model which performs the best under this circumstance.


There are four models trained for comparison, namely logistic model, LASSO regression model, decision tree model and random forest model. 
Random forest prediction shows that 1,246 (2.5%) customers in ‘repurchase_validation.csv’ are with Target = 1 which means they predicted to be a re-purchaser. However, to ensure not a single re-purchaser is missed, the car manufacturer can prepare two versions of re-purchase communication. The brief version can be sent to all customers with Target = 0 and a detailed version can be sent to those with Target = 1.

**Linear models:**
- Logistic model considers all provided variables 
- LASSO model eliminates the effect of some variables that are not significantly related to Target. 

**Tree-based models:** 
- Decision tree model predict each observation based on the most commonly occurring class of training observations in its region while 
- Random forest can be considered as producing multiple decision trees then combine to yield a single consensus prediction.


### Output:
#### [Written Report](https://github.com/wenyingw/Repurchase-Prediction-Model/blob/main/report_repurchase_prediction_model.pdf)

> The approach used, assumptions and supporting rationale for each stage of the CRISP-DM framework. Results and recommendations, including supporting visualisations and summary data. Evaluate the results of different techniques, giving reasons for the final approach.

#### [Workfile](https://github.com/wenyingw/Repurchase-Prediction-Model/blob/main/workfile_repurchase_prediction_model.R)
> An appendix including working code

### Result:

Random forest prediction shows that 1,246 (2.5%) customers in ‘repurchase_validation’ are with Target = 1 which means they predicted to be a re-purchaser. However, to ensure not a single re-purchaser is missed, the car manufacturer can prepare two versions of re-purchase communication. The brief version can be sent to all customers with Target = 0 and a detailed version can be sent to those with Target = 1.


<sub><sup>Edit on Apr 18, 2020</sup></sub>