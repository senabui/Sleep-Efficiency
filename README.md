# Sleep Efficiency Analysis & Interventions

#### This sleep efficiency project leverages machine learning and analysis to identify the key determinants of sleep efficiency. Our target metric represents the percentage of time in bed actually spent sleeping. By analyzing a comprehensive dataset that includes sleep architecture, lifestyle factors, and demographic information, we aim to develop predictive models that can accurately classify sleep efficiency and inform targeted interventions. The findings within this project hope to provide insight to the recent advances in wearable technology and mobile health appications, which have created unprecedented opportunities to monitor, analyze, and improve sleep patterns at both individual and population levels. 

The completion of this project was done using R. 

Variable importance of sleep factors affecting percentage of time in bed include the following: 

<img src="images/Variable Importance.png" width=800>

Classification was performed using Random Forest Model to classify each person's sleep efficiency into target variables of 'Low', 'Optimal', or 'High'. Given the class imbalance, SMOTE was applied onto our preprocessed dataset to balance out our three classes. Model accuracy is 91.2%, with strong class-specific performance (sensitivity ranging from 88,9% to 95.8% across all three classes). When applied to the original unbalanced dataset, accuracy increased to 97.1% with particularly strong performance on the 'Low' class. 

<img src="images/Confusion Matrix SMOTE.png" width=800>

Multiple regression was performed to identify the best predictors, measure predictor significance, and evaluate how accurately they can predict sleep efficiency. All three feature selection techniques results in the same significant features: percentage of light sleep, awakenings, alcohol consumption, smoking status, and exercise frequency. 

<img src="images/Multiple Regression .png" width=800>



