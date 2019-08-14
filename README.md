# Classification for Customer brand preference

### General description
This project is about complete the missing data of brand preference (Sony and Acer) of the customer survey. 

### Data set
Two data sets were provided.

1, Complete data has 9898 observations.

2, Incomplete data has 5000 observations.

Both data sets have attributes of 
- salary
- age
- elevel 
- car
- zipcode 
- credit 
- and only Complete data has brand preference Information.

### Methodes of analysis
The number of attributes was reduced to 5 from 7 based on the variance importance. 
Randomforest and C5.0 were applied to the modeling. 
In the end, since C5.0 model obtained higher accuracy than Randomforest model,
The prediction of Brand preference was made based on C5.0 model.
