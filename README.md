# Predicting-the-Premier-League-Table-Using-Multiple-Regression
In this project we develop a multiple regression model to attempt to predict the final standings of the premier league table.
A more in depth report can be found on my LinkedIn page in the projects section here www.linkedin.com/in/aaron-moorey-065302297

## Description of what the code does
We first begin by checking for multicollinearity in the data by looking at each variables VIF values and removing the variables which have VIFs larger than 10.
Next we plot a studentised residual plot to check for outliers, as well as check for influentual points using metric such as Cook's distance. The observation corresponding to Leicster City winning the league in 2015/16 is removed as a consequence.

We now perform subset selection (forward and backward) on the dataset, and obtain 3 possible models. They contain 9, 8 and 7 independent variables. The graphs plotted show how we came to these 3 possible models where we used metrics such as adjusted R-squared and BIC to assess the quality of the model. To further assess the quality of these 3 models we compare the training and test RMSEs for them, as well as the number of clubs' positions correctly predicted for the testing data. After this we see the 8 variable model is best, mainly due to the fact it has the lowest test RMSE.

Finally, we view partial regression plots for each variable and see it is suggested there is some non-linearity in the PTS variable. We therefore add a PTS^2 variable to the model and see the partial regression plots now look more linear. After assessing the final model including this new squared term we see the test RMSE has decreased and hence use this as our final model, which we can see predicts 9 out of 20 teams' final standings correctly for our test data.
