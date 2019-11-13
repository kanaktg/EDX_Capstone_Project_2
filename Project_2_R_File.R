# set working directory
setwd("C:/Users/Tyagi/Downloads")
getwd()

# Load the relevant packages.
if(!require(MASS)){install.packages('MASS')}
if(!require(dplyr)){install.packages('dplyr')}
if(!require(ggplot2)){install.packages('ggplot2')}
if(!require(BAS)){install.packages('BAS')}
if(!require(GGally)){install.packages('GGally')}
if(!require(car)){install.packages('car')}
if(!require(conover.test)){install.packages('conover.test')}
if(!require(kableExtra)){install.packages('kableExtra')}
if(!require(gridExtra)){install.packages('gridExtra')}
if(!require(ggpubr)){install.packages('ggpubr')}
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")


#Load Dataset
load('ames_train.RData')



################## Part-1: Exploratory Data Analysis ###################

## the dataset and its basic summary statistics
#Dimensions of the data
dim(ames_train)

# intial 7 rows with header
head(ames_train)


# basic summary statistics
summary(ames_train)



######### Cleaning the Data ##########
ames_train <- ames_train %>% 
  mutate(Alley = if_else(is.na(Alley), 'No Alley Access', as.character(Alley)),
         Bsmt.Qual = if_else(is.na(Bsmt.Qual), 'No Basement', as.character(Bsmt.Qual)),
         Bsmt.Cond = if_else(is.na(Bsmt.Cond), 'No Basement', as.character(Bsmt.Cond)),
         Bsmt.Exposure = if_else(is.na(Bsmt.Exposure), 'No Basement', as.character(Bsmt.Cond)),
         BsmtFin.Type.1 = if_else(is.na(BsmtFin.Type.1), 'No Basement', as.character(BsmtFin.Type.1)),
         BsmtFin.Type.2 = if_else(is.na(BsmtFin.Type.2), 'No Basement', as.character(BsmtFin.Type.2)),
         Fireplace.Qu = if_else(is.na(Fireplace.Qu), 'No Fireplace', as.character(Fireplace.Qu)),
         Garage.Type = if_else(is.na(Garage.Type), 'No Garage', as.character(Garage.Type)),
         Garage.Finish = if_else(is.na(Garage.Finish), 'No Garage', as.character(Garage.Finish)),
         Garage.Qual = if_else(is.na(Garage.Qual), 'No Garage', as.character(Garage.Qual)),
         Garage.Cond = if_else(is.na(Garage.Cond), 'No Garage', as.character(Garage.Cond)),
         Pool.QC = if_else(is.na(Pool.QC), 'No Pool', as.character(Pool.QC)),
         Fence = if_else(is.na(Fence), 'No Fence', as.character(Fence)),
         Misc.Feature = if_else(is.na(Misc.Feature), 'No Misc Features', as.character(Misc.Feature)),
         Years.Old = 2018 - Year.Built,
         MS.SubClass = as.factor(MS.SubClass),
         Overall.Qual = as.factor(Overall.Qual),
         Overall.Cond = as.factor(Overall.Cond),
         log.price = log(price),
         log.Lot.Area = log(Lot.Area))

ames_train <- ames_train %>% 
  filter(Sale.Condition == 'Normal')



############## Plots #########################
#Plot-1 : Evaluating the effect of MS.SubClass on price
ggplot(ames_train, aes(x = MS.SubClass, y = log.price, fill = MS.SubClass)) + geom_boxplot() + theme_bw() + scale_fill_discrete(guide=FALSE) + labs(title='Evaluating the effect of MS.SubClass on the log of price', x='Type of Dwelling involved in the sale', y='Logarithm of the Price')


#Plot-2 : Effect of when the house was built (Year.Built) and House Style (House.Style) on log of price
ggplot(ames_train, aes(x = Year.Built, y = log.price, col=House.Style)) + geom_point() + theme_bw() + labs(title='Effect of when the house was built and House Style on the log of price', x = 'Year the house was built', y = 'Logarithm of Price')


#Plot-3 : Effect of Heating Quality and condition on the Log Price
ggplot(ames_train, aes(x = Heating.QC, y = log.price, fill=Heating.QC)) + geom_boxplot() + theme_bw() + labs(title='Effect of Heating Quality and Condition on the log of price', x = 'Heating Quality', y = 'Logarithm of Price') + scale_fill_discrete(guide=FALSE)






## Development and assessment of an initial model, following a semi-guided process of analysis
# An Initial Model
initial_model <- lm(log(price) ~ log(Lot.Area) + MS.SubClass + Overall.Qual + Overall.Cond + Heating.QC + Year.Built + House.Style + Neighborhood + Exterior.1st + X1st.Flr.SF, data = ames_train)

summary(initial_model)


###Model Selection

#AIC
initial_model_AIC <- stepAIC(initial_model, direction = 'backward', trace = FALSE)
summary(initial_model_AIC)


#BIC
initial_model_BIC <- stepAIC(initial_model, direction='backward', k = log(nrow(ames_train)), trace = FALSE)
summary(initial_model_BIC)


# Initial Model Residuals
par(mfrow=c(2,2))
plot(initial_model_AIC)


# Initial Model RMSE
# Extract Predictions
predictions_initial <- exp(predict(initial_model_AIC, ames_train))

# Extract Residuals
residuals_initial <- ames_train$price - predictions_initial

# Calculate RMSE
rmse_initial <- sqrt(mean(residuals_initial^2))
rmse_initial



##Overfitting
load("ames_test.Rdata")

ames_test <- ames_test %>% 
  mutate(Alley = if_else(is.na(Alley), 'No Alley Access', as.character(Alley)),
         Bsmt.Qual = if_else(is.na(Bsmt.Qual), 'No Basement', as.character(Bsmt.Qual)),
         Bsmt.Cond = if_else(is.na(Bsmt.Cond), 'No Basement', as.character(Bsmt.Cond)),
         Bsmt.Exposure = if_else(is.na(Bsmt.Exposure), 'No Basement', as.character(Bsmt.Cond)),
         BsmtFin.Type.1 = if_else(is.na(BsmtFin.Type.1), 'No Basement', as.character(BsmtFin.Type.1)),
         BsmtFin.Type.2 = if_else(is.na(BsmtFin.Type.2), 'No Basement', as.character(BsmtFin.Type.2)),
         Fireplace.Qu = if_else(is.na(Fireplace.Qu), 'No Fireplace', as.character(Fireplace.Qu)),
         Garage.Type = if_else(is.na(Garage.Type), 'No Garage', as.character(Garage.Type)),
         Garage.Finish = if_else(is.na(Garage.Finish), 'No Garage', as.character(Garage.Finish)),
         Garage.Qual = if_else(is.na(Garage.Qual), 'No Garage', as.character(Garage.Qual)),
         Garage.Cond = if_else(is.na(Garage.Cond), 'No Garage', as.character(Garage.Cond)),
         Pool.QC = if_else(is.na(Pool.QC), 'No Pool', as.character(Pool.QC)),
         Fence = if_else(is.na(Fence), 'No Fence', as.character(Fence)),
         Misc.Feature = if_else(is.na(Misc.Feature), 'No Misc Features', as.character(Misc.Feature)),
         Years.Old = 2018 - Year.Built,
         MS.SubClass = as.factor(MS.SubClass),
         Overall.Qual = as.factor(Overall.Qual),
         Overall.Cond = as.factor(Overall.Cond),
         log.price = log(price),
         log.Lot.Area = log(Lot.Area))

ames_test <- ames_test %>% 
  filter(House.Style != '2.5Fin') %>% 
  filter(Neighborhood != 'Landmrk') %>% 
  filter(Exterior.1st != 'AsphShn')


predictions_test <- exp(predict(initial_model_AIC,ames_test))
residuals_test <- ames_test$price - predictions_test

rmse_test <- sqrt(mean(residuals_test^2))
rmse_test






########################### Final Model ######################################


final_model <- lm(log(price) ~ log(Lot.Area) + MS.SubClass + Overall.Qual + 
                    Overall.Cond + Heating.QC + Year.Built + House.Style + Neighborhood + 
                    Exterior.1st + X1st.Flr.SF + Year.Remod.Add + Bsmt.Qual + Garage.Area + Pool.QC, data = ames_train)



# Now to compare if the AIC criteria also chooses the same model using backward step selection:
final_model_AIC <- step(final_model, direction='backward', trace = FALSE)
final_model_AIC



#The AIC criteria also chooses the same model with the 14 selected predictor variables chosen. 
#Now to compare this model with the initial one and check if it truly is better than the initial.
anova(initial_model_AIC, final_model_AIC)


#The ANOVA results show that the new variables increase the predictive power of this new model compared to the older one. The final model summary thus is:
  
summary(final_model_AIC)




## Transformation

# Lot.Area needs to be log transformed as well as price due to their skeweness. The same doesn't happen with any other other variables used in the model so there is no need for transformations.
# To prove this is right, plotting the other continuous predictor variables used in the model.

ggplot(ames_train, aes(x = Garage.Area, y = log(price))) + geom_point() + theme_bw() + geom_smooth(method='lm', se = FALSE)




#Final Model Assessment
par(mfrow=c(2,2))
plot(final_model_AIC)


#Final Model RMSE
ames_test <- ames_test %>% 
  filter(Pool.QC != 'TA')

predictions_final_test <- exp(predict(final_model_AIC,ames_test))
residuals_final_test <- ames_test$price - predictions_final_test

rmse_final_test <- sqrt(mean(residuals_final_test^2))
rmse_final_test




# Final Model Validation
load("ames_validation.Rdata")

ames_validation <- ames_validation %>% 
  mutate(Alley = if_else(is.na(Alley), 'No Alley Access', as.character(Alley)),
         Bsmt.Qual = if_else(is.na(Bsmt.Qual), 'No Basement', as.character(Bsmt.Qual)),
         Bsmt.Cond = if_else(is.na(Bsmt.Cond), 'No Basement', as.character(Bsmt.Cond)),
         Bsmt.Exposure = if_else(is.na(Bsmt.Exposure), 'No Basement', as.character(Bsmt.Cond)),
         BsmtFin.Type.1 = if_else(is.na(BsmtFin.Type.1), 'No Basement', as.character(BsmtFin.Type.1)),
         BsmtFin.Type.2 = if_else(is.na(BsmtFin.Type.2), 'No Basement', as.character(BsmtFin.Type.2)),
         Fireplace.Qu = if_else(is.na(Fireplace.Qu), 'No Fireplace', as.character(Fireplace.Qu)),
         Garage.Type = if_else(is.na(Garage.Type), 'No Garage', as.character(Garage.Type)),
         Garage.Finish = if_else(is.na(Garage.Finish), 'No Garage', as.character(Garage.Finish)),
         Garage.Qual = if_else(is.na(Garage.Qual), 'No Garage', as.character(Garage.Qual)),
         Garage.Cond = if_else(is.na(Garage.Cond), 'No Garage', as.character(Garage.Cond)),
         Pool.QC = if_else(is.na(Pool.QC), 'No Pool', as.character(Pool.QC)),
         Fence = if_else(is.na(Fence), 'No Fence', as.character(Fence)),
         Misc.Feature = if_else(is.na(Misc.Feature), 'No Misc Features', as.character(Misc.Feature)),
         Years.Old = 2018 - Year.Built,
         MS.SubClass = as.factor(MS.SubClass),
         Overall.Qual = as.factor(Overall.Qual),
         Overall.Cond = as.factor(Overall.Cond),
         log.price = log(price),
         log.Lot.Area = log(Lot.Area))
ames_validation <- ames_validation %>% 
  filter(House.Style != '2.5Fin') %>% 
  filter(Exterior.1st != 'CBlock' & Exterior.1st != 'PreCast') %>% 
  filter(Pool.QC != 'TA') %>% 
  filter(MS.SubClass != '150')

predictions_validation <- exp(predict(final_model_AIC,ames_validation))
residuals_validation <- ames_validation$price - predictions_validation

rmse_validation <- sqrt(mean(residuals_validation^2))
rmse_validation



# Percentage of the 95% predictive confidence intervals that contain the true price of the house in the validation data set:
# Predict prices
predict.full.CI <- exp(predict(final_model_AIC, ames_validation, interval = "prediction", level=0.95))

# Calculate proportion of observations that fall within prediction intervals
coverage.prob.full <- mean(ames_validation$price > predict.full.CI[,"lwr"] &
                             ames_validation$price < predict.full.CI[,"upr"])
coverage.prob.full

  