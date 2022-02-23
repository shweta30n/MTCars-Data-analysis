library(readxl)
MT_cars<- read_excel("R Test &amp; Case study.xlsx", 
                                    sheet = "Question 2 Data - mtcars")
View(MT_cars)
mtcars_df = as.data.frame(MT_cars)
row.names(mtcars_df) <- MT_cars$CarModel
mtcars_df$CarModel <-NULL

class(mtcars_df)

                                                                


##################Exploratory analysis#################
#create EDA report,It provides accurate details about data with all detail such as variance, correlation among each perameters of dataset.
library(DataExplorer)
DataExplorer::create_report(mtcars_df)
###analysis by pairwise scatterplot and pairpanel plot to find correlation and coefficient among variables.
pairs(mtcars_df)
install.packages("psych")
library(psych)
pairs.panels (mtcars_df)



##############
library(datasets)
install.packages("car")
install.packages("knitr")
library(car)
library(dplyr)
library(knitr)
library(ggplot2)
unique(mtcars_df$am)

#####In dataset variable 'am' represents automatic with value 0 or manual transmission with 1 
#### Variable mpg(mile per gallon)will remain as dependent variable which can direct impact to other variable specifically "am"variable as we are going to analyz relation between these variables.

#statiscal analyse
with(MT_cars, plot(am, mpg, xlab = 'Automatic or Manual', ylab = 'Miles Per Gallon', main = 'Transmission Type vs. MPG', xaxt = 'no'))
axis(1, at = c(0, 1))



MT_car_table <- mtcars_df  %>%
  group_by(am) %>%
  summarise(n = n(),
            min = min(mpg),
            q1 = quantile(mpg, 0.25),
            median = median(mpg),
            mean_mpg = mean(mpg),
            q3 = quantile(mpg, 0.75),
            max = max(mpg),
            sd_mpg = sd(mpg))
View(MT_car_table)

####hypothesis###

####In plot on x-axis, 1 = manual and 0 = automatic. MPG with an manual transmission seems to be higher:As suggested in stastical and plot analyse that the median,Q1 and Q3 are higher with manual cars than automatic cars,but datasets refere some automatic cars in the dataset with higher mpg than manual cars.
###To determine manual transmission is better for mpg in comparison to automatic transmission by hypothesis test.
###here i will follow null hypothesis and alternative hypothesis.
###Null hypothesis refered to mean mpg of automatic car equal to manual cars.
###Alternative hypothesis refered to mean mpg of manual cars is greater than automatic cars.
###Now we perform a t-test to confirm accurate hypothesis.
t.test(mtcars_df$mpg~mtcars_df$am,conf.level =0.95)
###After t test analyses p-value is 0.001374 which below than the significance level 0.05,so we reject the null hypothesis.it mean manual cars are better with mpg than automatic cars.
########quantify the MPG different between automatic and manual transmission.Also find the other variables that create Mpg difference#######
####we try Multivariate linear regression with all variable in binary data###

###Regression model####

mlr <-lm(data =  mtcars_df,mpg ~.)
summary(mlr)
# the Multivariate liner regression provided "wt" factor also has change with "mpg.In addition, involvment of all variable cause overfitting which lead to test different model with different variables.
# we will use automatic variable selection function called step to choose best linear regression model.
###
best_model <- step(lm(data = mtcars_df, mpg ~.),
  trace= 0)
summary(best_model)
#####Best model includes am,qsec,wt it refered that other factor besides transmission types, weight and 1/4mile time change with mpg.
### qsec and am positvly change with mpg and weight change negatively with mpg.
### an increase of every 1000weight decrease the 3.9mpg , increase in 1/4mile time increase 1.2mpg and manual transmission is 2.9 more than automatic transmission.
####mutiple R-squared show 85% variance,residual plots are also randomly scattered.
par(mfrow =c(2,4))
plot(best_model)

####conclusion###
# manual transmission is much better than the auromatic transmission by2.9mpg
# other factor are such as weight and 1/4mile time also are value create with mpg other than transmission type.
