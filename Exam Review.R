###
## For exam tips, please go to the following link: https://tinyurl.com/r-exam-tips
###

#Set working directory 
setwd("~/Documents/Teaching/Psych 101")
#You can do this the point and click way, but if you use the "Import Dataset" button MAKE SURE to select Header: "yes".

#Load necessary packages
library(gplots)
library(psych)

#Introduction to the data
#This data was collected in 1987 by Chilean organization interested in examining factors that were associated with income. Of interest to us are the "sex", "age", and "income" variables, as well as the three items measuring competence on the job: "Comp1","Comp2","Comp3R". "Comp3R" is reverse coded & the maximum possible score for each comptenece item is 10. 

##### Load the data #####
Chile_Data <- read.csv("Chile.csv")

##### Examine distributions and frequencies and remove outliers/fix any issues with our levels, if necessary #####

#First, let's make sure our variables are the correct class.
class(Chile_Data$age)
class(Chile_Data$income)
class(Chile_Data$Comp1)
class(Chile_Data$Comp2)
class(Chile_Data$Comp3R)
#Our continuous variables look good

class(Chile_Data$sex)
#Uh oh, this is being registered as a "character".

Chile_Data$sex <- as.factor(Chile_Data$sex)
#Over write the existing varibale with a factor version of the same variable

class(Chile_Data$sex)
#Looks good


#Examining the distribution of our continuous variables
hist(Chile_Data$Comp1)
hist(Chile_Data$Comp2)
hist(Chile_Data$Comp3R)

hist(Chile_Data$income)
hist(Chile_Data$age)
#hmmm, these last two look a little bit weird. Let's use plot to see if there are any outliers we want to get rid of!

plot(Chile_Data$income)
#Nothing obvious to remove

plot(Chile_Data$age)
#Definitely some odd values here. 4 data points have age values of over 600!


#We're going to use indexing to remove these outliers. Remember, indexing is used to subset (or take part of) an existing data structure. It takes the following form: data[ROW,COLUMN]
#You can put a minus before either the row or the column section to REMOVE the listed row or column
#If you leave  the row or column section blank, it will include ALL rows or columns
#We're also going to use the which() function. which() searching through data to see which rows match a certain criterion

Chile_Data <- Chile_Data[-which(Chile_Data$age > 600),]
#Data | find rows in the age column who have a value over 600, minus sign means we remove them | column section is blank, so include all columns

plot(Chile_Data$age)
#That looks much better! 

#Examining the frequency of our categorical variable
plot(Chile_Data$sex)
#visually

summary(Chile_Data$sex)
#numerically, method 1
table(Chile_Data$sex)
#numerically, method 2


##### Calculate alpha for proposed scale items, interpret alpha, score the scale, and provide descriptive statistics for the scored scale#####

#Before we start wokring with our scale, let's reverse score our reverse keyed item
Chile_Data$Comp3R <- 11 - Chile_Data$Comp3R

#Now, let's make a dataframe to compute alpha

#Method 1
Competence_Scale <- Chile_Data[,c("Comp1","Comp2","Comp3R")]
#We use our same indexing rules to select all rows, and we make a list of the column names we want to include with c()
#Retains the names from our original dataframe

#Method 2
Competence_Scale <- data.frame(Chile_Data$Comp1,Chile_Data$Comp2,Chile_Data$Comp3R)
#Tell R to make a data.frame with the data.frame() function & then tell it what the columns should be.
#Uses slightly different names for the columns.

#Method 3
Competence_Scale <- cbind("Comp1" = Chile_Data$Comp1,"Comp2" = Chile_Data$Comp2, "Comp3R" = Chile_Data$Comp3R)
#Column bind (cbind) the existing columns together. We need to provide a name for each column. 


#compute alpha
alpha(Competence_Scale)
#Is the raw alpha close to one? Yes, 0.97! Our scale is reliable (i.e., People answer our separate items for competence similarly)


#Now, let's score our scale and add it to our data frame
Chile_Data$Comp_Scored <- rowMeans(Competence_Scale) 

#Providing descriptive statistics & visualizing our scored scale (from the psych package. If you're having trouble, try: psych::describe)
describe(Chile_Data$Comp_Scored)

hist(Chile_Data$Comp_Scored)


##### For each hypothesis, please report the slope, intercept, and R^2, providing interpretations for each. Then, report whether we should confirm or reject the null hypothesis and plot the model#####


##### The first hypothesis the researchers had was: Competence will be positively associated with income. #####
# Null: There is not a positive relationship between income and competence 
# Alternative: The relationship between income and competence will be positive 

### For NHST###: 
#Null:The relationship/association/slope between income and comptence is 0 (b = 0)
#Alternative:The relationship/association/slope between income and competence is not 0 (b =/= 0)



comp_mod <- lm(income ~ Comp_Scored, data = Chile_Data)
summary(comp_mod)
#slope = 3169.3
#For each 1-point increase in our comptence scale (1 unit increase in our predictor) the estimated income increases by 3169.3

#The p-value for this relationship is 0.00000000000869. The relationship IS significant (p is smaller than 0.05), so we reject the null hypothesis. Competence IS  significantly associated with income.

#intercept = 15128.0
#The predicted value of income, for someone who scored 0 on the competency scale  is $15128.0

#R^2 = 0.0182
#Our model explains 1.82% more variance in our outcome than a model using JUST mean income as our predictor.
#This doesn't explain a lot of variation in our outcome, but it does offer some improvement over the mean model.

#Remember, the highest possible R^2 value is 1. It can be negative if your model does worse than the "mean model".



plot(income ~ Comp_Scored, data =Chile_Data)
#Plot the relationship between our variables
abline(comp_mod, col = "red", lwd = 3)
#Plotting the regression line


#Let's Zoom in to see the intercept and slope better!
plot(income ~ Comp_Scored, data =Chile_Data, xlim = c(0,2), ylim = c(15128.0,15128.0+(3169.3*2)))
abline(comp_mod, col = "red", lwd = 3)


##### The second hypothesis the researchers had was: Age will be positively associated with income. #####
# Null: There is not a positive relationship between age and income 
# Alternative: The relationship between age and income will be positive 

### For NHST###: 
#Null:The relationship/association/slope between age and income is 0 (b = 0)
#Alternative:The relationship/association/slope between age and income is not 0 (b =/= 0)



age_mod <- lm(income ~ age, data = Chile_Data)
summary(age_mod)
#slope = -84.92
#For each year increase in age (1 unit increase in our predictor) the estimated income decreases by 84.92

#The p-value for this relationship is 0.113. The relationship is not significant (p is greater than 0.05), so we fail to reject the null hypothesis. Age is not significantly associated with income.

#intercept = 37271.97
#The predicted value of income, when age is 0 (not very meaningful) is $37271.97

#R^2 = 0.0009892
#Our model explains 0.09892% more variance in our outcome than a model using JUST mean income as our predictor.
#This is a pretty awful model!


plot(income ~ age, data =Chile_Data)
#Plot the relationship between our variables
abline(age_mod, col = "red", lwd = 3)
#Plotting the regression line



##### The third hypothesis the researchers had was: Men will have higher incomes, on average, than women #####
# Null: Men are not higher paid than women.
# Alternative: Men are paid higher, on average, than women.

### For NHST###: 
#Null:The relationship/association/slope between sex and income is 0 (b = 0)
#Alternative:The relationship/association/slope between sex and income is not 0 (b =/= 0)

#First, let's set our reference level so female.
#Remember, the reference level is the category in your variable that will be hidden in the "intercept"
#The slope will then represent the DIFFERENCE between your reference level/intercept and the category represented by the slope
#In this case, that means our slope will represent how the estimate for men differs from that of  women.

Chile_Data$sex <- relevel(Chile_Data$sex, ref = "F")

sex_mod <- lm(income ~ sex, data = Chile_Data)
summary(sex_mod)

#intercept = 32168
#The mean income for women in our sample is $32,168

#slope = 3706
#The mean income for men in our sample is $3,706 HIGHER than that of women. Mean for men = 32,168 + 3706 = 35,874

#The p-value for this difference is 0.0188. The difference IS  significant (p is smaller than 0.05), so we reject the null hypothesis. Income does differ significantly by sex. 

#R^2 = 0.002173
#Our model explains 0.2173% more variance in our outcome than a model using JUST mean income as our predictor.
#Yay! Despite the difference being significant, sex only explains a tiny tiny variation in income (in other words, for women in 1987 in Chile even though sex did have an impact on income other factors had much much larger impacts). 


plotmeans(income ~ sex, data = Chile_Data)
#Plot the relationship between our variables
#Note that the line is actually our regression line and we can use it to identify the intercept and the slope visually.

plotmeans(income ~ sex, data = Chile_Data, connect = F)
#Without the line. 


#### Let's Bootstrap one of our models ####
# A second way to test, "How reliable is our slope?" is to use bootstrapping. 

# The idea behind bootstrapping is to pretend our sample is the population and sample over and over again from it with replacement, caluclate the lm, get the slope, and do it again x 1000! Then we can plot a distribution of our slopes, find the mean (our overall estimate of the slope) and the standard deviation of our slopes.

# define an empty vector named slopes; this is just a place to put things from the for loop. You ALWAYS need an empty data structure when using for loops so R has somewhere to put the values it creates

bootstrapped_slopes <- vector()
#We're creating an empty vector using vector() and naming it exactly what we're going to store inside -- our bootstrapped slopes!


for(i in 1:1000){
  
  #We’re doing 1000 iterations because want to make 1000 resampled data frames and 1000 slopes
  resampled_data <- Chile_Data[sample(1:nrow(Chile_Data), size = nrow(Chile_Data), replace = T),]
  
  # This is the generation of our new data frame, which we’re then saving as an intermediate variable called “resampled_data”.
  
  #There are two things going on here, the first is sampling with the sample() function and the second is indexing. Let’s start with the sample() function.
  
  #The resampling is done using sample() which is a function which takes 3 arguments here.
  
  #Argument 1: What vector of values do you want to sample from.
  
  #By sampling we mean pick values at random.
  
  #Argument 2: How many values do you want to pick at random?
  
  #Argument 3: After each random pick, do you want to replace the value you picked. This will always be true if you’re bootstrapping.
  
  #In plain English, we want to pick at random from a list of numbers ranging from 1 through the number of rows in our data frame, Chile_data We want to make random picks until we have as many random picks as there are rows in our data frame, Chile_data After each pick, we want to replace any picked numbers before the next pick. Effectively, every time we make a random pick we pick on a brand new vector ranging from 1 through the number of rows in our data frame, Chile_data This means that we can have repeated picks.
  
  #Now, the second part is that the sample is being used as a ROW index. You can see we have: Chile_data[SAMPLE,]
  
  #This means our resampled data is going to be composed of the rows in Chile_data, that were specified by our sample() function. Note, that’s why we sampled from 1:nrow(Chile_data).
  
  model <- lm(income ~ sex, data = resampled_data)
  
  #Another intermediate variable: Our model! Note that we use resampled_data when we fit the model.
  
  bootstrapped_slopes[i] <- coef(model)[2]
  
  #Our output vector contains the second coefficient from our intermediate variable, model If you were interested in the intercept it would be: coef(model)[1] instead.
}

# What is the distribution of slopes for the 1000 models from the resampled data?
hist(bootstrapped_slopes)

#What is our average or mean slope in our 1000 bootstrapped samples
mean(bootstrapped_slopes) # mean of the 1000 slopes we got from the random samples.
sex_mod$coefficients[2]
# HEY, this looks very similar to the original slope.

#Let's plot the mean on the histogram
abline(v = mean(bootstrapped_slopes), col = 'red', lwd = 3)

#Now, let's use two methods to test the reliability of our estimate. 
#1) The 95% confidence interval: The 95% confidence interval gives us a range of estimates for our slope that we're 95% confident contains the true slope (i.e., the real relationship between our two variables)
#If our 95% confidence interval DOES NOT contain 0, we consider the observed relationship significant.

#To calculate the 95% confidence interval we need the mean of our sampling distribution and the standard deviation
bootstrapped_mean <- mean(bootstrapped_slopes)
bootstrapped_sd <- sd(bootstrapped_slopes)

upper_bound <- bootstrapped_mean + (bootstrapped_sd*1.96)
#Upper bound is given by mean + sd*1.96

lower_bound <- bootstrapped_mean - (bootstrapped_sd*1.96)
#Lower bound is given by mean - sd*1.96

lower_bound
upper_bound
#Doesn't contain 0! We can conclude that there IS a relationship between sex and income. 

#2) The proportion of slopes in the same direction as our original slope: Obviously, the higher the % of slopes in the same direction as our original slope, the more reliable the relationship is. 

sum(bootstrapped_slopes > 0)/1000 
#We count all the bootstrapped slopes that are greater than 0 (same direction as our original slope) and divide by the total number of bootstrapped samples to get a proportion.

#99.3% of the slopes are in the same direction as the original slope! The relationship we found is very reliable! 
