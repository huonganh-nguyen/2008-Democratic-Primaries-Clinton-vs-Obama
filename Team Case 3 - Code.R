########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")
# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
### 
# bar graph
counts <- table(election_data_train$Obama_wins, election_data_train$Region)
prop = prop.table(counts, margin = 2)
barplot(prop, main="Obama's victory by region",
        col=c("gray", "blue"),
        legend = c("Obama lost", "Obama won"))
# line graph
plot(election_data_train$Age65andAbove, election_data_train$SocialSecurityRate, type = "p",
     xlab = "% of Population at Age 65 and Above", ylab = "Social Security Rate")
# line graph
plot(election_data_train$Bachelors, election_data_train$AverageIncome, type = "p",
     xlab = "% of Population Holding a Bachelor's Degree", ylab = "Average Income")
# line graph
plot(election_data_train$HighSchool, election_data_train$AverageIncome, type = "p",
     xlab = "% of Population Holding a High School Degree", ylab = "Average Income")

###
### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###
# Drop the identifier and the categorical variables with too many factors
drop_election_data_train <- c("FIPS", "State", "County")
df <- election_data_train[, !names(election_data_train) %in% drop_election_data_train]
# Convert region to a factor 
df$Region <- factor(df$Region)
# Make data frame for interactions 
df_interaction <- model.matrix(~.^2, df)
df_interaction <- data.frame(df_interaction)
# Look at a regression with interactions to get an idea to put in model
ints<- glm(Obama_margin_percent~., data=df_interaction)
(summary(ints))
# K-Folds to compare 2 linear models with various sets of variables
nfold <- 10
n <- nrow(df) 
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
OOS <- data.frame(linNoInt=rep(NA,nfold), linInt=rep(NA,nfold))
for(k in 1:nfold){ 
  train <- which(foldid!=k)
  model.linNoInt   <- glm(Obama_margin_percent~AgeBelow35+Age65andAbove+ White+ Black+ HighSchool+Bachelors+Poverty+IncomeAbove75K+ MedianIncome+AverageIncome+MedicareRate+SocialSecurityRate+ ManfEmploy+DisabilitiesRate+ UnemployRate+ Homeowner+ SameHouse1995and2000+Region,  data=df, subset=train)
  model.linInt <- glm(Obama_margin_percent~AverageIncome+RetiredWorkers+
                        RegionNortheast.ElectionTypePrimary+RegionSouth.UnemployRate+
                        RegionWest.SocialSecurity+ElectionDate.RetiredWorkers+ 
                        ElectionDate.AverageIncome+RegionSouth.FarmArea+                   RegionSouth.LandArea+RegionWest.Pop+RegionSouth.SameHouse1995and2000+RegionWest.Disabilities+ RegionSouth.Disabilities+ RegionWest.RetiredWorkers, data=df_interaction, subset=train)
  pred.linNoInt    <- predict(model.linNoInt, newdata=df[-train,], type="response")
  OOS$linNoInt[k]  <- R2(y=df$Obama_margin_percent[-train], pred=pred.linNoInt)
  OOS$linNoInt[k]
  pred.linInt      <- predict(model.linInt, newdata=df_interaction[-train,], type="response")
  OOS$linInt[k]    <- R2(y=df_interaction$Obama_margin_percent[-train], pred=pred.linInt)
  OOS$linInt[k]
}
colMeans(OOS)
m.OOS <- as.matrix(OOS)
rownames(m.OOS) <- c(1:nfold)
if (nfold >= 10){
  boxplot(OOS, col="plum", las = 2, ylab=expression(paste("OOS ",R^2)), xlab="", main="10-fold Cross Validation")
}
#Based on the smaller spread, we are going to use the model without interactions. 
ourModel <- glm(Obama_margin_percent~AgeBelow35+Age65andAbove+White+Black+HighSchool+Bachelors+Poverty+IncomeAbove75K +MedianIncome+ AverageIncome+ MedicareRate+SocialSecurityRate+ManfEmploy+DisabilitiesRate+ UnemployRate+ Homeowner+ SameHouse1995and2000+Region, data=df)
summary(ourModel)
election_data_test$prediction <- predict(ourModel, newdata=election_data_test, type="response")
summary(election_data_test)
##Range of predictions = -141.233 to 98.963 with a average of -16.324 

### Question 3

installpkg("ElemStatLearn")
library(ElemStatLearn)
installpkg("class")
library(class)

#Creating dataframe of only the numeric variables
testing_numeric <- sapply(election_data, is.numeric)
numeric_election <- election_data[,testing_numeric]

#Random seed for replication purposes
set.seed(1)
#Total votes tells the size of the county
#Here we set the radius so that the area of the bubble reflects the total votes
radius <- sqrt(numeric_election$TotalVote/pi)

#Creating variables for obama wins and clinton wins
numeric_election$Obama_wins_ratio <- numeric_election$Obama/numeric_election$TotalVote
numeric_election$Clinton_wins_ratio <- numeric_election$Clinton/numeric_election$TotalVote

################################################################################
#Clustering with Obama and Black

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "Black")]
x <- c("Obama")
y <- c("Black")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

################################################################################
#Clustering with Obama and White

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "White")]
x <- c("Obama")
y <- c("White")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

################################################################################
#Clustering with Obama and Asian

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "Asian")]
x <- c("Obama")
y <- c("Asian")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)


################################################################################
#Clustering with Obama and Hispanic

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "Hispanic")]
x <- c("Obama")
y <- c("Hispanic")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)


################################################################################
#Clustering with Obama and HighSchool

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "HighSchool")]
x <- c("Obama")
y <- c("HighSchool")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

################################################################################
#Clustering with Obama and Bachelors

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "Bachelors")]
x <- c("Obama")
y <- c("Bachelors")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

################################################################################
#Clustering with Obama and Poverty

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "Poverty")]
x <- c("Obama")
y <- c("Poverty")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

################################################################################
#Clustering with Obama and MedianIncome

#Selecting the variables of interest
ElectionPerformance <-numeric_election[c("Obama", "MedianIncome")]
x <- c("Obama")
y <- c("MedianIncome")

#Set the number of centers for k-means
num_centers <- 2

#Replacing na's with mean values
for(i in 1:2) {
  ElectionPerformance[,i]<-impute_data(ElectionPerformance[,i],data_mean[i-9])
}

### we need normalizaiton of the data
ElectionPerformance[,1] <- ( ElectionPerformance[,1] - mean(ElectionPerformance[,1]))/ sd(ElectionPerformance[,1])
ElectionPerformance[,2] <- ( ElectionPerformance[,2] - mean(ElectionPerformance[,2]))/ sd(ElectionPerformance[,2])

#Computing kmeans
numeric_election_kmeans <- kmeans(ElectionPerformance,num_centers)

colorcluster <- 1+numeric_election_kmeans$cluster

#Plotting the cluster
plot(ElectionPerformance, xlab=x, ylab=y, col = colorcluster)
symbols(ElectionPerformance,circles=radius, xlab=x, ylab=y, bg = colorcluster)
points(numeric_election_kmeans$centers, col = 2:4, pch = 8, cex = 2)

####Question 4
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic
election_data_train$Hispanic<-election_data_train$Hispanic+0.05
election_data_train$Black<-election_data_train$Black+0.05

y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

HispanicAdv  <- glm(Obama_margin_percent~Hispanic + AgeBelow35+Age65andAbove+White+Black+HighSchool+Bachelors+Poverty+           IncomeAbove75K+MedianIncome+AverageIncome+MedicareRate+SocialSecurityRate+ManfEmploy+                         DisabilitiesRate+UnemployRate+Homeowner+SameHouse1995and2000+Region, 
                    data=election_data_train)

summary(HispanicAdv)
####
### Question 4(b) impact of changing black demographic
####
#### Model with 1771 controls to measure the impact of 10% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 

election_data_train$Obama_margin_percent<-ln(Obama_margin_percent)
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)

BlackAdv   <- glm(Obama_margin_percent~AgeBelow35+Age65andAbove+White+Black+HighSchool+Bachelors+Poverty+           IncomeAbove75K+MedianIncome+AverageIncome+MedicareRate+SocialSecurityRate+ManfEmploy+                         DisabilitiesRate+UnemployRate+Homeowner+SameHouse1995and2000+Region, 
                  data=election_data_train)

summary(BlackAdv)

