# Regression Analysis for Ticket Sales
rm(list=ls())
library(ggplot2)
library(car)

dat = read.csv('cinema_hall_ticket_sales.csv')  # read data
dat = dat[0:500,]  # take first 500 rows to reduce computation for simple demo

# =================== Exploratory Analysis =====================
# find data types
for (i in colnames(dat)){
  cat(i, ': ', class(dat[[i]]), '\n')
}

# clean and correct data types
dat$Number_of_Person[which(dat$Number_of_Person=="Alone")] = 1

factor_cols = list('Number_of_Person', 'Movie_Genre', 'Seat_Type', 'Purchase_Again')
for (col in factor_cols){
  dat[[col]] = as.factor(dat[[col]])
}

# let's reduce some of the levels for a more specific analysis
dat$Single_or_Group = ifelse(dat$Number_of_Person == 1, 0, 1)  # create column showing single ticket or group
dat$Single_or_Group = as.factor(dat$Single_or_Group)

dat$Age_Group = ifelse(dat$Age < 30.0, 0, 1)  # create young adult vs. other ages group
dat$Age_Group = as.factor(dat$Age_Group)

dat$Bin_Seat_Type = ifelse(dat$Seat_Type == 'Standard', 0, 1)  # basic ticket vs. upgraded
dat$Bin_Seat_Type = as.factor(dat$Bin_Seat_Type)

# view summary statistics and view some plots
summary(dat)

ggplot(dat, aes(x=Ticket_Price)) + 
  geom_histogram(binwidth=1, fill='blue', color='white') + labs(title='Ticket Price Distribution', x='Price $')

ggplot(dat, aes(x = Movie_Genre, fill = Purchase_Again)) +
  geom_bar(position = "fill") + labs(title = "Purchase Again by Genre", x = "Movie Genre", y = "Proportion")


# =================== Preliminary Model =====================
# check with all potential predictors
glm.fit1 = glm(Purchase_Again~Ticket_Price+Movie_Genre+Bin_Seat_Type+Single_or_Group+Age_Group, 
              data=dat,
              family=binomial)
summary(glm.fit1)

# Only slight significance, let's check collinearity
vif(glm.fit1)  # looks pretty good


# =================== Stepwise Deletion =====================
# begin to narrow down variables - Ticket Price is the least significant, so remove it
glm.fit2 = glm(Purchase_Again~Movie_Genre+Bin_Seat_Type+Single_or_Group+Age_Group, 
               data=dat,
               family=binomial)
summary(glm.fit2)

# anova to compare the two models
anova(glm.fit2, glm.fit1, test='Chisq')  
# p-value is large, so models account for similar variation; AIC also lower
# proceed with smaller model

# iteratively remove the variable with the highest p-value
glm.fit3 = glm(Purchase_Again~Movie_Genre+Bin_Seat_Type+Single_or_Group, # no Age_Group
               data=dat,
               family=binomial)
summary(glm.fit3)

# remove Single_or_Group
glm.fit4 = glm(Purchase_Again~Bin_Seat_Type+Single_or_Group, # no Single_or_Group
               data=dat,
               family=binomial)
summary(glm.fit4)

# remove binary genre variable
glm.fit5 = glm(Purchase_Again~Bin_Seat_Type, 
               data=dat,
               family=binomial)
summary(glm.fit5)

# compare models each time. If p>0.05, proceed with the smaller model
anova(glm.fit5, glm.fit4, test='Chisq')

# CONCLUSIONS: No variables are significant - these variable do not adequately predict Purchase_Again
# We need to redo data collection - study different variables!
