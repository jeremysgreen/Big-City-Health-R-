
rm(list = ls())
setwd("/Users/jeremygreen/Desktop/")
bigCityHealth <- read.csv("Big_Cities_Health_Data_Inventory.csv", stringsAsFactors = TRUE)
head(bigCityHealth)
-----------------------------------------------------------------------------------------------------------------
# Summary 
  
str(bigCityHealth)

nrow(bigCityHealth)

nrow(is.na(bigCityHealth))
colSums(is.na(bigCityHealth))

bigCityHealth <- na.omit(bigCityHealth)

tapply(bigCityHealth$Value, bigCityHealth$Indicator.Category, mean)
-----------------------------------------------------------------------------------------------------------------
# Finding number of data points in each year
eleven = subset(bigCityHealth, bigCityHealth$Year == "2011")
nrow(eleven)
# 3498

twelve = subset(bigCityHealth, bigCityHealth$Year == "2012")
nrow(twelve)
# 3947

thirteen = subset(bigCityHealth, bigCityHealth$Year == "2013")
nrow(thirteen)
# 3652
-----------------------------------------------------------------------------------------------------------------
# SUBSET OF 2011, 2012, 2013
top_years = subset(bigCityHealth, bigCityHealth$Year == "2013"| bigCityHealth$Year == "2012" | bigCityHealth$Year == "2011")
-----------------------------------------------------------------------------------------------------------------
# Life Expectancy by Top Three and Bottom Three Cities
lifeExpectancy.topyears <- subset(top_years, top_years$Indicator.Catagory == "Life Expectancy and Death Rate (Overall)")

lm.LEplace.topyears <- lm(lifeExpectancy.topyears$Value ~ lifeExpectancy.topyears$Place, data = lifeExpectancy.topyears)
summary(lm.LEplace.topyears)

tapply(lifeExpectancy.topyears$Value, lifeExpectancy.topyears$Place, mean)


-----------------------------------------------------------------------------------------------------------------
# Cancer Health Indicator
Cancer <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "Cancer")

head(Cancer) 

tapply(Cancer$Value, Cancer$Place, mean)
-----------------------------------------------------------------------------------------------------------------
# Cancer by Place
  
lm.place <- lm(Cancer$Value ~ Cancer$Place, data=Cancer)
summary(lm.place)
-----------------------------------------------------------------------------------------------------------------
# Cancer by Race
lm.race <- lm(Cancer$Value ~ Cancer$Race..Ethnicity, data=Cancer)
summary(lm.race)

-----------------------------------------------------------------------------------------------------------------
# Cancer by Gender
lm.gender <- lm(Cancer$Value ~ Cancer$Gender, data=Cancer)
summary(lm.gender)

-----------------------------------------------------------------------------------------------------------------
# Life Expectancy by City
lifeExpectancy <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "Life Expectancy and Death Rate (Overall)")
head(lifeExpectancy)

lm.LEplace <- lm(lifeExpectancy$Value ~ lifeExpectancy$Place, data = lifeExpectancy)
summary(lm.LEplace)

tapply(lifeExpectancy$Value, lifeExpectancy$Place, mean)

bigCityHealth$life


meanLE = tapply(lifeExpectancy$Value, lifeExpectancy$Place, mean)

sort(meanLE, decreasing = FALSE, na.last = NA)

-----------------------------------------------------------------------------------------------------------------
# Activity Levels by City
Activity <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "Nutrition, Physical Activity, & Obesity")
head(Activity)

lm.activity <- lm(Activity$Value ~ Activity$Place, data = Activity)
summary(lm.activity)

tapply(Activity$Value, Activity$Place, mean)

-----------------------------------------------------------------------------------------------------------------
# HIV/AIDS By Place
hiv.aids <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "HIV/AIDS")

head(hiv.aids)

lm.hiv.aids <- lm(hiv.aids$Value ~ hiv.aids$Place, data = hiv.aids)
summary(lm.hiv.aids)

tapply(hiv.aids$Value, hiv.aids$Place, mean)

-----------------------------------------------------------------------------------------------------------------
# Behavioral Health/Substance Abuse By Place
  
behavioralHealth.substanceAbuse <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "Behavioral Health/Substance Abuse")

head(behavioralHealth.substanceAbuse)

lm.behavioralHealth.substanceAbuse <- lm(behavioralHealth.substanceAbuse$Value ~ behavioralHealth.substanceAbuse$Place, data = behavioralHealth.substanceAbuse)
summary(lm.behavioralHealth.substanceAbuse)

tapply(behavioralHealth.substanceAbuse$Value, behavioralHealth.substanceAbuse$Place, mean)

unique(bigCityHealth$Indicator)



obesityHS <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "Percent of High School Students Who Are Obese")

obesityoverall <- subset(bigCityHealth, bigCityHealth$Indicator.Category == "All-Cause Mortality Rate")

corr(obesityHS.obesityoverall)

cor(bigCityHealth$obesityHS, bigCityHealth$obesityoverall, method = "spearman")

test <- cor.test(bigCityHealth$obesityHS, bigCityHealth$obesityoverall)
test


round(cor(bigCityHealth),
      digits = 2 # rounded to 2 decimals
)

library(corrplot)

install.packages("corrplot")

corrplot(cor(bigCityHealth),
         method = "number",
         type = "upper" # show only upper side
)


# train the neural network
library(neuralnet)
set.seed(300)
BCH.net <- neuralnet(lifeExpectancy ~ Cancer + behavioralHealth.substanceAbuse + hiv.aids, 
                         data = bigCityHealth, hidden = 3)
plot(BCHnet)
BCH.net

# make predictions using the neural network results
pred <- predict(housing.net, boulder.clean)

unique(bigCityHealth$Year)

sum(bigCityHealth$Year == "2007-2012")
sum(bigCityHealth$Year == "2003-2013")
sum(bigCityHealth$Year == "2011-2012")
-----------------------------------------------
  # count of each year

three = subset(bigCityHealth, bigCityHealth$Year == "2003")
nrow(three)
# 0

nine = subset(bigCityHealth, bigCityHealth$Year == "2009")
nrow(nine)
# 0

ten = subset(bigCityHealth, bigCityHealth$Year == "2010")
nrow(ten)
# 1356

library(tree)

as.numeric(Activity)
state.tree = tree(Activity ~ ., data = bigCityHealth)
bigCityHealth1 <- as.data.frame(bigCityHealth)
lifeExp1 <- as.data.frame(lifeExpectancy.topyears)
install.packages("party")

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)


sample_data = sample.split(bigCityHealth, SplitRatio = 0.9)
train_data <- subset(bigCityHealth, sample_data == TRUE)
test_data <- subset(bigCityHealth, sample_data == FALSE)


# temp code
#train_data1 = train_data[!is.na(train_data$Value), ]

library(tree)
re_cols = names(test_data)[sapply(test_data[, sapply(test_data, is.factor)], nlevels) > 20]
test_data1  = test_data[, !(names(test_data) %in% re_cols)]
test_data2 = cbind(test_data[, c("Value", "Indicator")], test_data1)
test_data2$Notes = NULL
str(test_data2)


test_data3 = test_data[, c("Value", "Indicator.Category", "Year", "Gender", 
                           "Race..Ethnicity", "Place")]

model<- tree(Value ~ ., test_data3)
plot(model)
text(model)


# testing the people who are native speakers
# and those who are not
predict_model<-predict(ctree, test_data)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_data$bigCityHealth, predict_model)
m_at


state.tree
library(tree)
tree.dt <- tree(Value ~ . , data = bigCityHealth)

--------------------------------------------------------------------------------------------------------------
# residuals
  
install.packages('ggfortify')
library(ggfortify)
library(ggplot2)
m <- glm(Value ~ ., family = 'binomial', data= bigCityHealth)

autoplot(m, which = 1:6, label.size = 3)



balt = subset(bigCityHealth, bigCityHealth$Place == "Baltimore, MD")
nrow(balt)

boston = subset(bigCityHealth, bigCityHealth$Place == "Boston, MA")
nrow(boston)

table(bigCityHealth$Place)


lm.LEplace.topyears <- lm(lifeExpectancy.topyears$Value ~ lifeExpectancy.topyears$Place, data = lifeExpectancy.topyears)
summary(lm.LEplace.topyears)

