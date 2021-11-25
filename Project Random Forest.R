# Project


setwd('C:/Users/fdjsp/Desktop/DSBA_6211')

#Loaded in 2010-2020 Data
NY2010 <-read.csv('Data/NYHouse2010.csv',stringsAsFactors = F)
str(NY2010)
summary(NY2010)
head(NY2010)


#Drops unnecessary variables

NY2010 = subset(NY2010, select = -c(Reporting.Period,Home.Performance.Project.ID,
                                    Home.Performance.Site.ID,Project.City,
                                    Project.Zip,Project.Completion.Date,
                                    Type.of.Program.Financing,
                                    Volume.of.Home,Estimated.Annual.kWh.Savings,
                                    Estimated.Annual.MMBtu.Savings,
                                    New.Georeferenced.Column,Low.Rise.or.Home.Performance.Indicator))

#Renamed columns so that it is easier to reference
names(NY2010)[14] <- "Estimated.Savings"
names(NY2010)[8]<-"Amount.Financed"
names(NY2010)[9]<- "Original.Fuel.Type"
names(NY2010)[15]<- "Green.Reduced.Cost"

View(NY2010)


# creating the target variable and grouping county by region to make it easier for computation
NY2010$Target <- ifelse(10*NY2010$Estimated.Savings>NY2010$Total.Project.Cost,1,0)



#Makes all string data lowercase
NY2010$Project.County <- tolower(NY2010$Project.County)
NY2010$Project.Region<-tolower(NY2010$Project.Region)
NY2010$Gas.Utility <- tolower(NY2010$Gas.Utility)
NY2010$Electric.Utility<- tolower(NY2010$Electric.Utility)
NY2010$Original.Fuel.Type<- tolower(NY2010$Original.Fuel.Type)
NY2010$Measure.Type <- tolower(NY2010$Measure.Type)
NY2010$Customer.Type <- tolower(NY2010$Customer.Type)


#Changed the variables to be the appropriate type

NY2010$Target<- as.factor(NY2010$Target)
NY2010$Project.County <- as.factor(NY2010$Project.County)
NY2010$Gas.Utility<-as.factor(NY2010$Gas.Utility)
NY2010$Electric.Utility<-as.factor(NY2010$Electric.Utility)
NY2010$Original.Fuel.Type<-as.factor(NY2010$Original.Fuel.Type)
NY2010$Measure.Type<-as.factor(NY2010$Measure.Type)
NY2010$Number.of.Units<- as.factor(NY2010$Number.of.Units)
NY2010$Green.Reduced.Cost<-as.factor(NY2010$Green.Reduced.Cost)
NY2010$Customer.Type<-as.factor(NY2010$Customer.Type)
NY2010$Project.Region<- as.factor(NY2010$Project.Region)

summary(NY2010)

View(NY2010)
######_______________________________________________________

#Random Forest Model

library(ISLR)
library(randomForest)
library(dplyr)
library(caret)

#Remove unwanted columns
NY2010 = subset(NY2010, select = -c(Estimated.Savings, Total.Project.Cost, Project.County, Year.Home.Built, Number.of.Units))

trainIndex = createDataPartition(NY2010$Target,
                                 p = 0.7,
                                 list = FALSE,
                                 times = 1)

#Create Training and Test data
NY2010.train = NY2010[trainIndex, ]
NY2010.test = NY2010[-trainIndex,]


#Random Forest Model
summary(NY2010.train)
str(NY2010.train)

NYrf_default = train(Target ~.,
                   data = NY2010.train,
                   method = 'rf',
                   metric = "Accuracy",
                   na.action = na.roughfix,
                   ntree = 10)

print(NYrf_default)


plot(NYrf_default)

#Tune predictors
tuneGrid = expand.grid(.mtry = c(4:7))


NYrf_mtry = train(Target ~.,
                data = NY2010.train,
                method = 'rf',
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                na.action = na.roughfix,
                ntree = 10)

print(NYrf_mtry)
plot(NYrf_mtry)

varImp(NYrf_mtry)


#Prediction Accuracy and Confusion Matrix
prediction = predict(NYrf_mtry, NY2010.test, na.action=na.roughfix)
confusionMatrix(prediction, NY2010.test$Target)

#ROC Curve
library(pROC)

rf.prob = predict(NYrf_default,newdata = NY2010.test,type='prob',na.action=na.roughfix)

rf.ROC = roc(predictor=rf.prob$`1`,
               response=NY2010.test$Target,
               levels=levels(NY2010.test$Target))
plot(rf.ROC)
rf.ROC$auc

