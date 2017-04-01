# Assignment two 

# 1. Download the dataset. Carefully read the data description 
# 2. Import both the training set (adult.data) and test set (adult.test) into
# R, and merge the resulting data frames. 

# Dataset Links
# http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
# http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test

# Data Description ####

#age: continuous.
#workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#fnlwgt: continuous.
#education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#education-num: continuous.
#marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, 
# Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.

#relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#sex: Female, Male.
#capital-gain: continuous.
#capital-loss: continuous.
#hours-per-week: continuous.
#native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, 
#  China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, 
#  Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.


# Load packages
library(data.table)

# Download the datasets
adult.data <- fread("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = FALSE)
adult.test <- fread("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test", 
                       skip = 1, header = FALSE)

# Checking the format we have our data in
class(adult.test) # data.frame
mode(adult.test) # list

# Delete dot in V15 column 
adult.test <- adult.test[, income := gsub("[.]", "", adult.test[[15]])]
# Delete column V15 
adult.test[, V15:=NULL]

# Preview the data ####
head(adult.data)
head(adult.test)

# Change column names 
colnames(adult.data) <- c("age","workclass","fnlwgt","education","education-num",
                          "marital-status","occupation","relationship","race",
                          "sex","capital-gain", "capital-loss", "hours-per-week", 
                          "native-country", "income")
colnames(adult.test) <- c("age","workclass","fnlwgt","education","education-num",
                          "marital-status","occupation","relationship","race",
                          "sex","capital-gain", "capital-loss", "hours-per-week", 
                          "native-country", "income")


# Combine the data frames together 
dt.adult <- rbind((adult.data), (adult.test))
head(dt.adult)
nrow(dt.adult) # 48842

# Save the cleaned data ####
save(adult.data, adult.test, dt.adult, file="adult-dataset.RData")

# Load adult-dataset
load("adult-dataset.RData")

# Q3 ####
# 3.  Carefully inspect the data. Note that there are missing values and numeric variables with
# different ranges 
# Checking if the variables have the right type
str(dt.adult)

# Check for duplicates
dupl <- duplicated(dt.adult)
unique(dupl)
table(dupl)

# Deleting the duplicate observations
dt.adult <- dt.adult[!duplicated(dt.adult),]


# See which variables have missing values 
unique(dt.adult$age)
unique(dt.adult$workclass) # ?
unique(dt.adult$fnlwgt)
unique(dt.adult$education)
unique(dt.adult$'education-num')
unique(dt.adult$'marital-status')
unique(dt.adult$occupation) # ?
unique(dt.adult$relationship)
unique(dt.adult$race)
unique(dt.adult$sex)
unique(dt.adult$'capital-gain')
unique(dt.adult$'capital-loss')
unique(dt.adult$'hours-per-week')
unique(dt.adult$'native-country') # ? 
unique(dt.adult$income) # "<=50K" ">50K" 

# Missing values in: workclass, occupation and native-country

# Checking the amount of missing values in respective variables
dt.adult$workclass[dt.adult$workclass == "?"] = NA
dt.adult$occupation[dt.adult$occupation == "?"] = NA
dt.adult$'native-country'[dt.adult$'native-country' == "?"] = NA
colSums(is.na(dt.adult))
all.na <- dt.adult[!complete.cases(dt.adult),]
nrow(all.na)
3615/48842 # 7% 
# Since missing values observations represent only 7% of our total data, are deciding to remove this observations
dt.adult <- na.omit(dt.adult)


# Q4 #### 
# 4. Prepare the data for analysis. Make a descriptive summary of the data highlighting relevant
# aspects to the task at hand

# We are removing the variable fnlwgt since the value of the salary information is already captured in the variable income 
dt.adult <- dt.adult[,  fnlwgt := NULL]
# We are removing the variable education-num because we think the variable education already contains that information
dt.adult <- dt.adult[, `education-num` := NULL]

# Variable: Age ####
summary(dt.adult$age)
# Based on the age summary we are slipting the age variable into 4 different factor options
# 0 : people of age under 30 
# 1 : people of age between 31-40 
# 2 : people of age between 41-50
# 4 : people of age over 51

# Variable: WorkClass ####
# Variable: Education ####
# We are differentiating between people that left their studies uncompleted, or they did finished a specific degree. 
table(dt.adult$education)

dt.adult$education <- gsub("^10th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^11th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^12th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^1st-4th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^5th-6th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^7th-8th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^9th","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^Assoc-acdm","Associate",dt.adult$education)
dt.adult$education <- gsub("^Assoc-voc","Associate",dt.adult$education)
dt.adult$education <- gsub("^Bachelors","Bachelor",dt.adult$education)
dt.adult$education <- gsub("^Doctorate","Doctorate",dt.adult$education)
dt.adult$education <- gsub("^HS-Grad","HS-Graduate",dt.adult$education)
dt.adult$education <- gsub("^Masters","Master",dt.adult$education)
dt.adult$education <- gsub("^Preschool","uncompleted",dt.adult$education)
dt.adult$education <- gsub("^Prof-school","Prof-School",dt.adult$education)
dt.adult$education <- gsub("^Some-college","HighSchool-Graduate",dt.adult$education)

# variable: marital-status #####
table(dt.adult$`marital-status`)
# Add column for more detailed information
dt.adult <- dt.adult[, maritalStatus2 := dt.adult$'marital-status']

# Boolean variable: either married(1) or not(0)
dt.adult$'marital-status' <- gsub("Divorced",0,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Married-AF-spouse',1,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Married-civ-spouse',1,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Married-spouse-absent',1,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Never-married',0,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Separated',0,dt.adult$'marital-status')
dt.adult$'marital-status' <- gsub('Widowed',0,dt.adult$'marital-status')

# variable: occupation #####
table(dt.adult$occupation)

# variable: relationship #####
table(dt.adult$relationship)

# variable: race #####
table(dt.adult$race)

# variable: sex #####
table(dt.adult$sex)

# variable: capital-gain #####
table(dt.adult$'capital-gain')
dt.adult <- dt.adult[, `capital-gain` := NULL]

# variable: capital-loss #####
table(dt.adult$'capital-loss')
dt.adult <- dt.adult[, `capital-loss` := NULL]

# variable: hours-per-week #####
table(dt.adult$'hours-per-week')
# Group: 
# <=30hours 
# 31 - 40 hours 
# 41 - 50 hours
# 51 - 60 hours
# 61 - 70 hours
# >= 70 hours

# variable: native-country #####
# We are grouping countries by continents
dt.adult$`native-country`[dt.adult$`native-country`=="Cambodia"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Canada"] = "North-A"    
dt.adult$`native-country`[dt.adult$`native-country`=="China"] = "Asia"       
dt.adult$`native-country`[dt.adult$`native-country`=="Columbia"] = "SA"    
dt.adult$`native-country`[dt.adult$`native-country`=="Cuba"] = "SA"        
dt.adult$`native-country`[dt.adult$`native-country`=="Dominican-Republic"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Ecuador"] = "SA"     
dt.adult$`native-country`[dt.adult$`native-country`=="El-Salvador"] = "SA" 
dt.adult$`native-country`[dt.adult$`native-country`=="England"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="France"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Germany"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Greece"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Guatemala"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Haiti"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Holand-Netherlands"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Honduras"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Hong"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Hungary"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="India"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Iran"] = "ME"
dt.adult$`native-country`[dt.adult$`native-country`=="Ireland"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Italy"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Jamaica"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Japan"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Laos"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Mexico"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Nicaragua"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Outlying-US(Guam-USVI-etc)"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Peru"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Philippines"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Poland"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Portugal"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Puerto-Rico"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="Scotland"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="South"] = "EU"
dt.adult$`native-country`[dt.adult$`native-country`=="Taiwan"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Thailand"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Trinadad&Tobago"] = "SA"
dt.adult$`native-country`[dt.adult$`native-country`=="United-States"] = "North-A"
dt.adult$`native-country`[dt.adult$`native-country`=="Vietnam"] = "Asia"
dt.adult$`native-country`[dt.adult$`native-country`=="Yugoslavia"] = "EU"

table(dt.adult$`native-country`)

# Q5 ####
# 5. Using the 10-fold cross-validation method, build three different classification models: 
# a logistic regression model, a SVM, and a neural network 

# Logistic Regression ####



# Q6 ####
# 6. Describe which method performs ebtter in your problem according to the mean percentage of 
# correctly classified instances 

# NOTES: use markdown, and use 385556FC-381588TM.pdf
