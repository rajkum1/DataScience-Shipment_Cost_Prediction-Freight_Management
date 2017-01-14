######################### Problem Statement ####################################

#Freight management: Predicting the cost of shipment

#Quickfreight is committed being the industry leader in logistic services through
#the continous investment in technology solutions and talented individuals.
#Quickfreight is the middle man between the customer and freight services.
#They will charge from the customer and pay to the freight services and during
#the transaction they will make their profit/revenue. The challenge is, given
#a request from a customer for shipment they would like to decide the estimated
#cost for the Carriers. This information will enable them to quote the best price
#for the customer and reduce the revenue leakages and increase their bottom line.

############### Data Exploration/Observations on Attributes ####################

## Dependent/Traget Attribute

## ORDER_COST (Cost of the shipment)

## Independent/Predictor Variables

## ORDER_NBR

# This will be an unique number/ID for each new orders
# This will not have any contribution in predicting the cost. we can Drop this attribute

## EQUIPMENT_TYPE

# This is an important attribute, This will have impact on target attribute
# because based on the type e.g "R" Refrigerated , the shipment cost may change.

## CUSTOMER_MILES

# This would be treated as distance. Will have major impact on the shipment cost

## WEIGHT

# This is an important attribute, This will have impact on target attribute
# More weight, more shipment cost.

## FIRST_PICK_ZIP

# Map this to Pick up Regions

## FIRST_PICK_EARLY_APPT

# This will be used to create a new variable DELIVERY_DAY by subtracting
# (LAST_DELIVERY_EARLY_APPT - FIRST_PICK_EARLY_APPT), considering only days

## FIRST_PICK_LATE_APPT

# This variable can be dropped, as we have are considering "FIRST_PICK_EARLY_APPT" 

## LAST_DELIVERY_ZIP

# Map this to Delivery Regions

## LAST_DELIVERY_EARLY_APPT

# This will be used to create a new variable DELIVERY_DAY by subtracting
# (AST_DELIVERY_EARLY_APPT - FIRST_PICK_EARLY_APPT) and considering only days

## LAST_DELIVERY_LATE_APPT

# This variable can be dropped, as we have are considering "LAST_DELIVERY_LATE_APPT"

## IS_HAZARDOUS

# This could be an important Attribute, This will have impact on target attribte
# because based on the type e.g "Y" HAZARDOUS , the shipment cost may change.

## CREATED_DATE

# Create new Variables like "CREATED_MONTH", CREATED_DAY"

#################### Load the Libararies ##################################

# Load the required Libraries

require(dplyr)
require(lattice)
require(ggplot2)
require(h2o)
require(car)
require(MASS)

###################### Environment Variables ###############################

## Clear Environment Variables
rm(list=ls(all=TRUE))

######################### Load the CSV files ###############################

## Load the Data from csv files

# Main data file
Freight_Data = read.csv("Spot_Freight_Data.csv" ,header=T ,sep = "," ,
                        na.strings = c("",NA) )

# Region Pick look up file
Region_Pick_Lookup_Data = read.csv("Region_Lookups_Pick.csv" ,header=T ,sep = "," ,
                                   na.strings = c("",NA) )

# Region Delivery look up file
Region_Delivery_Lookup_Data = read.csv("Region_Lookups_Delivery.csv" ,header=T ,sep = "," ,
                                       na.strings = c("",NA) )

# Market Pick look up file
Market_Pick_Lookup_Data = read.csv("Market_Lookups_Pick.csv" ,header=T ,sep = "," ,
                                   na.strings = c("",NA) )
# Market Delivery look up file
Market_Delivery_Lookup_Data = read.csv("Market_Lookups_Delivery.csv" ,header=T ,sep = "," ,
                                       na.strings = c("",NA) )
# Equipment look up file
Equipment_Data = read.csv("Equipment_Codes1.csv" ,header=T ,sep = "," ,na.strings = c("",NA) )

####################### Summary ###############################################

## Summary

dim(Freight_Data)
head(Freight_Data)
str(Freight_Data)
summary(Freight_Data)


######Expand the Equipment Type ########################################

## Do the lookup using Merge function

Freight_Data <- merge(x=Freight_Data, y=Equipment_Data, by.x="EQUIPMENT_TYPE", by.y="EQUIPMENT_TYPE")
table(Freight_Data$EQUIP_TYPE)

################### Data Exploration ##################################

##Univariate Analaysis:

# Continuous Variables:
# Understand the central tendency and spread of the variable.
#   ORDER_NBR (No need so we can drop this variable)
#   CUSTOMER_MILES
#   WEIGHT
#   ORDER_COST

## 1) CUSTOMER_MILES/DISTANCE

# Summary

summary(Freight_Data$CUSTOMER_MILES)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.0   254.0   437.0   566.3   696.0  3960.0       1 

# Histogram

h_miles <- hist(Freight_Data$CUSTOMER_MILES,
                main="CUSTOMER MILES ",
                xlab="Distance in Miles",
                breaks=100,
                col="darkmagenta")


## Box plot

boxplot(Freight_Data$CUSTOMER_MILES)

# All the above shows a lot of Customer Miles are zeros (0) and distribution is right
# Skewed, so better to remove these records.

length(Freight_Data$CUSTOMER_MILES[Freight_Data$CUSTOMER_MILES == 0])

# Remove the Customer Miles having Zeros. 

Freight_Data$CUSTOMER_MILES <- ifelse(Freight_Data$CUSTOMER_MILES==0,NA,Freight_Data$CUSTOMER_MILES)
table(is.na(Freight_Data$CUSTOMER_MILES))

Freight_Data <- Freight_Data[!is.na(Freight_Data$CUSTOMER_MILES),]

## 2) WEIGHT

summary(Freight_Data$WEIGHT)

## Histogram

h_wt <- hist(Freight_Data$WEIGHT,
             main="ORDER WEIGHT",
             xlab="Weights",
             breaks=500)


## Box Plot

boxplot(Freight_Data$WEIGHT)

# All the above shows a lot of Weights are zeros (0) so better to remove
# these records. Also remove one record having weight > 200000 (Treating as outlier)

length(Freight_Data$WEIGHT[Freight_Data$WEIGHT == 0])
length(Freight_Data$WEIGHT[Freight_Data$WEIGHT > 200000])

# Remove the outliers and Zeros
Freight_Data$WEIGHT <- ifelse((Freight_Data$WEIGHT == 0) | (Freight_Data$WEIGHT > 200000),
                              NA,Freight_Data$WEIGHT)
table(is.na(Freight_Data$WEIGHT))
Freight_Data <- Freight_Data[!is.na(Freight_Data$WEIGHT),]


## 3) ORDER_COST

# Summary

summary(Freight_Data$ORDER_COST)

# Histogram

h_cost <- hist(Freight_Data$ORDER_COST,
               main="COST OF OREDRS ",
               xlab="Order Cost",
               breaks=100,
               col="Green")

# Box Plot

boxplot(Freight_Data$ORDER_COST)

# The plot is showing Right Skewed Distribution and there are a lots of zeros (0) in the cost

length(Freight_Data$ORDER_COST[Freight_Data$ORDER_COST == 0])

# There are about 6505 rows where Costs are 0.It's better to replace these
# records with NA and then remove these records.
# Also the summary shows
# Min.   :   0.0
# Max.   :6400.00
# So seems that  record having order cost = 0.01 is outlier.
# So remove this record

Freight_Data$ORDER_COST <- ifelse(Freight_Data$ORDER_COST < 1,NA, as.numeric(Freight_Data$ORDER_COST))
table(is.na(Freight_Data$ORDER_COST))
Freight_Data <- Freight_Data[!is.na(Freight_Data$ORDER_COST),]


############### Categorical Vribales ####################################

## Categorical Variables (frequency table to understand distribution of each category)

# EQUIPMENT_TYPE
# FIRST_PICK_ZIP
# LAST_DELIVERY_ZIP
# IS_HAZARDOUS
# CREATED_DATE

## 1) EQUIPMENT_TYPE

# The major category is "DRY Freight"
counts <- table(Freight_Data$EQUIP_TYPE)
barplot(counts, main="EQUIPMENT TYPES", xlab="Type of Equipments")

## 2) IS_HAZARDOUS

counts <- table(Freight_Data$IS_HAZARDOUS)
barplot(counts, main="HAZARDOUS", xlab="Is the Equipment Hazardus")

# Major category is "N" not hazardous

## Bi-variate Analysis
## Continuous & Continuous

## Now plot the ORDER_COST & CUSTOMER_MILES and check the relations
# Shows a fairly good positive correlation between Order Cost and Miles

ggplot(Freight_Data, aes(x= CUSTOMER_MILES, y=ORDER_COST)) + geom_point()

cor(Freight_Data$CUSTOMER_MILES,Freight_Data$ORDER_COST)

# 0.9031744
# good positive relationship(0.90) between two variables.

## Now plot the ORDER_COST & WEIGHT and check the relations
# Shows a fairly good positive correlation between Order Cost and Miles

ggplot(Freight_Data, aes(x=WEIGHT, y=ORDER_COST)) + geom_point()

# The relation is not very clear because weight has lots of variance

## IS_HAZARDOUS vs Order CosT

ggplot(Freight_Data, aes(x= Freight_Data$IS_HAZARDOUS, y=Freight_Data$ORDER_COST)) + geom_point()

# The plot shows order cost is more for Non Harzardous

## Equipment Type vs Order Cost

ggplot(Freight_Data, aes(x=EQUIP_TYPE, y=ORDER_COST)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Equipment Type") + 
  ylab("Order Cost") + 
  ggtitle("Equipment Type vs Order Cost")

ggplot(Freight_Data, aes(x= EQUIP_TYPE, y=ORDER_COST, fill= IS_HAZARDOUS)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Equipment Type") + 
  ylab("Oreder Cost") + 
  ggtitle("Equipment Type vs Order Cost")

# The above plot shows that the "R" refrigerated will have more shipment cost

#################### Data Cleaning ###########################################

# Remove the NA's and Spaces

sum(is.na(Freight_Data))
Freight_Data<-na.omit(Freight_Data)

################## Feature Engineering #######################################

## 1)

# Create a new variable DELIVERY_DAY (Number od days required for shipping the goods) 
# by subtracting (LAST_DELIVERY_EARLY_APPT - FIRST_PICK_EARLY_APPT), Convert both the dates into
# Julian dates and then subtract

Temp_Pick_Date <- as.Date(Freight_Data$FIRST_PICK_EARLY_APPT,format="%m-%d-%Y")
Temp_Delivery_Date <- as.Date(Freight_Data$LAST_DELIVERY_EARLY_APPT,format="%m-%d-%Y")

Freight_Data$PICK_DAY_JULIAN <- as.numeric(format(Temp_Pick_Date, "%j"))
Freight_Data$DELIVERY_DAY_JULIAN = as.numeric (format(Temp_Delivery_Date,"%j"))

Freight_Data$DELIVERY_DAY <- Freight_Data$DELIVERY_DAY_JULIAN - Freight_Data$PICK_DAY_JULIAN

# Replace the Negative and greater than 15 DELIVERY_TIME values to Zero(0)

Freight_Data$DELIVERY_DAY[Freight_Data$DELIVERY_DAY < 0] <- 0
Freight_Data$DELIVERY_DAY[Freight_Data$DELIVERY_DAY > 15] <- 0

rm(Temp_Pick_Date,Temp_Delivery_Date)

## 2)

# Create a new variable DELIVERY_TIME by subtracting
# (LAST_DELIVERY_EARLY_APPT - FIRST_PICK_EARLY_APPT), Convert both the dates and time into
# Hours and then subtract

Freight_Data$PICKUP_TIME_TEMP <- as.POSIXct(Freight_Data$FIRST_PICK_EARLY_APPT,format='%m-%d-%Y %H:%M')
Freight_Data$DELIVERY_TIME_TEMP <- as.POSIXct(Freight_Data$LAST_DELIVERY_EARLY_APPT,format='%m-%d-%Y %H:%M')

Freight_Data$DELIVERY_TIME <- ceiling((Freight_Data$DELIVERY_TIME_TEMP - Freight_Data$PICKUP_TIME_TEMP)/3600)
Freight_Data$DELIVERY_TIME <- as.numeric(Freight_Data$DELIVERY_TIME)
Freight_Data$DELIVERY_TIME[Freight_Data$DELIVERY_TIME < 0] <- 0
Freight_Data$DELIVERY_TIME[Freight_Data$DELIVERY_TIME > 360] <- 0

## 3)

# convert CREATED_DATE to DAY and MONTH to check the trends in orders.

Freight_Data$CREATED_DAY <- weekdays(as.Date(Freight_Data$CREATED_DATE,format="%m-%d-%Y"))
Freight_Data$CREATED_MONTH <- months(as.Date(Freight_Data$CREATED_DATE,format="%m-%d-%Y"))

## 4)

# Covert the FIRST_PICKUP_ZIP into Area and Market. Do the lookup based on first
# three char into region and market file/dataset.

# Subsrting the FIRST_PICK_ZIP TO FIRST 3 char to compare
# with market & region codes

Freight_Data$PICK_ZIP <- substr(Freight_Data$FIRST_PICK_ZIP,1,3)

# Do the lookup using Merge function

Freight_Data <- merge(x=Freight_Data, y=Region_Pick_Lookup_Data, by.x="PICK_ZIP", by.y="ZIPCODE")
Freight_Data <- merge(x=Freight_Data, y=Market_Pick_Lookup_Data, by.x="PICK_ZIP", by.y="ZIPS")

## 5)

# Covert the LAST_DELIVERY_ZIP into Area and Market. Do the lookup based on first
# 3 char into region and market file/dataset.

# Subsrting the LAST_DELIVERY_ZIP TO FIRST 3 char to compare
# with market & region codes

Freight_Data$DELIVERY_ZIP <- substr(Freight_Data$LAST_DELIVERY_ZIP,1,3)

# Do the lookup using Merge function

Freight_Data <- merge(x=Freight_Data, y=Region_Delivery_Lookup_Data, by.x="DELIVERY_ZIP", by.y="ZIPCODE")
Freight_Data <- merge(x=Freight_Data, y=Market_Delivery_Lookup_Data, by.x="DELIVERY_ZIP", by.y="ZIPS")


## 6)

# Generate the new column from PICKUP_MARKET and DELIVERY_MARKET

Freight_Data$PICK_DEL_MARKET <- paste(Freight_Data$PICKUP_MARKET,'->',Freight_Data$DELIVERY_MARKET)
Freight_Data <- merge(Freight_Data, data.frame(table(PICK_DEL_MARKET = Freight_Data$PICK_DEL_MARKET)), by = c("PICK_DEL_MARKET"))

# Reduce the Levels

Freight_Data$PICK_DEL_MARKET <- ifelse(Freight_Data$Freq < 100,"OTHER -> OTHER",Freight_Data$PICK_DEL_MARKET)
Freight_Data$PICK_DEL_MARKET <- as.factor(Freight_Data$PICK_DEL_MARKET)
Freight_Data$Freq <- NULL

## 7)

# Generate the new column from PICKUP_REGION and DELIVERY_REGION

Freight_Data$PICK_DEL_REGION <- paste(Freight_Data$PICKUP_REGION,'->',Freight_Data$DELIVERY_REGION)

Freight_Data <- merge(Freight_Data, data.frame(table(PICK_DEL_REGION = Freight_Data$PICK_DEL_REGION)), by = c("PICK_DEL_REGION"))
# Reduce the Levels

Freight_Data$PICK_DEL_REGION <- ifelse(Freight_Data$Freq < 200,"REGOTHER - REGOTHER",Freight_Data$PICK_DEL_REGION)
Freight_Data$PICK_DEL_REGION <- as.factor(Freight_Data$PICK_DEL_REGION)


###################### Remove the unwanted Varibales ##############################

## Remove the attributes from dataset which will not have any contributions

Freight_Data$FIRST_PICK_ZIP <- NULL
Freight_Data$FIRST_PICK_LATE_APPT <- NULL
Freight_Data$LAST_DELIVERY_ZIP <- NULL
Freight_Data$LAST_DELIVERY_LATE_APPT <- NULL
Freight_Data$FIRST_PICK_EARLY_APPT <- NULL
Freight_Data$LAST_DELIVERY_EARLY_APPT <- NULL
Freight_Data$PICK_DAY_JULIAN <- NULL
Freight_Data$DELIVERY_DAY_JULIAN <- NULL
Freight_Data$CREATED_DATE <- NULL
Freight_Data$DELIVERY_ZIP <- NULL
Freight_Data$PICK_ZIP <- NULL
Freight_Data$FIRST_PICK_ZIP <- NULL
Freight_Data$LAST_DELIVERY_ZIP <- NULL
Freight_Data$PICKUP_TIME_TEMP <- NULL
Freight_Data$DELIVERY_TIME_TEMP <- NULL
Freight_Data$NEW_ABBR <- NULL
Freight_Data$EQUIPMENT_TYPE <- NULL
Freight_Data$Freq <- NULL
Freight_Data$PICKUP_REGION <- NULL
Freight_Data$DELIVERY_REGION <- NULL
Freight_Data$PICKUP_MARKET <- NULL
Freight_Data$DELIVERY_MARKET <- NULL
Freight_Data$DELIVERY_TIME <- NULL

# Convert to proper format (Numeric/Factor)

Freight_Data$CREATED_DAY <- as.factor(Freight_Data$CREATED_DAY)
Freight_Data$CREATED_MONTH <- as.factor(Freight_Data$CREATED_MONTH)
Freight_Data$CUSTOMER_MILES <- as.numeric(Freight_Data$CUSTOMER_MILES)
Freight_Data$WEIGHT <- as.numeric(Freight_Data$WEIGHT)

# Summary

str(Freight_Data)
names(Freight_Data)


##### Remove the duplicates based on order Number(ORDER_NBR)##############

table(duplicated(Freight_Data$ORDER_NBR))
Freight_Data_No_Dup  <- subset(Freight_Data, !duplicated(Freight_Data[,3]))

################ Final datsets with all pre- proceesing ##################

# Final dataset (Freight_Data_No_Dup)

Freight_Data_No_Dup$ORDER_NBR <- NULL

################### Univariate Analyis for new Variables####################

## For CREATED_DAY

counts <- table(Freight_Data_No_Dup$CREATED_DAY)
barplot(counts, main="Order Creation Day", xlab="Creation Day")

# This shows more business on weekdays (Mon, Tue, Wed, Thur, Friday). 
# Thursday is the Highest
# Least business on weekend.

## For CREATED_MONTH

counts <- table(Freight_Data_No_Dup$CREATED_MONTH)
barplot(counts, main="Order Creation Month", xlab="Creation Month")

# This shows the APR and MAy month has more business. 

## For PICKUP_REGION

counts <- table(Freight_Data_No_Dup$PICK_DEL_REGION)
ggplot(Freight_Data_No_Dup, aes(x=PICK_DEL_REGION)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Pickup Delivery Regions") + 
  ylab("Total Orders") + 
  ggtitle("Pickup Delivery Region vs Total Orders")

# For Market

counts <- table(Freight_Data_No_Dup$PICK_DEL_MARKET)

ggplot(Freight_Data_No_Dup, aes(x=PICK_DEL_MARKET)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Pickup Delivery Markets") + 
  ylab("Total Orders") + 
  ggtitle("Pickup Delivery Markets vs Total Orders")

## For DELIVERY_DAY

counts <- table(Freight_Data_No_Dup$DELIVERY_DAY)
barplot(counts, main="Delivery Days", xlab="Delivery Days")

# One day(1) deliveries is the heighst, then comes the same day(0) deliveries

################## Bi Varaite Analysis #####################

## Analysis for DELIVERY_DAY & ORDER_COST

ggplot(Freight_Data_No_Dup, aes(x= DELIVERY_DAY, y=ORDER_COST)) + geom_point()
cor(Freight_Data$DELIVERY_DAY,Freight_Data$ORDER_COST)

## Plot for PICK_DEL_REGION & ORDER_COST

ggplot(Freight_Data_No_Dup, aes(x= PICK_DEL_REGION, y=ORDER_COST)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Pickup Delivery Region") + 
  ylab("Order Cost") + 
  ggtitle("Pickup Delivery Region vs Order Cost")

## Plot for PICK_DEL_MARKET & ORDER_COST

ggplot(Freight_Data_No_Dup, aes(x= PICK_DEL_MARKET, y=ORDER_COST)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))+
  xlab("Pickup Delivery Market") + 
  ylab("Order Cost") + 
  ggtitle("Pickup Delivery Market vs Order Cost")


################### Re-Arrange the variables #####################################

## Re arrange the Numeric and categorical attributes

# Categorical Variables

Fac_Col_Names = c("EQUIP_TYPE", "IS_HAZARDOUS", "CREATED_DAY", "CREATED_MONTH",
                  "PICK_DEL_REGION", "PICK_DEL_MARKET")
# Numeric Vraibles

Num_Col_Names = c("WEIGHT","CUSTOMER_MILES","DELIVERY_DAY")

## Prepare for Clustering

Freight_Data_No_Dup_Clust <- Freight_Data_No_Dup

# Keeping only "WEIGHT","CUSTOMER_MILES","DELIVERY_DAY" varibales in dataset

Freight_Data_Clust <- Freight_Data_No_Dup[,Num_Col_Names] 

# Categorizing cat, num and taget varibales

Freight_Data_F = Freight_Data_No_Dup[,Fac_Col_Names]
Freight_Data_N = Freight_Data_No_Dup[,Num_Col_Names]
Freight_Data_T = subset(Freight_Data_No_Dup,select="ORDER_COST")

## Converting categorical attributes into DUMMY variables

Freight_Data_Dum_F <- model.matrix(Freight_Data_No_Dup$ORDER_COST ~ Freight_Data_No_Dup$EQUIP_TYPE + Freight_Data_No_Dup$IS_HAZARDOUS +
                                     Freight_Data_No_Dup$CREATED_DAY + Freight_Data_No_Dup$CREATED_MONTH + Freight_Data_No_Dup$PICK_DEL_REGION + Freight_Data_No_Dup$PICK_DEL_MARKET)[,-1]

## Final dataset with dummy

Freight_Data_No_Dup <- cbind(Freight_Data_N,Freight_Data_Dum_F,Freight_Data_T)

## Split into Train and Test (70:30)

rows<-seq(1,nrow(Freight_Data_No_Dup),1)
set.seed(1234)
trainrows<-sample(rows,0.7*nrow(Freight_Data_No_Dup))

Train <- data.frame(Freight_Data_N,Freight_Data_Dum_F,ORDER_COST=Freight_Data_No_Dup$ORDER_COST)[trainrows,]
Test <- data.frame(Freight_Data_N,Freight_Data_Dum_F,ORDER_COST=Freight_Data_No_Dup$ORDER_COST)[-trainrows,]


##################### Model Building #########################################
## First Apply the Random Forest to know the important Variables

## Random Forest using h2O

library(h2o)

## h2o.init functions connect to H2O
## Starts H2O using localhost IP, port 54321, all CPUs, and 4g of memory
## h2o.init(ip = "localhost", port = 54321, nthreads= -1, max_mem_size = "4g")

h2o.init(ip='localhost', port = 54321, max_mem_size = '4g')

## Converts R object into H2O object "Train.hex"

Train.hex <- as.h2o(x = Train, destination_frame = "Train.hex")

y = "ORDER_COST"
x = setdiff(colnames(Train.hex), y)   

## Build the Random Forest with 500 Trees

rf_mod <- h2o.randomForest(x, y, Train.hex, ntrees = 500)

## Summary

summary(rf_mod)

h2o.removeAll()
h2o.shutdown(prompt = F)

## Results

# MSE:  27619.87
# RMSE:  166.1923
# MAE:  82.61399
# RMSLE:  0.1406605
# Mean Residual Deviance :  27619.87

#Variable Importances (With Dummyfying):

#  variable	                                              relative_importance 	   scaled_importance 	percentage
#1                                         CUSTOMER_MILES	1157324210176.000000     1.000000   		    0.595263
#2                                           DELIVERY_DAY	327230652416.000000      0.282748   		    0.168309
#3 Freight_Data_No_Dup.PICK_DEL_REGIONREGOTHER...REGOTHER	159471566848.000000      0.137793   		    0.082023
#4             Freight_Data_No_Dup.EQUIP_TYPEREFRIGERATED	57268416512.000000       0.049483   		    0.029456
#5                                                 WEIGHT	54027456512.000000       0.046683   		    0.027789

##Variable Importances(without Dummyfying): 

#  variable  relative_importance            scaled_importance	percentage
#1  CUSTOMER_MILES 1105231216640.000000     1.000000	        0.537506
#2 PICK_DEL_REGION  409482985472.000000     0.370495	        0.199143
#3    DELIVERY_DAY  265031450624.000000     0.239797	        0.128892
#4 PICK_DEL_MARKET  107617697792.000000     0.097371	        0.052338
#5          WEIGHT   59044601856.000000     0.053423	        0.028715
#6      EQUIP_TYPE   58947239936.000000     0.053335	        0.028668
#7     CREATED_DAY   24107673600.000000     0.021812	        0.011724
#8   CREATED_MONTH   24059353088.000000     0.021769	        0.011701
#9    IS_HAZARDOUS    2698920960.000000     0.002442	        0.001313

######################## Model (1) #############################

## Baseline model

# Baseline model is the one which requires no predictive model and its like an informed guess.
# For instance, in this case lets predict the shipment cost as overall average of ORDER_COST.

summary(Freight_Data$ORDER_COST)

## Results

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#78     550     750    1019    1250    6400 

# Mean = 1019


######################## Model (2) #############################

## Linear Regression

Linreg <- lm(ORDER_COST ~ .,data=Train)
summary(Linreg)

# Check for Multicolinarity, All are less than 10
vif(Linreg)

# Plot

par(mfrow=c(2,2))
plot(Linreg)

#Coefficients:
#Estimate    Std. Error  t value   Pr(>|t|)
#  (Intercept)     		2.138e+02  5.841e+00  36.605  < 2e-16 ***
#  CUSTOMER_MILES  	1.417e+00  7.777e-03 182.168  < 2e-16 ***
#  WEIGHT          		1.767e-03  1.659e-04  10.647  < 2e-16 ***
#  IS_HAZARDOUSY   	1.461e+02  2.435e+01   6.001 2.02e-09 ***
#  DELIVERY_DAY   	-3.455e+01  3.209e+00 -10.765  < 2e-16 ***


#Multiple R-squared:  0.8896,	Adjusted R-squared:  0.8888 
#F-statistic:  1051 on 89 and 11607 DF,  p-value: < 2.2e-16

## Error metrics evaluation on train data

library(DMwR)

train_Lm <- regr.eval(Train$ORDER_COST, predict(Linreg,Train))
train_Lm

## Results

#mae          mse         rmse         mape 
#1.316923e+02 5.020059e+04 2.240549e+02 1.471939e-01

## Error verification on test data

test_Lm <- regr.eval(Test$ORDER_COST, predict(Linreg,Test))
test_Lm

## Results

#mae          mse         rmse         mape 
#1.370007e+02 5.553791e+04 2.356648e+02 1.491166e-01

# The difference between the mean absolute percentage error in train and test
#is not much. So the model seems to be good.


########### Prepare the data for GLM models #######################

## Converted the data into matrix form for input

Freight_Data_glm <- as.matrix(data.frame(Freight_Data_N,Freight_Data_Dum_F))

## Split the data into Train and Test for GLM

Train_glm = Freight_Data_glm[trainrows,]
Test_glm = Freight_Data_glm[-trainrows,]

## Take out the Target Varaible

y_train = Freight_Data_No_Dup$ORDER_COST[trainrows]
y_test = Freight_Data_No_Dup$ORDER_COST[-trainrows]

#################### Model (3) ##############################

## Lasso Regression using glmnet - L1 norm (Least Absolute Shrinkage and Selection Operator)

# LASSO creates a regression model that is penalized with the L1-norm
# which is the sum of the absolute coefficients. This has the effect of
# shrinking coefficient values (and the complexity of the model),allowing some
# with a minor effect to the response to become zero.

library(glmnet)
library(DMwR)

## Fit Model

fit1 <- glmnet(Train_glm, y_train, alpha=1)

## Plots

plot(fit1,xvar="lambda", label=TRUE)
plot(fit1,xvar="dev", label=TRUE)
plot(fit1,xvar="norm", label=TRUE)

## Model Selection

cv.lasso=cv.glmnet(Train_glm, y_train)

## Plot

plot(cv.lasso)
#coef(cv.lasso)
#cv.lasso$cvm

## lambda.min - value of lambda that gives minimum cvm - mean cross-validated error
cv.lasso$lambda.min

## Predict

fit_Lasso = glmnet(Train_glm,y_train,lambda=cv.lasso$lambda.min,alpha=1)
#predict(fit_Lasso,Train_glm)

## Error Verification on Train

train_Lasso = regr.eval(y_train, predict(fit_Lasso,Train_glm))
train_Lasso

## Results
#mae          mse         rmse         mape 
#1.318873e+02 5.023455e+04 2.241307e+02 1.474764e-01 

## Error Verification on Test
test_Lasso = regr.eval(y_test, predict(fit_Lasso,Test_glm))
test_Lasso

## Results:
#mae         mse        rmse        mape 
#137.11576 55534.84935   235.65833     0.14928 

#################### Model (4) ##################################

## Ridge Regression  using glmnet  - L2 norm

# Ridge Regression creates a linear regression model that is penalized with
# the L2-norm which is the sum of the squared coefficients. This has the effect
# of shrinking the coefficient values (and the complexity of the model) allowing
# some coefficients with minor contribution to the response to get close to zero.

library(glmnet)
library(DMwR)

## fit model

fit2 <- glmnet(Train_glm,y_train,alpha=0)

## Plots

plot(fit2,xvar="lambda",label=TRUE)
plot(fit2,xvar="dev", label=TRUE)
plot(fit2,xvar="norm", label=TRUE)

## Model Selection

cv.ridge=cv.glmnet(Train_glm,y_train,alpha=0)
plot(cv.ridge)
#coef(cv.ridge)
#cv.ridge$cvm

## lambda.min - value of lambda that gives minimum cvm - mean cross-validated error

cv.ridge$lambda.min

## Results
# 67.35013

## predict

fit_Ridge = glmnet(Train_glm,y_train,lambda=cv.ridge$lambda.min,alpha=0)

#predict(fit_Ridge,Train_glm)

## Error Verification on Train

train_Ridge = regr.eval(y_train, predict(fit_Ridge,Train_glm))
train_Ridge

## Results
#mae          mse         rmse         mape 
#1.418346e+02 5.653708e+04 2.377753e+02 1.616798e-01

## Error Verification on Test

test_Ridge = regr.eval(y_test, predict(fit_Ridge,Test_glm))
test_Ridge

## Results
#mae          mse         rmse         mape 
#1.472684e+02 6.219973e+04 2.493987e+02 1.640374e-01


################### Model (5) #####################################

## Elastic regression
# Elastic Net creates a regression model that is penalized with both the L1-norm
# and L2-norm. This has the effect of effectively shrinking coefficients (
# as in ridge regression) and setting some coefficients to zero (as in LASSO).


library(glmnet)
library(DMwR)

## Fit Model

cv.Elastic=cv.glmnet(Train_glm,y_train,alpha=0.5)

## Plots

plot(cv.Elastic)
#coef(cv.Elastic)
#cv.Elastic$cvm

## lambda.min - value of lambda that gives minimum cvm - mean cross-validated error

cv.Elastic$lambda.min

## Results
# 0.1478334

## Predict

fit_Elastic = glmnet(Train_glm,y_train,lambda=cv.Elastic$lambda.min,alpha=0.5)

## Error Verification on Train

train_Elastic = regr.eval(y_train, predict(fit_Elastic,Train_glm))
train_Elastic

## Results
#mae          mse         rmse         mape 
#1.318814e+02 5.023289e+04 2.241270e+02 1.474699e-01

## Error Verification on Test

test_Elastic = regr.eval(y_test, predict(fit_Elastic,Test_glm))
test_Elastic

## Results
#mae          mse         rmse         mape 
#1.371099e+02 5.553499e+04 2.356586e+02 1.492724e-01 

####### Error tables for Linear, Ridge, Lasso and Elastic Regression #########


finalerros <- data.frame(rbind(train_Lm,test_Lm,
                               train_Lasso,test_Lasso,
                               train_Ridge,test_Ridge,
                               train_Elastic,test_Elastic))
finalerros

## Results
#               mae      mse     rmse      mape
#train_Lm      131.6923 50200.59 224.0549 0.1471939
#test_Lm       137.0007 55537.91 235.6648 0.1491166
#train_Lasso   131.8873 50234.55 224.1307 0.1474764
#test_Lasso    137.1158 55534.85 235.6583 0.1492800
#train_Ridge   141.8346 56537.08 237.7753 0.1616798
#test_Ridge    147.2684 62199.73 249.3987 0.1640374
#train_Elastic 131.8814 50232.89 224.1270 0.1474699
#test_Elastic  137.1099 55534.99 235.6586 0.1492724

#######################################################################

############### Clustering Approach ###################################                
############### Start Of CLUSTERING ###################################

## With variable "WEIGHT","CUSTOMER_MILES","DELIVERY_DAY" 
## with 4 Clusters as suggested by Elbow graph

## Normailze/Standardize all the variables
library(vegan)
temp_df1 <- decostand(Freight_Data_Clust,"range")
#View(temp_df1)

## Finding the right value of "K"

par(mfrow=c(1,1))
wss <- 0
for (i in 1:15) {
  set.seed(12345)
  wss[i] <- sum(kmeans(temp_df1,centers=i)$withinss)
}

## Ploting Elbow Graph

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster using 'k-means' algorithm (k=4)

set.seed(12345)
clus<-kmeans(temp_df1,4, iter.max = 100)
table(clus$cluster)

#1    2    3    4 
#2233 6204 7456  818 

clus$centers
#   WEIGHT       CUSTOMER_MILES DELIVERY_DAY
#1 0.3535582     0.22127194   0.19618313
#2 0.3616646     0.09114683   0.06608640
#3 0.1013058     0.09375954   0.07850157
#4 0.2270982     0.49655195   0.29462103

#clus
#Within cluster sum of squares by cluster:
#  [1] 32.43171 41.52532 72.68887 33.58397
#(between_SS / total_SS =  72.5 %)

# Plot
par(mfrow=c(1,1))

plot(temp_df1,col=clus$cluster)

## Create a new dataset, adding the cluster numbers

temp_out1 <- cbind(Freight_Data_No_Dup_Clust, CLUSTER_NUM = clus$cluster)

## Cluster (1)

Freight_Data_Clust1 <- temp_out1[temp_out1$CLUSTER_NUM == 1, ]

## Plots

ggplot(Freight_Data_Clust1, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()
ggplot(Freight_Data_Clust1, aes(x= jitter(DELIVERY_DAY), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 200 - 1500
#hist(Freight_Data_Clust1$CUSTOMER_MILES)
plot(density(Freight_Data_Clust1$CUSTOMER_MILES))

## This cluster has the data of weights in between 20000 - 45000
#hist(Freight_Data_Clust1$WEIGHT)
plot(density(Freight_Data_Clust1$WEIGHT))

## order cost in between 500 - 3000
#hist(Freight_Data_Clust1$ORDER_COST)
plot(density(Freight_Data_Clust1$ORDER_COST))

## Cluster (2)

Freight_Data_Clust2 <- temp_out1[temp_out1$CLUSTER_NUM == 2, ]

## Plots

ggplot(Freight_Data_Clust2, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust2, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

# This cluster has the data of customer miles in between 70 - 800
#hist(Freight_Data_Clust2$CUSTOMER_MILES)
plot(density(Freight_Data_Clust2$CUSTOMER_MILES))

# This cluster has the data of weights in between 40000 - 50000
#hist(Freight_Data_Clust2$WEIGHT)
plot(density(Freight_Data_Clust2$WEIGHT))

# order cost in between 250 - 1500
#hist(Freight_Data_Clust2$ORDER_COST)
plot(density(Freight_Data_Clust2$ORDER_COST))


## Cluster (3)

Freight_Data_Clust3 <- temp_out1[temp_out1$CLUSTER_NUM == 3, ]

## Plots

ggplot(Freight_Data_Clust3, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust3, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 100 - 1000
#hist(Freight_Data_Clust3$CUSTOMER_MILES)
plot(density(Freight_Data_Clust3$CUSTOMER_MILES))

## This cluster has the data of weights in between 5000 - 25000
#hist(Freight_Data_Clust3$WEIGHT)
plot(density(Freight_Data_Clust3$WEIGHT))

## order cost  100 - 2000
#hist(Freight_Data_Clust3$ORDER_COST)
plot(density(Freight_Data_Clust3$ORDER_COST))

##cluster (4)

Freight_Data_Clust4 <- temp_out1[temp_out1$CLUSTER_NUM== 4, ]

## Plots
ggplot(Freight_Data_Clust4, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust4, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 1000 - 3000
#hist(Freight_Data_Clust4$CUSTOMER_MILES)
plot(density(Freight_Data_Clust4$CUSTOMER_MILES))

## This cluster has the data of weights in between 2500 - 45000
#hist(Freight_Data_Clust4$WEIGHT)
plot(density(Freight_Data_Clust4$WEIGHT))

## order cost  100 - 5000
#hist(Freight_Data_Clust4$ORDER_COST)
plot(density(Freight_Data_Clust4$ORDER_COST))

##Linear Regression

## Linear Reg on 1st cluster

Freight_Data_Clust1$CLUSTER_NUM <- NULL

## Split the cluster (1) into test & Train (70:30)

rows1<-seq(1,nrow(Freight_Data_Clust1),1)
set.seed(1234)
trainrows_clust1<-sample(rows1,0.7*nrow(Freight_Data_Clust1))
Train_clust1<-Freight_Data_Clust1[trainrows_clust1,]
Test_clust1<- Freight_Data_Clust1[-trainrows_clust1,]

## Linear Reg Model

Linreg_clust1 <- lm(ORDER_COST ~ .,data=Train_clust1)
#summary(Linreg_clust1)

## Results
#Multiple R-squared:  0.758,	Adjusted R-squared:  0.7481 
#F-statistic: 77.05 on 61 and 1501 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust1)

## Evaluation on Train

library(DMwR)
train_lm_clust1 <- regr.eval(Train_clust1$ORDER_COST, predict(Linreg_clust1,Train_clust1))
train_lm_clust1

## Results
#mae          mse         rmse         mape 
#2.163920e+02 8.831492e+04 2.971783e+02 1.558501e-01 

## Evaluation on Train

remove = c("Ohio River -> Ohio River")
remove1 = c("NY_ROC -> OH_COL")
remove2 = c("Sunday")

Test_clust1 <- Test_clust1[!Test_clust1$PICK_DEL_REGION %in% remove,]
Test_clust1 <- Test_clust1[!Test_clust1$PICK_DEL_MARKET %in% remove1,]
Test_clust1 <- Test_clust1[!Test_clust1$CREATED_DAY %in% remove2,]

test_lm_clust1 <- regr.eval(Test_clust1$ORDER_COST, predict(Linreg_clust1,Test_clust1))
test_lm_clust1

## Results
#mae          mse         rmse         mape 
#2.211876e+02 8.876561e+04 2.979356e+02 1.507899e-01

## Linear Reg on 2nd cluster

Freight_Data_Clust2$CLUSTER_NUM <- NULL

## Split the cluster (2) into test & Train (70:30)

rows2<-seq(1,nrow(Freight_Data_Clust2),1)
set.seed(1234)
trainrows_clust2<-sample(rows2,0.7*nrow(Freight_Data_Clust2))
Train_clust2<-Freight_Data_Clust2[trainrows_clust2,]
Test_clust2<- Freight_Data_Clust2[-trainrows_clust2,]

## Linear Reg Model

Linreg_clust2 <- lm(ORDER_COST ~ .,data=Train_clust2)
#summary(Linreg_clust2)

## Results
#Multiple R-squared:  0.8047,	Adjusted R-squared:  0.8014 
#F-statistic:   241 on 73 and 4268 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust2)

## Evaluation on Train
library(DMwR)
train_lm_clust2 <- regr.eval(Train_clust2$ORDER_COST, predict(Linreg_clust2,Train_clust2))
train_lm_clust2

## Results
#mae          mse         rmse         mape 
#9.445837e+01 2.054189e+04 1.433244e+02 1.352266e-01

## Evaluation on Test

remove1 = c("VA_ROA -> MD_BAL")
Test_clust2 <- Test_clust2[!Test_clust2$PICK_DEL_MARKET %in% remove1,]

test_lm_clust2 <- regr.eval(Test_clust2$ORDER_COST, predict(Linreg_clust2,Test_clust2))
test_lm_clust2

## Results
#mae          mse         rmse         mape 
#1.040364e+02 2.643169e+04 1.625782e+02 1.448428e-01

## Linear Reg on 3rd cluster

Freight_Data_Clust3$CLUSTER_NUM <- NULL

## Split the cluster (3) into test & Train (70:30)

rows3<-seq(1,nrow(Freight_Data_Clust3),1)
set.seed(1234)
trainrows_clust3<-sample(rows3,0.7*nrow(Freight_Data_Clust3))
Train_clust3<-Freight_Data_Clust3[trainrows_clust3,]
Test_clust3<- Freight_Data_Clust3[-trainrows_clust3,]

## Linear Reg Model

Linreg_clust3 <- lm(ORDER_COST ~ .,data=Train_clust3)
#summary(Linreg_clust3)

## Results
#Multiple R-squared:  0.8351,	Adjusted R-squared:  0.8324 
#F-statistic: 305.9 on 85 and 5133 DF,  p-value: < 2.2e-16

## Plot
par(mfrow=c(2,2))
plot(Linreg_clust3)

## Evaluation on Train

library(DMwR)
train_lm_clust3 <- regr.eval(Train_clust3$ORDER_COST, predict(Linreg_clust3,Train_clust3))
train_lm_clust3

## Results
#mae         mse        rmse        mape 
#89.35460 23647.85934   153.77860     0.12209 

## Evaluation on Test

remove1 = c("IN_IND -> PA_ALL")
Test_clust3 <- Test_clust3[!Test_clust3$PICK_DEL_MARKET %in% remove1,]

test_lm_clust3 <- regr.eval(Test_clust3$ORDER_COST, predict(Linreg_clust3,Test_clust3))
test_lm_clust3

## Results
#mae          mse         rmse         mape 
#8.866386e+01 2.038513e+04 1.427765e+02 1.231248e-01


## Linear Reg on 4th cluster

Freight_Data_Clust4$CLUSTER_NUM <- NULL
Freight_Data_Clust4$PICK_DEL_MARKET <- NULL

## Split the cluster (4) into test & Train (70:30)

rows4<-seq(1,nrow(Freight_Data_Clust4),1)
set.seed(1234)
trainrows_clust4<-sample(rows4,0.7*nrow(Freight_Data_Clust4))
Train_clust4<-Freight_Data_Clust4[trainrows_clust4,]
Test_clust4<- Freight_Data_Clust4[-trainrows_clust4,]

## Linear Reg Model

Linreg_clust4 <- lm(ORDER_COST ~ .,data=Train_clust4)
#summary(Linreg_clust4)

## Results
#Multiple R-squared:  0.6465,	Adjusted R-squared:  0.6324 
#F-statistic: 45.64 on 22 and 549 DF,  p-value: < 2.2e-16

## Plots

par(mfrow=c(2,2))
plot(Linreg_clust4)

## Evaluation on Train

train_lm_clust4 <- regr.eval(Train_clust4$ORDER_COST, predict(Linreg_clust4,Train_clust4))
train_lm_clust4

## Results
#mae          mse         rmse         mape 
#4.014556e+02 3.100047e+05 5.567807e+02 1.818439e-01

## Evaluation on Test

remove = c("South Central -> South Central", "Southeast -> Great Lakes", "Upper Atlantic -> Ohio River")
Test_clust4 <- Test_clust4[!Test_clust4$PICK_DEL_REGION %in% remove,]

test_lm_clust4 <- regr.eval(Test_clust4$ORDER_COST, predict(Linreg_clust4,Test_clust4))
test_lm_clust4

## Results
#mae          mse         rmse         mape 
#4.011419e+02 2.879123e+05 5.365746e+02 1.864107e-01 

### Final Errors
finalerros <- data.frame(rbind(train_lm_clust1,test_lm_clust1,
                               train_lm_clust2,test_lm_clust2,
                               train_lm_clust3,test_lm_clust3,
                               train_lm_clust4,test_lm_clust4))
finalerros

## Results

#                 mae       mse       rmse      mape
#train_lm_clust1 216.39198  88314.92 297.1783 0.1558501
#test_lm_clust1  221.18765  88765.61 297.9356 0.1507899
#train_lm_clust2  94.45837  20541.89 143.3244 0.1352266
#test_lm_clust2  104.03637  26431.69 162.5782 0.1448428
#train_lm_clust3  89.35460  23647.86 153.7786 0.1220900
#test_lm_clust3   88.66386  20385.13 142.7765 0.1231248
#train_lm_clust4 401.45561 310004.74 556.7807 0.1818439
#test_lm_clust4  401.14189 287912.33 536.5746 0.1864107

################# End Of Clustering ###################################

########## Remove not required Variables ###############################
rm(temp_df1,clus)
rm(remove,remove1,remove2)
rm(Freight_Data_Clust1,Freight_Data_Clust2,Freight_Data_Clust3,Freight_Data_Clust4)
rm(rows1,rows2,rows3,rows4)
rm(trainrows_clust1,trainrows_clust2,trainrows_clust3,trainrows_clust4)
rm(Train_clust1,Train_clust2,Train_clust3,Train_clust4)
rm(Test_clust1,Test_clust2,Test_clust3,Test_clust4)
rm(Linreg_clust1,Linreg_clust2,Linreg_clust3,Linreg_clust4)
rm(train_lm_clust1,train_lm_clust2,train_lm_clust3,train_lm_clust4)
rm(test_lm_clust1,test_lm_clust2,test_lm_clust3,test_lm_clust4)

##################################################################

############### Start Of Clustering ##############################

## With only two variables "CUSTOMER_MILES","DELIVERY_DAY" 
## with 4 Clusters as suggested by Elbow graph

## Normailze/Standardize all the variables
library(vegan)
temp_df1 <- decostand(Freight_Data_Clust[,-1],"range")
#View(temp_df1)

## Finding the right value of "K"
wss <- 0
for (i in 1:15) {
  set.seed(12345)
  wss[i] <- sum(kmeans(temp_df1,centers=i)$withinss)
}

## Ploting Elbow Graph
par(mfrow=c(1,1))

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster using 'k-means' algorithm (k=4)

set.seed(12345)
clus<-kmeans(temp_df1, 4 , iter.max = 100)
table(clus$cluster)

#1    2    3    4 
#6079 6364 3261 1007 

clus$centers
#    CUSTOMER_MILES DELIVERY_DAY
#1     0.12449158   0.07907424
#2     0.05015892   0.04267998
#3     0.18651505   0.19689571
#4     0.47726371   0.27805362

#clus
#Within cluster sum of squares by cluster:
#  [1]  9.430213 12.682454 29.628040 22.980760
#(between_SS / total_SS =  77.6 %)

# Plot
par(mfrow=c(1,1))
plot(temp_df1,col=clus$cluster)

## Create an new dataset adding the cluster numbers

temp_out1 <- cbind(Freight_Data_No_Dup_Clust, CLUSTER_NUM = clus$cluster)

## Cluster (1)

Freight_Data_Clust1 <- temp_out1[temp_out1$CLUSTER_NUM == 1, ]

## Plots

ggplot(Freight_Data_Clust1, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()
ggplot(Freight_Data_Clust1, aes(x= jitter(DELIVERY_DAY), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 200 - 1700
#hist(Freight_Data_Clust1$CUSTOMER_MILES)
plot(density(Freight_Data_Clust1$CUSTOMER_MILES))

## This cluster has the data of weight in between 20000 - 45000
#hist(Freight_Data_Clust1$WEIGHT)
plot(density(Freight_Data_Clust1$WEIGHT))

## order cost in between 500 - 3000
#hist(Freight_Data_Clust1$ORDER_COST)
plot(density(Freight_Data_Clust1$ORDER_COST))

## Cluster (2)

Freight_Data_Clust2 <- temp_out1[temp_out1$CLUSTER_NUM == 2, ]

## Plots

ggplot(Freight_Data_Clust2, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust2, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

# This cluster has the data of customer miles in between 70 - 800
#hist(Freight_Data_Clust2$CUSTOMER_MILES)
plot(density(Freight_Data_Clust2$CUSTOMER_MILES))

# This cluster has the data of weight in between 30000 - 50000
#hist(Freight_Data_Clust2$WEIGHT)
plot(density(Freight_Data_Clust2$WEIGHT))

# order cost in between 250 - 1500
#hist(Freight_Data_Clust2$ORDER_COST)
plot(density(Freight_Data_Clust2$ORDER_COST))

## Cluster (3)

Freight_Data_Clust3 <- temp_out1[temp_out1$CLUSTER_NUM == 3, ]

## Plots

ggplot(Freight_Data_Clust3, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust3, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 100 - 1100
#hist(Freight_Data_Clust3$CUSTOMER_MILES)
plot(density(Freight_Data_Clust3$CUSTOMER_MILES))

## This cluster has the data of weights in between 5000 - 25000
#hist(Freight_Data_Clust3$WEIGHT)
plot(density(Freight_Data_Clust3$WEIGHT))

## order cost in between 100 - 2000
#hist(Freight_Data_Clust3$ORDER_COST)
plot(density(Freight_Data_Clust3$ORDER_COST))

##cluster (4)
Freight_Data_Clust4 <- temp_out1[temp_out1$CLUSTER_NUM== 4, ]

## Plots
ggplot(Freight_Data_Clust4, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust4, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 1000 - 3000
#hist(Freight_Data_Clust4$CUSTOMER_MILES)
plot(density(Freight_Data_Clust4$CUSTOMER_MILES))

## This cluster has the data of weights in between 25000 - 45000
#hist(Freight_Data_Clust4$WEIGHT)
plot(density(Freight_Data_Clust4$WEIGHT))

## order cost in between 100 - 5000
#hist(Freight_Data_Clust4$ORDER_COST)
plot(density(Freight_Data_Clust4$ORDER_COST))

##Linear Regression

## Linear Reg on 1st cluster

Freight_Data_Clust1$CLUSTER_NUM <- NULL

## Split the cluster (1) into test & Train (70:30)

rows1<-seq(1,nrow(Freight_Data_Clust1),1)
set.seed(1234)
trainrows_clust1<-sample(rows1,0.7*nrow(Freight_Data_Clust1))
Train_clust1<-Freight_Data_Clust1[trainrows_clust1,]
Test_clust1<- Freight_Data_Clust1[-trainrows_clust1,]

## Linear Reg Model

Linreg_clust1 <- lm(ORDER_COST ~ .,data=Train_clust1)
#summary(Linreg_clust1)

## Results
#Multiple R-squared:  0.7314,	Adjusted R-squared:  0.7263 
#F-statistic: 143.9 on 79 and 4175 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust1)

## Evaluation on Train

library(DMwR)
train_lm_clust1 <- regr.eval(Train_clust1$ORDER_COST, predict(Linreg_clust1,Train_clust1))
train_lm_clust1

## Results
#mae          mse         rmse         mape 
#113.282481 29461.239261   171.642766     0.125915  

## Evaluation on Test

remove = c("Ohio River -> Ohio River")
remove1 = c("NY_ROC -> OH_COL")
remove2 = c("Sunday")

Test_clust1 <- Test_clust1[!Test_clust1$PICK_DEL_REGION %in% remove,]
Test_clust1 <- Test_clust1[!Test_clust1$PICK_DEL_MARKET %in% remove1,]
Test_clust1 <- Test_clust1[!Test_clust1$CREATED_DAY %in% remove2,]

test_lm_clust1 <- regr.eval(Test_clust1$ORDER_COST, predict(Linreg_clust1,Test_clust1))
test_lm_clust1

## Results
#mae          mse         rmse         mape 
#1.257479e+02 3.863184e+04 1.965499e+02 1.372295e-01 

## Linear Reg on 2nd cluster

Freight_Data_Clust2$CLUSTER_NUM <- NULL

## Split the cluster (1) into test & Train (70:30)

rows2<-seq(1,nrow(Freight_Data_Clust2),1)
set.seed(1234)
trainrows_clust2<-sample(rows2,0.7*nrow(Freight_Data_Clust2))
Train_clust2<-Freight_Data_Clust2[trainrows_clust2,]
Test_clust2<- Freight_Data_Clust2[-trainrows_clust2,]

## Linear Reg Model

Linreg_clust2 <- lm(ORDER_COST ~ .,data=Train_clust2)
#summary(Linreg_clust2)

## Results

#Multiple R-squared:  0.7006,	Adjusted R-squared:  0.6959 
#F-statistic: 148.7 on 69 and 4384 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust2)

## Evaluation on Train
library(DMwR)
train_lm_clust2 <- regr.eval(Train_clust2$ORDER_COST, predict(Linreg_clust2,Train_clust2))
train_lm_clust2

## Results
#        mae          mse         rmse         mape 
#56.6030572 9322.9062460   96.5551979    0.1140783 

## Evaluation on Test

remove1 = c("VA_ROA -> MD_BAL")
Test_clust2 <- Test_clust2[!Test_clust2$PICK_DEL_MARKET %in% remove1,]

test_lm_clust2 <- regr.eval(Test_clust2$ORDER_COST, predict(Linreg_clust2,Test_clust2))
test_lm_clust2

## Results
#         mae          mse         rmse         mape 
#57.6471109 9662.8167641   98.2996275    0.1163318 

## Linear Reg on 3rd cluster

Freight_Data_Clust3$CLUSTER_NUM <- NULL

## Split the cluster (1) into test & Train (70:30)

rows3<-seq(1,nrow(Freight_Data_Clust3),1)
set.seed(1234)
trainrows_clust3<-sample(rows3,0.7*nrow(Freight_Data_Clust3))
Train_clust3<-Freight_Data_Clust3[trainrows_clust3,]
Test_clust3<- Freight_Data_Clust3[-trainrows_clust3,]

## Linear Reg Model

Linreg_clust3 <- lm(ORDER_COST ~ .,data=Train_clust3)
#summary(Linreg_clust3)

## Results
#Multiple R-squared:  0.7889,	Adjusted R-squared:  0.7813 
#F-statistic: 104.2 on 79 and 2202 DF,  p-value: < 2.2e-16

## Plot
par(mfrow=c(2,2))
plot(Linreg_clust3)

## Evaluation on Train

library(DMwR)
train_lm_clust3 <- regr.eval(Train_clust3$ORDER_COST, predict(Linreg_clust3,Train_clust3))
train_lm_clust3

## Results
#         mae          mse         rmse         mape 
#1.800937e+02 6.445793e+04 2.538857e+02 1.631142e-01 

## Evaluation on Test

remove1 = c("IN_IND -> PA_ALL")
remove = c("Carolinas -> Lower Atlantic","Great Lakes -> Great Lakes")
Test_clust3 <- Test_clust3[!Test_clust3$PICK_DEL_REGION %in% remove,]
Test_clust3 <- Test_clust3[!Test_clust3$PICK_DEL_MARKET %in% remove1,]

test_lm_clust3 <- regr.eval(Test_clust3$ORDER_COST, predict(Linreg_clust3,Test_clust3))
test_lm_clust3

## Results
#         mae          mse         rmse         mape 
#2.002846e+02 8.614709e+04 2.935083e+02 1.644825e-01 

## Linear Reg on 4th cluster

Freight_Data_Clust4$CLUSTER_NUM <- NULL
Freight_Data_Clust4$PICK_DEL_MARKET <- NULL

## Split the cluster (4) into test & Train (70:30)

rows4<-seq(1,nrow(Freight_Data_Clust4),1)
set.seed(1234)
trainrows_clust4<-sample(rows4,0.7*nrow(Freight_Data_Clust4))
Train_clust4<-Freight_Data_Clust4[trainrows_clust4,]
Test_clust4<- Freight_Data_Clust4[-trainrows_clust4,]

## Linear Reg Model

Linreg_clust4 <- lm(ORDER_COST ~ .,data=Train_clust4)
#summary(Linreg_clust4)

## Results
#Multiple R-squared:  0.6161,	Adjusted R-squared:  0.6049 
#F-statistic: 54.81 on 20 and 683 DF,  p-value: < 2.2e-16

## Plots

par(mfrow=c(2,2))
plot(Linreg_clust4)

## Evaluation on Train

train_lm_clust4 <- regr.eval(Train_clust4$ORDER_COST, predict(Linreg_clust4,Train_clust4))
train_lm_clust4

## Results
#         mae          mse         rmse         mape 
#4.023579e+02 2.969211e+05 5.449047e+02 1.887735e-01 

## Evaluation on Test

remove = c("South Central -> South Central", "Southeast -> Great Lakes", "Upper Atlantic -> Ohio River","Great Lakes -> Great Lakes")
Test_clust4 <- Test_clust4[!Test_clust4$PICK_DEL_REGION %in% remove,]

test_lm_clust4 <- regr.eval(Test_clust4$ORDER_COST, predict(Linreg_clust4,Test_clust4))
test_lm_clust4

## Results
#         mae          mse         rmse         mape 
#3.840713e+02 2.614047e+05 5.112775e+02 1.437333e-01  

### Final Errors
finalerros <- data.frame(rbind(train_lm_clust1,test_lm_clust1,
                               train_lm_clust2,test_lm_clust2,
                               train_lm_clust3,test_lm_clust3,
                               train_lm_clust4,test_lm_clust4))
finalerros

##Results

#                 mae        mse      rmse      mape
#train_lm_clust1 113.28248  29461.239 171.64277 0.1259150
#test_lm_clust1  125.74788  38631.845 196.54985 0.1372295
#train_lm_clust2  56.60306   9322.906  96.55520 0.1140783
#test_lm_clust2   57.64711   9662.817  98.29963 0.1163318
#train_lm_clust3 180.09371  64457.928 253.88566 0.1631142
#test_lm_clust3  200.28459  86147.093 293.50825 0.1644825
#train_lm_clust4 402.35789 296921.145 544.90471 0.1887735
#test_lm_clust4  384.07134 261404.677 511.27750 0.1437333

################# End of Clustering ############################

########## Remove not required Variables ##############
rm(temp_df1,clus,finalerros)
rm(Freight_Data_Clust1,Freight_Data_Clust2,Freight_Data_Clust3,Freight_Data_Clust4)
rm(rows1,rows2,rows3,rows4)
rm(trainrows_clust1,trainrows_clust2,trainrows_clust3,trainrows_clust4)
rm(Train_clust1,Train_clust2,Train_clust3,Train_clust4)
rm(Test_clust1,Test_clust2,Test_clust3,Test_clust4)
rm(Linreg_clust1,Linreg_clust2,Linreg_clust3,Linreg_clust4)
rm(train_lm_clust1,train_lm_clust2,train_lm_clust3,train_lm_clust4)
rm(test_lm_clust1,test_lm_clust2,test_lm_clust3,test_lm_clust4)

##################################################################

############ Start of Clustering #################################
## With only two variable "CUSTOMER_MILES","DELIVERY_DAY" 
## with 3 Clusters only

## Normailze/Standardize all the variables
library(vegan)
temp_df1 <- decostand(Freight_Data_Clust[,-1],"range")
#View(temp_df1)

## Finding the right value of "K"
wss <- 0
for (i in 1:15) {
  set.seed(12345)
  wss[i] <- sum(kmeans(temp_df1,centers=i)$withinss)
}

## Ploting Elbow Graph

par(mfrow=c(1,1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

## Cluster using 'k-means' algorithm (k=4)

set.seed(12345)
clus<-kmeans(temp_df1, 3 , iter.max = 100)
table(clus$cluster)

#1     2     3 
#3632 12107   972

clus$centers
#    CUSTOMER_MILES DELIVERY_DAY
#1     0.18769092   0.18771179
#2     0.08385117   0.05925371
#3     0.48142626   0.28315923

#clus
#Within cluster sum of squares by cluster:
#  [1] 34.63208 38.28121 21.95941
#(between_SS / total_SS =  71.5 %)

# Plots
par(mfrow=c(1,1))
plot(temp_df1,col=clus$cluster)

## Create an new dataset adding the cluster numbers

temp_out1 <- cbind(Freight_Data_No_Dup_Clust, CLUSTER_NUM = clus$cluster)

## Cluster (1)

Freight_Data_Clust1 <- temp_out1[temp_out1$CLUSTER_NUM == 1, ]

## Plots

ggplot(Freight_Data_Clust1, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()
ggplot(Freight_Data_Clust1, aes(x= jitter(DELIVERY_DAY), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 200 - 1500
#hist(Freight_Data_Clust1$CUSTOMER_MILES)
plot(density(Freight_Data_Clust1$CUSTOMER_MILES))

## This cluster has the data of weights in between 5000 - 45000
#hist(Freight_Data_Clust1$WEIGHT)
plot(density(Freight_Data_Clust1$WEIGHT))

## order cost in between 100 - 3000
#hist(Freight_Data_Clust1$ORDER_COST)
plot(density((Freight_Data_Clust1$ORDER_COST)))

## Cluster (2)

Freight_Data_Clust2 <- temp_out1[temp_out1$CLUSTER_NUM == 2, ]

## Plots

ggplot(Freight_Data_Clust2, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust2, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

# This cluster has the data of customer miles in between 70 - 800
#hist(Freight_Data_Clust2$CUSTOMER_MILES)
plot(density(Freight_Data_Clust2$CUSTOMER_MILES))

# This cluster has the data of weights in between 100 - 60000
#hist(Freight_Data_Clust2$WEIGHT)
plot(density(Freight_Data_Clust2$WEIGHT))

# order cost in between 250 - 1600
#hist(Freight_Data_Clust2$ORDER_COST)
plot(density(Freight_Data_Clust2$ORDER_COST))


## Cluster (3)

Freight_Data_Clust3 <- temp_out1[temp_out1$CLUSTER_NUM == 3, ]

## Plots

ggplot(Freight_Data_Clust3, aes(x= CUSTOMER_MILES, y= WEIGHT)) + geom_point()
ggplot(Freight_Data_Clust3, aes(x= jitter(CUSTOMER_MILES), y= jitter(ORDER_COST))) + geom_point()

## This cluster has the data of customer miles in between 1000 - 3000
#hist(Freight_Data_Clust3$CUSTOMER_MILES)
plot(density(Freight_Data_Clust3$CUSTOMER_MILES))

## This cluster has the data of weights in between 100 - 50000
#hist(Freight_Data_Clust3$WEIGHT)
plot(density(Freight_Data_Clust3$WEIGHT))

## order cost  1500 - 5000
#hist(Freight_Data_Clust3$ORDER_COST)
plot(density(Freight_Data_Clust3$ORDER_COST))

##Linear Regression

## Linear Reg on 1st cluster

Freight_Data_Clust1$CLUSTER_NUM <- NULL

## Split the cluster (1) into test & Train (70:30)

rows1<-seq(1,nrow(Freight_Data_Clust1),1)
set.seed(1234)
trainrows_clust1<-sample(rows1,0.7*nrow(Freight_Data_Clust1))
Train_clust1<-Freight_Data_Clust1[trainrows_clust1,]
Test_clust1<- Freight_Data_Clust1[-trainrows_clust1,]

## Linear Reg Model

Linreg_clust1 <- lm(ORDER_COST ~ .,data=Train_clust1)
#summary(Linreg_clust1)

## Results
#Multiple R-squared:  0.7801,	Adjusted R-squared:  0.7728 
#F-statistic: 106.4 on 82 and 2459 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust1)

## Evaluation on Train

library(DMwR)
train_lm_clust1 <- regr.eval(Train_clust1$ORDER_COST, predict(Linreg_clust1,Train_clust1))
train_lm_clust1

## Results
#mae          mse         rmse         mape 
#1.852619e+02 6.910331e+04 2.628751e+02 1.600499e-01 

## Evaluation on Train

test_lm_clust1 <- regr.eval(Test_clust1$ORDER_COST, predict(Linreg_clust1,Test_clust1))
test_lm_clust1

## Results
#mae          mse         rmse         mape 
#1.966980e+02 8.080525e+04 2.842626e+02 1.794879e-01 

## Linear Reg on 2nd cluster

Freight_Data_Clust2$CLUSTER_NUM <- NULL

## Split the cluster (2) into test & Train (70:30)

rows2<-seq(1,nrow(Freight_Data_Clust2),1)
set.seed(1234)
trainrows_clust2<-sample(rows2,0.7*nrow(Freight_Data_Clust2))
Train_clust2<-Freight_Data_Clust2[trainrows_clust2,]
Test_clust2<- Freight_Data_Clust2[-trainrows_clust2,]

## Linear Reg Model

Linreg_clust2 <- lm(ORDER_COST ~ .,data=Train_clust2)
#summary(Linreg_clust2)

## Results
#Multiple R-squared:  0.8039,	Adjusted R-squared:  0.8019 
#F-statistic: 399.7 on 86 and 8387 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust2)

## Evaluation on Train

library(DMwR)
train_lm_clust2 <- regr.eval(Train_clust2$ORDER_COST, predict(Linreg_clust2,Train_clust2))
train_lm_clust2

## Results
#mae          mse         rmse         mape 
#8.702783e+01 1.923424e+04 1.386876e+02 1.300547e-01

## Evaluation on Test

test_lm_clust2 <- regr.eval(Test_clust2$ORDER_COST, predict(Linreg_clust2,Test_clust2))
test_lm_clust2

## Results
#mae          mse         rmse         mape 
#9.019242e+01 2.058020e+04 1.434580e+02 1.329716e-01 

## Linear Reg on 3rd cluster

Freight_Data_Clust3$CLUSTER_NUM <- NULL
Freight_Data_Clust3$PICK_DEL_MARKET <- NULL

## Split the cluster (3) into test & Train (70:30)

rows3<-seq(1,nrow(Freight_Data_Clust3),1)
set.seed(1234)
trainrows_clust3<-sample(rows3,0.7*nrow(Freight_Data_Clust3))
Train_clust3<-Freight_Data_Clust3[trainrows_clust3,]
Test_clust3<- Freight_Data_Clust3[-trainrows_clust3,]

## Linear Reg Model

Linreg_clust3 <- lm(ORDER_COST ~ .,data=Train_clust3)
summary(Linreg_clust3)

## Results
#Multiple R-squared:  0.6177,	Adjusted R-squared:  0.6061 
#F-statistic: 53.25 on 20 and 659 DF,  p-value: < 2.2e-16

## Plot

par(mfrow=c(2,2))
plot(Linreg_clust3)

## Evaluation on Train

library(DMwR)
train_lm_clust3 <- regr.eval(Train_clust3$ORDER_COST, predict(Linreg_clust3,Train_clust3))
train_lm_clust3

## Results
#mae          mse         rmse         mape 
#3.800446e+02 2.625281e+05 5.123750e+02 1.647014e-01

## Evaluation on Test

remove = c("Great Lakes -> Lower Midwest", "Great Lakes -> South Central")
Test_clust3 <- Test_clust3[!Test_clust3$PICK_DEL_REGION %in% remove,]

test_lm_clust3 <- regr.eval(Test_clust3$ORDER_COST, predict(Linreg_clust3,Test_clust3))
test_lm_clust3

## Results
#mae          mse         rmse         mape 
#4.541509e+02 3.772688e+05 6.142221e+02 2.187172e-01 

### Final Errors

finalerros <- data.frame(rbind(train_lm_clust1,test_lm_clust1,
                               train_lm_clust2,test_lm_clust2,
                               train_lm_clust3,test_lm_clust3))
finalerros

##Results

#                 mae       mse      rmse      mape
#train_lm_clust1 185.26193  69103.31 262.8751 0.1600499
#test_lm_clust1  196.69800  80805.25 284.2626 0.1794879
#train_lm_clust2  87.02783  19234.24 138.6876 0.1300547
#test_lm_clust2   90.19242  20580.20 143.4580 0.1329716
#train_lm_clust3 380.04458 262528.12 512.3750 0.1647014
#test_lm_clust3  454.15093 377268.79 614.2221 0.2187172

############## End Of Clustering ####################################

