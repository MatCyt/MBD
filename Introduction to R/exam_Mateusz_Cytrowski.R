## Final Exam 

### 1. Read the df_states.csv, uploaded in the campus [ 0.5 pts ].

dfstates<- read.csv2('df_states.csv', header = TRUE, sep = ";")
dfstates

### 2. Calculate a vector with the total number of NAs in each variable in df_states [ 1.5 pts ]. 

sapply(dfstates, function(x) sum(length(which(is.na(x)))))  

# Filter the data.frame eliminating all Nas

dfstates<- dfstates[complete.cases(dfstates), ]

sum(is.na(dfstates))

# If there are any character variables in the data.frame, convert them to factor ones, except for id and name that should
# remain as characters [ 1.5 pt ].

str(dfstates)

# in my dfstates everything is already in factors. In this case Im going just to change the ID and name to characters.
# If I would have to change the characters into factors I would just add 
# additional if condition at the very beginning matching the input with a column name or specify in the loop dfstates[ ,3:14]

dfstates$id<-as.character(dfstates$id)
dfstates$name<-as.character(dfstates$name)

str(dfstates)

# Applying it to integers and numeric

int_to_num<-function(var){
  if(is.integer(var)){
    return(var<-as.numeric(as.integer(var)))
  } else {
    return(var)
  }
}


for (i in 1:ncol(dfstates)){
  
  dfstates[[i]]<-int_to_num(dfstates[[i]])
}

str(dfstates)

### 4. Analyze the mean murder rate by variable division. [ 1.5 pt ].

dfstates$Murder<-as.numeric(dfstates$Murder)

murder_rate<-aggregate(dfstates$Murder ~ dfstates$division, data = dfstates, mean)
murder_rate

# ??? Create a scatter plot with HS.Grad Vs Murder, coloured by division. 

library(ggplot2)
ggplot(dfstates, aes(x=dfstates$Murder, y=dfstates$HS.Grad, color=dfstates$division)) + geom_point()

### 5. Create a subset of the data.frame keeping only variables: id, Population, Income, Illiteracy, Life.Exp, Murder, HS.Grad
# and Area, naming the new data set numeric_data 

str(dfstates)

#for some reason some of my numeric variables are in factor format - I will change them into numeric data
dfstates$Illiteracy<-as.numeric(dfstates$Illiteracy)
dfstates$Life.Exp<-as.numeric(dfstates$Life.Exp)
dfstates$HS.Grad<-as.numeric(dfstates$HS.Grad)
str(dfstates)

#subset
numeric_data <- dfstates[ , c('id', 'Population', 'Income', 'Illiteracy', 'Life.Exp', 'Murder', 'HS.Grad', 'Area')]
str(numeric_data)


# 6. Calculate the correlation matrix and store it a data.frame called correlations [ 1.5 pts ].

#for numerical variables
cor_dfstates<-cor(dfstates[, 3:10])
cor_dfstates

#??? Plot the correlation matrix

library(corrplot)
corrplot(cor_dfstates, method = 'number', tl.col = 'black')
corrplot(cor_dfstates,type='upper', method = 'square', tl.col = 'black')

# 7. Remove the state California from numeric data and build a linear regression model that explains Murder with the
# variables Income, Population, Illiteracy, HS.Grad and Area [ 2.5 pts ]

train<-numeric_data[-4,]
train

lm_murder<-lm(train$Murder ~ train$Population + train$Income + train$Illiteracy + train$HS.Grad + train$Area)

summary(lm_murder)
lm_murder_stats<-summary(lm_murder)

# ??? Eliminate all variables non significant at a 95% of confidence

# ANSWER :  from the summary of the results only HS.Grad is significant at 95% level.

lm_murder2<-lm(train$Murder ~ train$HS.Grad)
summary(lm_murder2)
lm_murder2_stats<-summary(lm_murder2)

# Extract the the coefficients and the r-squared of the model.

str(lm_murder2_stats)

lm_murder2_stats$coefficients
lm_murder2_stats$r.squared

extract_both<-list(lm_murder2_stats$coefficients, lm_murder2_stats$r.squared)
extract_both

# ??? Predict the Murder rate for California, and calculate the estimation error.

predict<-predict(lm_murder2, numeric_data[4, ])

# 8. Save the model object in an .RData file [ 0.5 pts ].

saveRDS(lm_murder2, 'MurderModel.RData')
