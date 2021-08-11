#=============================================
# Credit Card Defaulters
#=============================================
rm(list=ls())
#=============================================
# Loading necessary libraries
#=============================================
library(ggplot2)
library(xtable)
library(ggpubr)
library(cowplot)
library(ggcorrplot)
library(caTools)
library(ResourceSelection)
library(ROCR)
#=============================================
# Reading in Data
#=============================================

cc <- read.csv('credit_card_default.csv', header = TRUE)
head(cc)
cc <- cc[,-1] #taking out ID column (since it corresponds to the row name)
head(cc)

#=============================================
# Data Cleaning
#=============================================
str(cc)

# change coding of sex
cc$SEX[cc$SEX=='1'] <- 'Male'
cc$SEX[cc$SEX=='2'] <- 'Female'

cc$SEX <- as.factor(cc$SEX)
cc$EDUCATION <- as.factor(cc$EDUCATION)
cc$MARRIAGE <- as.factor(cc$MARRIAGE)
cc$default.payment_next_month <- as.factor(cc$default.payment_next_month)

#### converting 0=did not pay, 1=pay ####
cc$PAY_0[cc$PAY_0 == -2] <- 0
cc$PAY_0[cc$PAY_0 == -1] <- 0
cc$PAY_0[cc$PAY_0 == 2] <- 1
cc$PAY_0[cc$PAY_0 == 3] <- 1
cc$PAY_0[cc$PAY_0 == 4] <- 1
cc$PAY_0[cc$PAY_0 == 5] <- 1
cc$PAY_0[cc$PAY_0 == 6] <- 1
cc$PAY_0[cc$PAY_0 == 7] <- 1
cc$PAY_0[cc$PAY_0 == 8] <- 1

cc$PAY_2[cc$PAY_2 == -2] <- 0
cc$PAY_2[cc$PAY_2 == -1] <- 0
cc$PAY_2[cc$PAY_2 == 2] <- 1
cc$PAY_2[cc$PAY_2 == 3] <- 1
cc$PAY_2[cc$PAY_2 == 4] <- 1
cc$PAY_2[cc$PAY_2 == 5] <- 1
cc$PAY_2[cc$PAY_2 == 6] <- 1
cc$PAY_2[cc$PAY_2 == 7] <- 1
cc$PAY_2[cc$PAY_2 == 8] <- 1


cc$PAY_3[cc$PAY_3 == -2] <- 0
cc$PAY_3[cc$PAY_3 == -1] <- 0
cc$PAY_3[cc$PAY_3 == 2] <- 1
cc$PAY_3[cc$PAY_3 == 3] <- 1
cc$PAY_3[cc$PAY_3 == 4] <- 1
cc$PAY_3[cc$PAY_3 == 5] <- 1
cc$PAY_3[cc$PAY_3 == 6] <- 1
cc$PAY_3[cc$PAY_3 == 7] <- 1
cc$PAY_3[cc$PAY_3 == 8] <- 1

cc$PAY_4[cc$PAY_4 == -2] <- 0
cc$PAY_4[cc$PAY_4 == -1] <- 0
cc$PAY_4[cc$PAY_4 == 2] <- 1
cc$PAY_4[cc$PAY_4 == 3] <- 1
cc$PAY_4[cc$PAY_4 == 4] <- 1
cc$PAY_4[cc$PAY_4 == 5] <- 1
cc$PAY_4[cc$PAY_4 == 6] <- 1
cc$PAY_4[cc$PAY_4 == 7] <- 1
cc$PAY_4[cc$PAY_4 == 8] <- 1

cc$PAY_5[cc$PAY_5 == -2] <- 0
cc$PAY_5[cc$PAY_5 == -1] <- 0
cc$PAY_5[cc$PAY_5 == 2] <- 1
cc$PAY_5[cc$PAY_5 == 3] <- 1
cc$PAY_5[cc$PAY_5 == 4] <- 1
cc$PAY_5[cc$PAY_5 == 5] <- 1
cc$PAY_5[cc$PAY_5 == 6] <- 1
cc$PAY_5[cc$PAY_5 == 7] <- 1
cc$PAY_5[cc$PAY_5 == 8] <- 1

cc$PAY_6[cc$PAY_6 == -2] <- 0
cc$PAY_6[cc$PAY_6 == -1] <- 0
cc$PAY_6[cc$PAY_6 == 2] <- 1
cc$PAY_6[cc$PAY_6 == 3] <- 1
cc$PAY_6[cc$PAY_6 == 4] <- 1
cc$PAY_6[cc$PAY_6 == 5] <- 1
cc$PAY_6[cc$PAY_6 == 6] <- 1
cc$PAY_6[cc$PAY_6 == 7] <- 1
cc$PAY_6[cc$PAY_6 == 8] <- 1

cc$PAY_0 <- as.factor(cc$PAY_0)
cc$PAY_2 <- as.factor(cc$PAY_2)
cc$PAY_3 <- as.factor(cc$PAY_3)
cc$PAY_4 <- as.factor(cc$PAY_4)
cc$PAY_5 <- as.factor(cc$PAY_5)
cc$PAY_6 <- as.factor(cc$PAY_6)


#### renaming default column ####
colnames(cc)[24] <- 'Default_Status'


# change coding of default
cc$Default_Status <- as.character(cc$Default_Status)
cc$Default_Status[cc$Default_Status == '0'] <- 'Non-Defaulter'
cc$Default_Status[cc$Default_Status == '1'] <- 'Defaulter'
cc$Default_Status <- as.factor(cc$Default_Status)
# renaming pay_0 to pay_1
colnames(cc)[6] <- 'PAY_1'

# data cleaning marriage
summary(cc$MARRIAGE)
cc$MARRIAGE <- as.character(cc$MARRIAGE)
cc$MARRIAGE[cc$MARRIAGE=='0'] <- '3' #collapsing unspecified into 'other' category
cc$MARRIAGE <- as.factor(cc$MARRIAGE)

# data cleaning education
summary(cc$EDUCATION)
cc$EDUCATION <- as.character(cc$EDUCATION)
cc$EDUCATION[cc$EDUCATION=='0'] <- '4' #collapsing unspecified into 'other' category
cc$EDUCATION[cc$EDUCATION=='5'] <- '4' 
cc$EDUCATION[cc$EDUCATION=='6'] <- '4' 
cc$EDUCATION <- as.factor(cc$EDUCATION)

# checking structure of dataset again
str(cc)
#=============================================
# Exploratory Data Analysis
#=============================================
# way more non-defaulters than defaulters; imbalanced data - could cause issues with analysis
# show this graphically:
ggplot(cc, aes(x=Default_Status,fill=Default_Status)) + 
  geom_bar(position = 'dodge') +
  geom_text(stat = 'count', aes(label=..count.., vjust=-1)) +
  #geom_text(stat = 'count', aes(label=..count.., vjust=-10)) +
  theme_minimal() +
  xlab('') +
  ylab('Count') +
  scale_fill_brewer(palette = 'PuBuGn') + 
  labs(fill='')

# sex versus default
ggplot(cc, aes(x=Default_Status,fill=SEX)) + 
  geom_bar(position = 'dodge') +
  #geom_text(stat = 'count', aes(label=..count.., vjust=-1)) +
  #geom_text(stat = 'count', aes(label=..count.., vjust=-10)) +
  theme_minimal() +
  xlab('') +
  ylab('Count') +
  scale_fill_brewer(palette = 'PuBuGn') + 
  labs(fill='')

# imbalanced dataset - way more males than females, so lets look at proportions
table1 <- table(cc$SEX, cc$Default_Status)

(9015 / (9015+2873)) *100 # percentage of males who don't default - 75.83277
(2873 / (9015+2873)) *100 # percentage of males who default - 24.16723
(14349 / (14349+3763)) *100 # percentage of females who don't default - 79.22372
(3763/ (14349+3763)) *100  # percentage of females who default - 20.77628

ggplot(cc, aes(Default_Status, group = SEX)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage") +
  facet_grid(~SEX) +
  theme_minimal() + 
  labs(fill='') +
  scale_fill_brewer(palette = 'BuGn') +
  xlab('') +
  theme(legend.position='none')

# education versus default

ggplot(cc, aes(x=Default_Status,fill=EDUCATION)) + 
  geom_bar(position = 'dodge') +
  #geom_text(stat = 'count', aes(label=..count.., vjust=-1)) +
  #geom_text(stat = 'count', aes(label=..count.., vjust=-10)) +
  theme_minimal() +
  xlab('Default Next Payment') +
  ylab('Count') +
  scale_fill_brewer(palette = 'PuBuGn') + 
  labs(fill='EDUCATION')

ggplot(cc, aes(Default_Status, group = EDUCATION)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage") +
  facet_grid(~EDUCATION) +
  theme_minimal() + 
  labs(fill='') +
  scale_fill_brewer(palette = 'BuGn') +
  xlab('') +
  theme(legend.position='none')

# marriage versus default
ggplot(cc, aes(Default_Status, group = MARRIAGE)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies") +
  facet_grid(~MARRIAGE) +
  theme_minimal() +
  labs(fill='') +
  scale_fill_brewer(palette = 'PuBuGn') +
  xlab('') +
  theme(legend.position='none')

# limit vs default
options(scipen=999)
# univariate analysis for limit
ggplot(cc, aes(x=LIMIT_BAL)) +
  geom_histogram(alpha=.2, fill='blue') +
  theme_minimal() +
  xlab('Credit Limit') +
  ylab('Density') 

ggplot(cc, aes(x=Default_Status, y=LIMIT_BAL, fill=Default_Status)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_brewer(palette='BuGn') +
  xlab('Default') +
  ylab('Credit Limit') + 
  stat_summary(fun=mean,geom="point",
               shape=20,size=5,color="blue",fill="blue")


summary(cc$LIMIT_BAL[cc$Default_Status=='Defaulter'])
summary(cc$LIMIT_BAL[cc$Default_Status=='Non-Defaulter'])
# age vs default

# univariate analysis for age
ggplot(cc, aes(x=AGE)) +
  geom_histogram(alpha=.2, fill='purple') +
  theme_minimal() +
  xlab('Age') +
  ylab('Density') 


ggplot(cc, aes(x=Default_Status, y=AGE, fill=Default_Status)) +
  geom_boxplot() +
  theme_minimal() +
  scale_fill_brewer(palette='BuGn') +
  xlab('Default') +
  ylab('Age') +   
  stat_summary(fun=mean,geom="point",
                               shape=20,size=5,color="blue",fill="blue")

summary(cc$AGE[cc$Default_Status=='Defaulter'])
summary(cc$AGE[cc$Default_Status=='Non-Defaulter'])

ggcorrplot(cor(cc[,12:23]), lab=T)
#=============================================
# Logistic Regression
#=============================================
# change coding of default
cc$Default_Status <- as.character(cc$Default_Status)
cc$Default_Status[cc$Default_Status == 'Non-Defaulter'] <- '0'
cc$Default_Status[cc$Default_Status == 'Defaulter'] <- '1'
cc$Default_Status <- as.factor(cc$Default_Status)

set.seed(101)

split = sample.split(cc$Default_Status, SplitRatio = 0.80)

final.train = subset(cc, split == TRUE)
final.test = subset(cc, split == FALSE)

finalmodel <- glm(formula=Default_Status ~ . , family = binomial(link='logit'),data = final.train)

summary(finalmodel)
#=============================================
# Logistic Regression Classification Scheme
#=============================================
# Find optimal point for threshold using ROC curve
prediction.object <- prediction(fitted(finalmodel), labels = final.train$Default_Status)

roc = performance(prediction.object,"tpr","fpr") #tpr / fpr : true / false positive rate
plot(roc, col='lightblue', lwd = 2, xlab='1-Specificity', ylab='Sensitivity')
abline(a = 0, b = 1) 

cutoffs <- data.frame(cut=roc@alpha.values[[1]], tpr=roc@y.values[[1]], spec = 1 - roc@x.values[[1]],
                      fpr=roc@x.values[[1]])
cutoffs

#Area Under Curve
performance(prediction.object,"auc")@y.values # the larger, the better, ours it 0.7

J <- cutoffs$tpr + cutoffs$spec - 1
which.max(J)
cutoffs[6432,] 
points(cutoffs[6432,4], cutoffs[6432,2], lwd=2, pch=16)
text(cutoffs[6432,4]-0.02, cutoffs[6432,2]+0.05, "Optimal cut-off", cex = 0.75)

# Test Confusion matrix 
threshold = 0.213147
predicted_cat <- ifelse(predict(finalmodel, newdata=final.test, type='response')>threshold,1,0) # predicted 0s and 1s
actual_cat<- as.numeric(final.test$Default_Status)
conf_matrix<-table(predicted_cat,actual_cat) # we cross tabulate and get a confusion / classification matrix 
conf_matrix



