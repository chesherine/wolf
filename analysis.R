rm(list = ls())
require('readr')


wolf_data <- read_delim('CSV_clear.csv',  "\t", escape_double = FALSE, trim_ws = TRUE)

nrow(wolf_data)
#98

nrow(wolf_data[which(wolf_data$STATUS2=='yes'),])
#32
round(100*nrow(wolf_data[which(wolf_data$STATUS2=='yes'),])/nrow(wolf_data),2)
#32.65% success
round(100*nrow(wolf_data[which(wolf_data$STATUS2=='no'),])/nrow(wolf_data),2)
#67.35% partial success or no success

nrow(wolf_data[which(wolf_data$STATUS1 %in% c('yes','partially')),])
#n=83
round(100*nrow(wolf_data[which(wolf_data$STATUS1 %in% c('yes','partially')),])/nrow(wolf_data),2)
#84.69% got at least partial result

#test for normal distribution
shapiro.test(wolf_data$STATUS1_NUM)
#W = 0.79183, p-value = 1.862e-10
#not normal distribution


#% of male and female participants

100*nrow(wolf_data[which(wolf_data$SEX == 'w'),])/nrow(wolf_data)
#51.02%
100*nrow(wolf_data[which(wolf_data$SEX == 'm'),])/nrow(wolf_data)
#48.98%


chisq.test(x=table(wolf_data$SEX), p =c(0.5,0.5))
#X-squared = 0.040816, df = 1, p-value = 0.8399 equal gender distribution

chisq.test(x=table(wolf_data[,c('SEX','STATUS1')]))
#Pearson's Chi-squared test

#data:  table(wolf_data[, c("SEX", "STATUS1")])
#X-squared = 3.2078, df = 2, p-value = 0.2011
# gender did not influence the result

chisq.test(x=table(wolf_data[,c('STATUS2','METHOD')]))
#Pearson's Chi-squared test
#data:  table(wolf_data[, c("STATUS2", "METHOD")])
#X-squared = 3.0123, df = 2, p-value = 0.2218
#induction method did not influence the result

g2<-summary(glm(wolf_data$STATUS1_NUM~as.numeric(as.character(wolf_data$PHASE))))
g2$coefficients
#Estimate Std. Error    t value     Pr(>|t|)
#(Intercept)                                2.22797607 0.30549208  7.2930730 9.013471e-11
#as.numeric(as.character(wolf_data$PHASE)) -0.01650505 0.06702677 -0.2462457 8.060233e-01
#phase experienece did not influence outcome

feature_mat<-matrix(ncol=4)

wolf_semi_suc<-wolf_data[which(wolf_data$STATUS1_NUM %in% c(2,3)),]
feature_mat<-as.data.frame(table(wolf_semi_suc[,c('ANATOMY','FUR','BEHAVIOR','TAIL')], useNA = "always"))
sum(feature_mat$Freq)
sum(feature_mat[which(feature_mat$ANATOMY %in% c('yes','partial')),'Freq'])

#72 out of 83 volunteers were atleat least, partially or fully, make wolve anatomy
# only 1 volunteer got full transformation with all the features - anatomy, fur, tail, behaviour
sum(feature_mat[which(feature_mat$FUR %in% c('yes','partial')),'Freq'])
#31
sum(feature_mat[which(feature_mat$BEHAVIOR %in% c('yes','partial')),'Freq'])
#15
sum(feature_mat[which(feature_mat$TAIL %in% c('yes','partial')),'Freq'])
#9





