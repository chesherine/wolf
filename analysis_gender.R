rm(list = ls())

require("readr")
require('data.table')



gender_data <- read_delim('gender_data.csv',  "\t", escape_double = FALSE, trim_ws = TRUE)
nrow(gender_data[which(gender_data$STATUS1=='yes'),])
#79
nrow(gender_data[which(gender_data$STATUS1=='no'),])
#20

gender_suc<-gender_data[which(gender_data$STATUS1=='yes'),]
gender_non_suc<-gender_data[which(gender_data$STATUS1=='no'),]

chisq.test(x=table(gender_non_suc$SEX))
chisq.test(x=table(gender_suc$SEX))

chisq.test(x=table(gender_non_suc$PHASE))

table(gender_suc$STATUS_Genitals)

table(gender_data$METHOD)/nrow(gender_data)

table(gender_data$SEX)/nrow(gender_data)
sum(100*table(gender_data$PHASE)/nrow(gender_data))

