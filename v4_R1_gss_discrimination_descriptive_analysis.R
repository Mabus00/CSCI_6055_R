
# MAKE SURE TO RUN R1_gss_discrimination_dataprep.R : To prepare the data for the analysis

######################################################################################################

library(survey)
library(gtsummary)
library(haven)
library(tidyverse)
library(magrittr)
library(plyr)

#packages for making a pretty table
library(gtExtras)
library(gt)

library (berryFunctions) #this package allows me to add in empty rows 


##################################

options(survey.lonely.psu = "adjust")
options(na.action="na.pass")

#prepare survey data

dstrat<-svydesign(id=~vpsu,strata=~vstrat, weights=~wtssall, 
                  data=b, nest=T)

#################################################################################
#####                 Need to subset to Ballot 3   because
#####                 this is the ballot that contains all of the data we need
#(every day discrimination & nature variables #

#also going to subset to observations in ballot 3 that have the discrimination variable

#################################################################################

dstrat<-subset(dstrat, ballot==3) #subset to ballot 3

age <- svyquantile(~age, design = dstrat, quantile=c(0.5), na.rm=T)
# Extract quantile and standard error
quantile_value <- round(age$age["0.5", "quantile"], 2)
standard_error <- round(age$age["0.5", "se"], 2)
# Combine into a vector
# Combine into a data frame
age_df <- data.frame(median = quantile_value, se = standard_error)
# Combine into a vector
colnames(age_df)<-c('median or mean', 'se')

disc <- svyquantile(~disc, design = dstrat, quantile=c(0.5), na.rm=T)
# Extract quantile and standard error
quantile_value <- round(disc$disc["0.5", "quantile"], 2)
standard_error <- round(disc$disc["0.5", "se"], 2)
# Combine into a vector
# Combine into a data frame
disc_df <- data.frame(median = quantile_value, se = standard_error)
# Combine into a vector
colnames(disc_df)<-c('median or mean', 'se')





disc_notscaled <- svyquantile(~disc_notscaled, design = dstrat, quantile=c(0.5), na.rm=T)
# Extract quantile and standard error
quantile_value <- round(disc_notscaled$disc_notscaled["0.5", "quantile"], 2)
standard_error <- round(disc_notscaled$disc_notscaled["0.5", "se"], 2)
# Combine into a vector
# Combine into a data frame
disc_notscaled_df <- data.frame(median = quantile_value, se = standard_error)
# Combine into a vector
colnames(disc_notscaled_df)<-c('median or mean', 'se')




#for additional IQR addition to the table

d_iqr<-svyquantile(~disc, design = dstrat, quantile=c(0.25, 0.5, 0.75), na.rm=T)
a_iqr<-svyquantile(~age, design = dstrat, quantile=c(0.25, 0.5, 0.75), na.rm=T)

dns_iqr<-svyquantile(~disc_notscaled, design = dstrat, quantile=c(0.25, 0.5, 0.75), na.rm=T)


# Extract quantile and standard error
quantile_value <- round(disc$disc["0.5", "quantile"], 2)
standard_error <- round(disc$disc["0.5", "se"], 2)

quantile_value <- round(disc_notscaled$disc_notscaled["0.5", "quantile"], 2)
standard_error <- round(disc_notscaled$disc_notscaled["0.5", "se"], 2)


# Combine into a vector
# Combine into a data frame
disc_df <- data.frame(median = quantile_value, se = standard_error)
# Combine into a vector
colnames(disc_df)<-c('median or mean', 'se')

#income<-as.data.frame(svymean(~income_cat, dstrat, na.rm=T))
#colnames(income)<-c('median or mean', 'se')

gender<-as.data.frame(svymean(~as.factor(genderid), dstrat, na.rm=T))
colnames(gender)<-c('median or mean', 'se')

race<-as.data.frame(svymean(~as.factor(race_self3cat), dstrat, na.rm=T))
colnames(race)<-c('median or mean', 'se')

nataccess<-as.data.frame(svymean(~as.factor(nataccess_yn), dstrat, na.rm=T))
colnames(nataccess)<-c('median or mean', 'se')

nattimeok<-as.data.frame(svymean(~as.factor(nattimeok_yn), dstrat, na.rm=T))
colnames(nattimeok)<-c('median or mean', 'se')

natsat_yn<-as.data.frame(svymean(~as.factor(natsat_yn), dstrat, na.rm=T))
colnames(natsat_yn)<-c('median or mean', 'se')

nattime_yn<-as.data.frame(svymean(~as.factor(nattime_yn), dstrat, na.rm=T))
colnames(nattime_yn)<-c('median or mean', 'se')

work<-as.data.frame(svymean(~as.factor(workcat), dstrat, na.rm=T))
colnames(work)<-c('median or mean', 'se')


region<-as.data.frame(svymean(~as.factor(region_cat), dstrat, na.rm=T))
colnames(region)<-c('median or mean', 'se')


descriptive<-as.data.frame(rbind( age_df,  disc_df, disc_notscaled_df, gender,race,
                                  work, 
                                  region, 
                                  nataccess, nattimeok, natsat_yn, nattime_yn))

########################################################################################
#create a descriptive column 
descriptive$var<-c("Age (years)", 
                   "Discrimination", "Discrimination, not scaled",
                   "Man", "Marginalized gender", "Missing",
                   "White", "Black", "Other", "Hispanic/Latinx",
                  # "< 20K", "20K to < 50K", "50K to < 90K", "90K to < 150K", "150K +","Missing Income",
                   "In school", "Keeping house", "Other", "Unemployed or retired", "Working for pay",
                  # "Safe", "Not safe",
                   #"Not segregated", "Segregated",
                   "Eastern N Central", "Eastern S Central", "Middle Atlantic", "Mountain", "New England", "Pacific",
                   "South Atlantic", "Western North Central", "Western South Central",
                   "Have easy access to nature", "Does not have easy access to nature",
                   "Spend as much as time would like in nature", "Does not spend as much time as would like in nature",
                   "Satisfied with experience of nature", "Not satisfied with experience of nature",
                   "Spend at least one day per week in natural settings", "Does not spend at least one day per week in natural settings")


descriptive_final<-as.data.frame(cbind(descriptive$var, as.numeric(round(descriptive$"median or mean", 2)), as.numeric(round(descriptive$se, 2))))
colnames(descriptive_final)<-c("var", "Median or Proportion", "SE")

setwd("C:/Users/lhs36/OneDrive - Drexel University/GSS_green_discrimination/Output")
write.csv(descriptive_final, "tableone_descriptive_updated02082024.csv")



################################
# Additional descriptives for Table 2 of the manuscript
#Median IQR by categories of race and gender


d_iqr_gender <- svyby(~disc, by = ~genderid, design = dstrat, 
                      FUN = svyquantile, quantile = c(0.25, 0.5, 0.75), na.rm = TRUE)


d_iqr_race <- svyby(~disc, by = ~nhwhite2cat, design = dstrat, 
                    FUN = svyquantile, quantile = c(0.25, 0.5, 0.75), na.rm = TRUE)


d_iqr_race_mean <- svyby(~disc, by = ~nhwhite2cat, design = dstrat, 
                         FUN = svymean, na.rm = TRUE)


d_iqr_gender_mean <- svyby(~disc, by = ~genderid, design = dstrat, 
                           FUN = svymean, na.rm = TRUE)

d_mean<-svymean(~disc, design = dstrat, na.rm=T)


d_iqr <- svyby(~disc,  design = dstrat, 
                    FUN = svyquantile, quantile = c(0.25, 0.5, 0.75), na.rm = TRUE)



################################

dns_iqr_gender <- svyby(~disc_notscaled, by = ~genderid, design = dstrat, 
                      FUN = svyquantile, quantile = c(0.25, 0.5, 0.75), na.rm = TRUE)


dns_iqr_race <- svyby(~disc_notscaled, by = ~nhwhite2cat, design = dstrat, 
                    FUN = svyquantile, quantile = c(0.25, 0.5, 0.75), na.rm = TRUE)



dns_iqr_race_mean <- svyby(~disc_notscaled, by = ~nhwhite2cat, design = dstrat, 
                         FUN = svymean, na.rm = TRUE)


dns_iqr_gender_mean <- svyby(~disc_notscaled, by = ~genderid, design = dstrat, 
                           FUN = svymean, na.rm = TRUE)


dns_mean<-svymean(~disc_notscaled, design = dstrat,  na.rm=T)

dns_iqr<-svyquantile(~disc_notscaled, design = dstrat, quantile=c(0.25, 0.5, 0.75), na.rm=T)

