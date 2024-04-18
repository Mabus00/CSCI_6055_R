

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

############################################################

#FULL adjustment set based on DAG 
# Region of the country, Working status (employed vs not), Age


#explore as modifiers: #genderid
#nhwhite2cat

#workcat
#region
#Age

# To assess nonlinear associations, first run models using spline terms with varying degrees of freedom
# AND output AIC stats to compare 

library(splines)


model_linearity_nattimeok3<-svyglm(nattimeok_yn~ns(disc_c, df=3) + as.factor(workcat) + age + 
                              as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value3 <- AIC(model_linearity_nattimeok3) #4538.826825

model_linearity_nattimeok2<- svyglm(nattimeok_yn~ns(disc_c, df=2) + as.factor(workcat) + age + 
                                      as.factor(region_cat), design=dstrat, na.action= na.omit)


aic_value2 <- AIC(model_linearity_nattimeok2) #4535.218482 


model_linearity_nattimeok1<-svyglm(nattimeok_yn~(disc_c) + + as.factor(workcat) + age + 
                                     as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value1 <- AIC(model_linearity_nattimeok1) #4531.109821

nattimeokaic<-rbind(aic_value3, aic_value2, aic_value1)


##############natsat_yn


model_linearity_natsat3<-svyglm(natsat_yn~ns(disc_c, df=3) + as.factor(workcat) + age + 
                                     as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value3 <- AIC(model_linearity_natsat3) #4538.826825

model_linearity_natsat2<- svyglm(natsat_yn~ns(disc_c, df=2) + as.factor(workcat) + age + 
                                      as.factor(region_cat), design=dstrat, na.action= na.omit)


aic_value2 <- AIC(model_linearity_natsat2) #4535.218482 


model_linearity_natsat1<-svyglm(natsat_yn~(disc_c) + + as.factor(workcat) + age + 
                                     as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value1 <- AIC(model_linearity_natsat1) #4531.109821


natsataic<-rbind(aic_value3, aic_value2, aic_value1)

##############################################nattime_yn


model_linearity_nattime3<-svyglm(nattime_yn~ns(disc_c, df=3) + as.factor(workcat) + age + 
                                  as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value3 <- AIC(model_linearity_nattime3) #4538.826825

model_linearity_nattime2<- svyglm(nattime_yn~ns(disc_c, df=2) + as.factor(workcat) + age + 
                                   as.factor(region_cat), design=dstrat, na.action= na.omit)


aic_value2 <- AIC(model_linearity_nattime2) #4535.218482 


model_linearity_nattime1<-svyglm(nattime_yn~(disc_c) + + as.factor(workcat) + age + 
                                  as.factor(region_cat), design=dstrat, na.action= na.omit)

aic_value1 <- AIC(model_linearity_nattime1) #4531.109821



nattimeaic<-rbind(aic_value3, aic_value2, aic_value1)

#Linearity assumption ok...

###############################################################
# MAIN : NATTIME OK


model1<-svyglm(nattimeok_yn~(disc) + 
                  as.factor(workcat) + age + 
                as.factor(region_cat) ,
               family = poisson (link= "log"), 
               design=dstrat,na.action= na.omit)
#MEN
model2<-svyglm(nattimeok_yn~disc + as.factor(genderid) +  disc*as.factor(genderid)
               + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat, na.action=na.omit)

#WOMEN
model3<-svyglm(nattimeok_yn~disc + as.factor(genderidr) + disc*as.factor(genderidr) + 
                 + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
                design=dstrat, na.action=na.omit)

#Missing

model3a<-svyglm(nattimeok_yn~disc_c + as.factor(genderidr2) +disc*as.factor(genderidr2) + 
                  + as.factor(workcat) + age + 
                  as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat,
               na.action=na.omit)

model_foraic1<-svyglm(nattimeok_yn~(disc) + as.factor(genderid) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)

# WHITE

model4<-svyglm(nattimeok_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                 age + as.factor(nhwhite2cat) + disc*as.factor(nhwhite2cat), 
  family = poisson (link= "log"),  design=dstrat,
  na.action=na.omit)

# NON WHITE

model5<-svyglm(nattimeok_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                 age + as.factor(nhwhite2catr) + disc*as.factor(nhwhite2catr), 
               family = poisson (link= "log"),  design=dstrat,
               na.action=na.omit)



model_foraic2<-svyglm(nattimeok_yn~(disc) + as.factor(nhwhite2cat) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)


########################################################################################################################

# nature satisfaction natsat_yn

model6<-svyglm(natsat_yn~(disc) + 
                 as.factor(workcat) + age + 
                 as.factor(region_cat) ,
               family = poisson (link= "log"), 
               design=dstrat,na.action= na.omit)
#MEN

model7<-svyglm(natsat_yn~disc + as.factor(genderid) +  disc*as.factor(genderid)
               + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat, na.action=na.omit)

#WOMEN
model8<-svyglm(natsat_yn~disc + as.factor(genderidr) + disc*as.factor(genderidr) + 
                 + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat, na.action=na.omit)

#Missing

model8a<-svyglm(natsat_yn~disc_c + as.factor(genderidr2) +disc*as.factor(genderidr2) + 
                  + as.factor(workcat) + age + 
                  as.factor(region_cat) , family = poisson (link= "log"), 
                design=dstrat,
                na.action=na.omit)

model_foraic3<-svyglm(natsat_yn~(disc) + as.factor(genderid) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)

# WHITE

model9<-svyglm(natsat_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                 age + as.factor(nhwhite2cat) + disc*as.factor(nhwhite2cat), 
               family = poisson (link= "log"),  design=dstrat,
               na.action=na.omit)

# NON WHITE

model10<-svyglm(natsat_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                 age + as.factor(nhwhite2catr) + disc*as.factor(nhwhite2catr), 
               family = poisson (link= "log"),  design=dstrat,
               na.action=na.omit)

model_foraic4<-svyglm(natsat_yn~(disc) + as.factor(nhwhite2cat) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)

m1<-c("nattimeok", "main", as.numeric(summary(model1)$coefficients[2,1]), as.numeric(summary(model1)$coefficients[2,2]), summary(model1)$deviance)
m2<-c("nattimeok", "men", summary(model2)$coefficients[2,1], summary(model2)$coefficients[2,2], summary(model2)$deviance)
m3<-c("nattimeok", "women", summary(model3)$coefficients[2,1], summary(model3)$coefficients[2,2], summary(model3)$deviance)
m3a<-c("nattimeok", "missing gender", summary(model3a)$coefficients[2,1], summary(model3a)$coefficients[2,2], summary(model3a)$deviance)
m1lrt<-c("nattimeok", "gender adj for LRT", summary(model_foraic1)$coefficients[2,1], summary(model_foraic1)$coefficients[2,2], summary(model_foraic1)$deviance)

m4<-c("nattimeok", "white", summary(model4)$coefficients[2,1], summary(model4)$coefficients[2,2], summary(model4)$deviance)
m5<-c("nattimeok", "nonwhite", summary(model5)$coefficients[2,1], summary(model5)$coefficients[2,2], summary(model5)$deviance)
m2lrt<-c("nattimeok", "race adj for LRT", summary(model_foraic2)$coefficients[2,1], summary(model_foraic2)$coefficients[2,2], summary(model_foraic2)$deviance)


m6<-c("natsat", "main", summary(model6)$coefficients[2,1], summary(model6)$coefficients[2,2], summary(model6)$deviance)
m7<-c("natsat", "men", summary(model7)$coefficients[2,1], summary(model7)$coefficients[2,2], summary(model7)$deviance)
m8<-c("natsat", "women", summary(model8)$coefficients[2,1], summary(model8)$coefficients[2,2], summary(model8)$deviance)
m8a<-c("natsat", "missing gender", summary(model8a)$coefficients[2,1], summary(model8a)$coefficients[2,2], summary(model8a)$deviance)
m3lrt<-c("natsat", "gender adj for LRT", summary(model_foraic3)$coefficients[2,1], summary(model_foraic3)$coefficients[2,2], summary(model_foraic3)$deviance)

m9<-c("natsat", "white", summary(model9)$coefficients[2,1], summary(model9)$coefficients[2,2], summary(model9)$deviance)
m10<-c("natsat", "nonwhite", summary(model10)$coefficients[2,1], summary(model10)$coefficients[2,2], summary(model10)$deviance)
m4lrt<-c("natsat", "race adj for LRT", summary(model_foraic4)$coefficients[2,1], summary(model_foraic4)$coefficients[2,2], summary(model_foraic4)$deviance)

all<-as.data.frame(rbind(m1, m2, m3, m3a, m1lrt, m4, m5, m2lrt, m6, m7, m8,m8a, m3lrt, m9, m10, m4lrt ))
colnames(all)<-c("naturevar", "subgroup", "beta", "se", "deviance")
all$beta<-as.numeric(all$beta)
all$se<-as.numeric(all$se)
all$RR<-round(exp(all$beta),2)
all$LL<-round(exp(all$beta-1.96*all$se),2)
all$UL<-round(exp(all$beta+1.96*all$se),2)

#main<-ifelse(all$naturevar=="natsat", all$subsummary(model6)$deviance,
 #            ifelse(all$naturevar=="nattimeok", summary(model1)$deviance,  NA))
             
#all$p_lrt<-pchisq((main-as.numeric(all$deviance)),1, lower.tail=F)


write.csv(all, "model_results_fulladjustment_updated_20240208.csv")

# run models with naturetime_yn as dependent variable



model11<-svyglm(nattime_yn~(disc) + 
                 as.factor(workcat) + age + 
                 as.factor(region_cat) ,
               family = poisson (link= "log"), 
               design=dstrat,na.action= na.omit)
#MEN

model12<-svyglm(nattime_yn~disc + as.factor(genderid) +  disc*as.factor(genderid)
               + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat, na.action=na.omit)

#WOMEN
model13<-svyglm(nattime_yn~disc + as.factor(genderidr) + disc*as.factor(genderidr) + 
                 + as.factor(workcat) + age + 
                 as.factor(region_cat) , family = poisson (link= "log"), 
               design=dstrat, na.action=na.omit)

#Missing

model13a<-svyglm(nattime_yn~disc_c + as.factor(genderidr2) +disc*as.factor(genderidr2) + 
                  + as.factor(workcat) + age + 
                  as.factor(region_cat) , family = poisson (link= "log"), 
                design=dstrat,
                na.action=na.omit)

model_foraic5<-svyglm(nattime_yn~(disc) + as.factor(genderid) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)

# WHITE

model14<-svyglm(nattime_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                 age + as.factor(nhwhite2cat) + disc*as.factor(nhwhite2cat), 
               family = poisson (link= "log"),  design=dstrat,
               na.action=na.omit)

# NON WHITE

model15<-svyglm(nattime_yn~disc +  as.factor(workcat) + as.factor(region_cat) + 
                  age + as.factor(nhwhite2catr) + disc*as.factor(nhwhite2catr), 
                family = poisson (link= "log"),  design=dstrat,
                na.action=na.omit)

model_foraic6<-svyglm(nattime_yn~(disc) + as.factor(nhwhite2cat) +
                        as.factor(workcat) + age + 
                        as.factor(region_cat) ,
                      family = poisson (link= "log"), 
                      design=dstrat,na.action= na.omit)


m11<-c("nat_timeoncewk", "main", summary(model11)$coefficients[2,1], summary(model11)$coefficients[2,2], summary(model11)$deviance)
m12<-c("nat_timeoncewk", "men", summary(model12)$coefficients[2,1], summary(model12)$coefficients[2,2], summary(model12)$deviance)
m13<-c("nat_timeoncewk", "women", summary(model13)$coefficients[2,1], summary(model13)$coefficients[2,2], summary(model13)$deviance)
m13a<-c("nat_timeoncewk", "missinggender", summary(model13a)$coefficients[2,1], summary(model13a)$coefficients[2,2], summary(model13a)$deviance)
m5lrt<-c("nat_timeoncewk", "gender adj for lrt", summary(model_foraic5)$coefficients[2,1], summary(model_foraic5)$coefficients[2,2], summary(model_foraic5)$deviance)
m14<-c("nat_timeoncewk", "white", summary(model14)$coefficients[2,1], summary(model14)$coefficients[2,2], summary(model14)$deviance)
m15<-c("nat_timeoncewk", "nonwhite", summary(model15)$coefficients[2,1], summary(model15)$coefficients[2,2], summary(model15)$deviance)
m6lrt<-c("nat_timeoncewk", "race adj for lrt", summary(model_foraic6)$coefficients[2,1], summary(model_foraic6)$coefficients[2,2], summary(model_foraic6)$deviance)



all<-as.data.frame(rbind(m11, m12, m13, m13a, m5lrt,  m14, m15, m6lrt))
colnames(all)<-c("naturevar", "subgroup", "beta", "se", "deviance")
all$beta<-as.numeric(all$beta)
all$se<-as.numeric(all$se)
all$RR<-round(exp(all$beta),2)
all$LL<-round(exp(all$beta-1.96*all$se),2)
all$UL<-round(exp(all$beta+1.96*all$se),2)
#main<-summary(model11)$deviance
#all$p_lrt<-pchisq((main-as.numeric(all$deviance)),1, lower.tail=F)
write.csv(all, "more_model_results_fulladjustment_updated20240208.csv")

#################################################################################################################

   