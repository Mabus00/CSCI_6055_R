
library(haven)

library(gssr)

library(dplyr)

library(formattable)

GSS2018 <- read_dta("C:/Users/wayne/Dal/CSCI_6055_R/gss_green_eds/GSS2018.dta")

#names(GSS2018)

#calculate the everyday discrimination variable, which is an index based on the following variables:

#DISRSPCT R IS TREATED WITH LESS COURTESY OR RESPECT THAN OTHERS
#POORSERV R RECIEVES POORER SERVICE IN RESTAURANTS OR STORES
#NOTSMART PEOPLE ACT AS IF THEY THINK R IS NOT SMART
#AFRAIDOF PEOPLE ACT AS IF THEY ARE AFRAID OF R
#THREATEN R IS THREATENED OR HARASSED

####################################
#1      almost every day# RECODE as 6
#2  at least once a week# RECODE as 5
#3   a few times a month # RECODE as 4
#4    a few times a year # RECODE as 3
#5 less than once a year # RECODE as 2
#6                 never # RECODE as 1

####################################

# RECODE so Higher values represent higher frequency

GSS2018 <- GSS2018 %>%
  mutate(
    disrspct = as.numeric(as.character(disrspct)),
    poorserv = as.numeric(as.character(poorserv)),
    notsmart = as.numeric(as.character(notsmart)),
    afraidof = as.numeric(as.character(afraidof)),
    threaten = as.numeric(as.character(threaten))
  ) %>%
  mutate(
    disrspct = recode(disrspct, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    poorserv = recode(poorserv, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    notsmart = recode(notsmart, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    afraidof = recode(afraidof, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1),
    threaten = recode(threaten, `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)
  )



GSS2018$disc_notscaled<-(GSS2018$disrspct) + (GSS2018$poorserv) + 
  (GSS2018$notsmart) + (GSS2018$afraidof) + 
  (GSS2018$threaten)


GSS2018$disc<-scale(GSS2018$disrspct) + scale(GSS2018$poorserv) + 
  scale(GSS2018$notsmart) + scale(GSS2018$afraidof) + 
  scale(GSS2018$threaten)

ballot3<-GSS2018[which(GSS2018$ballot==3),]

#now center the reverse coded discrimination score at the median

#note: -0.46543 is the median of the distribution of disc among ballot 3 respondents

GSS2018$disc_c<-GSS2018$disc - (-0.46543)


# EDUCATION
GSS2018$educ_cat<-ifelse (GSS2018$educ %in% c(0:11), "lt_highschool",
                          ifelse(GSS2018$educ ==12, "high school",
                                 ifelse(GSS2018$educ %in% c(13,14), "three years college",
                                        ifelse(GSS2018$educ %in% c(15,16), "four years college",
                                               ifelse(GSS2018$educ %in% c(17:20), "more than four years college",NA)))))

GSS2018$educ2cat<-ifelse(GSS2018$educ %in% c(0:11), "lt_highschool",
                         ifelse(GSS2018$educ %in% c(12:20), "high school or more",NA))

# ONE MISSING

#sexnow: gender identity now. 1=woman, 2=man, 3=transgender, 4=gender not listed here. 

#put the trans/other gender ID with the women (marginalized gender id categories) because of small sample size issues

GSS2018$genderid<-ifelse(GSS2018$sexnow==2,0,# man
                         ifelse(GSS2018$sexnow==1,1,#woman
                                ifelse(GSS2018$sexnow %in% c(3,4),1,NA))) #transgender/not listed

GSS2018$genderid<-ifelse(is.na(GSS2018$genderid),99,GSS2018$genderid)                                                                          #0 = MAN, 1=woman, 2=other, 3=missing


#0   1  99 
#315 377 847 


#for the table 1 table
#GSS2018$genderid_strat<-ifelse(GSS2018$sexnow==2,0,# man
   #                            ifelse(GSS2018$sexnow==1,1,#woman
   #                                   ifelse(GSS2018$sexnow %in% c(3,4),2,NA))) #transgender/not listed

#GSS2018$genderid_strat<-ifelse(is.na(GSS2018$genderid_strat),99,GSS2018$genderid_strat)                                                                          #0 = MAN, 1=woman, 2=other, 3=missing


#reverse code the gender id impute 2 cat variable so that I can easily output 95% CIs from interaction term models
GSS2018$genderidr<-ifelse(GSS2018$genderid==1, 0,
                          ifelse(GSS2018$genderid==0, 1,99))


GSS2018$genderidr2<-ifelse(GSS2018$genderid==99, 0,
                           ifelse(GSS2018$genderid==0, 1,2))


#racecen corresponds to self-identified race

GSS2018$race_self3cat<-ifelse(GSS2018$racecen1==1,0, #white
                              ifelse(GSS2018$racecen1==2,1,#black
                                     ifelse(GSS2018$racecen1 %in% c(4:15), 2, #other
                                            ifelse(GSS2018$racecen1 == 16, 3, NA)))) #3=HISPANIC
#33 missing


#create a non-hispanic white vs not white or hispanic variable b/c of small numbers issues

GSS2018$nhwhite2cat<-ifelse(GSS2018$race_self3cat==0, 0,
                            ifelse(GSS2018$race_self3cat %in% c(1,2,3),1, NA))    

#reverse code this variable so I can include in interaction terms
GSS2018$nhwhite2catr<-ifelse(GSS2018$race_self3cat==0, 1,
                             ifelse(GSS2018$race_self3cat %in% c(1,2,3),0, NA))    

# REGION

GSS2018$region_cat<-ifelse(GSS2018$region==1, "New England",
                           ifelse(GSS2018$region==2, "Middle Atlantic",
                                  ifelse(GSS2018$region==3, "Eastern North Central",
                                         ifelse(GSS2018$region==4, "Western North Central",    
                                                ifelse(GSS2018$region==5, "South Atlantic",
                                                       ifelse(GSS2018$region==6, "Eastern South Central",
                                                              ifelse(GSS2018$region==7, "Western South Central",
                                                                     ifelse(GSS2018$region==8, "Mountain",
                                                                            ifelse(GSS2018$region==9, "Pacific",NA)))))))))


#In which of these groups did your total family income, from all sources, fall last year before taxes, that is. Just tell me the letter.
#Income16: In which of these groups did your total family income, from all sources, fall last year before taxes, that is. Just tell me the letter.


GSS2018$income_cat<-ifelse(GSS2018$income16 %in% c(1:12), "A. Less than 20K",
                           ifelse(GSS2018$income16 %in% c(13:18), "B. 20K to <50K",
                                  ifelse(GSS2018$income16 %in% c(19:21), "C. 50K to <90K",
                                         ifelse(GSS2018$income16 %in% c(22:24), "D. 90K to <150K",
                                                ifelse(GSS2018$income16 %in% c(25,26), "E. 150K +","MISSING")))))



nature<-GSS2018[c("natnotice", "natviews", "nataccess", "nattime", "natsat", "natrelax", "natactive", 
                  "natmeet", "nattimeok", "natlack", "natpark")]

#NATTIMEOK I spend as much time as I would like in natural environments
#1=strongly agree, 2=somewhat agree, 3=somewhat disagree, 4=strongly disagree

# NATTIME 
#Usually, I spend time in natural environments, such as public parks, gardens or trails, at least once a week

#natsat I am satisfied with my day to day experience of nature.
GSS2018$nattimeok_yn<-ifelse(GSS2018$nattimeok %in% c(1,2), 0,
                             ifelse(GSS2018$nattimeok %in% c(3,4), 1,NA)) #1=not enough


GSS2018$nattimeok_s<-scale(GSS2018$nattimeok)
#I have easy access to natural environments, such as public parks, gardens or trails.
GSS2018$nataccess_yn<-ifelse(GSS2018$nataccess %in% c(1,2), 0,
                             ifelse(GSS2018$nataccess %in% c(3,4), 1,NA)) #1=not enough

#Usually, I spend time in natural environments, such as public parks, gardens or trails, at least once a week
GSS2018$nattime_yn<-ifelse(GSS2018$nattime %in% c(1,2), 0,
                           ifelse(GSS2018$nattime %in% c(3,4), 1,NA)) #1=not enough

#I am satisfied with my day to day experience of nature.
GSS2018$natsat_yn<-ifelse(GSS2018$natsat %in% c(1,2), 0,
                          ifelse(GSS2018$natsat %in% c(3,4), 1,NA)) #1=not enough
GSS2018$natsat_s<-scale(GSS2018$natsat)


#create a nature appreciation index 
#NATRELAX: Natural environments are peaceful and relaxing.
#NATACTIVE: Natural environments are a good place for physical activity, such as walking, cycling, sports, etc.
#NATMEET: Natural environments are a good place to meet others from the local community.


#sum the three nature appreciate variables (after z score standardizing them). A lower value means greater appreciation for natural spaces

GSS2018$nature_appreciate<-scale(GSS2018$natactive) + scale(GSS2018$natmeet) + scale(GSS2018$natrelax)

# wrkstat

# 1= working full time
# 2 = working part time 
# 3 = With a job, but not at work because of illness/vacation/strike
# 4 = unemployed
#5 = retired
#6 = in school
#7 = keeping house
#8 = other


#  GSS2018$workcat<-ifelse(GSS2018$wrkstat %in% c(1,2,6), "working for pay or in school",
#                       ifelse(GSS2018$wrkstat %in% c(3,4,5,7,8), "not working", NA))    

# New recode, as of 2/7/2024

GSS2018$workcat<-ifelse(GSS2018$wrkstat %in% c(1,2), "working for pay",
                        ifelse(GSS2018$wrkstat %in% c(3, 4,5), "Unemployed or retired",
                               ifelse(GSS2018$wrkstat %in% c(6), "In school",  
                                      ifelse(GSS2018$wrkstat %in% c(7), "Keeping house",   "Other"))))


#neisafe : Now thinking about safety in this neighborhood, how safe do you think it is?

#1=very safe
#2=somewhat safe
#3=somewhat unsafe
#4=very unsafe

GSS2018$safecat<-ifelse(GSS2018$neisafe %in% c(1,2), 0, #0=safe, 1=not safe
                        ifelse(GSS2018$neisafe %in% c(3,4), 1,NA))

#reverse code for interaction terms

GSS2018$safecatr<-ifelse(GSS2018$neisafe %in% c(1,2), 1,
                         ifelse(GSS2018$neisafe %in% c(3,4), 0,NA))


GSS2018$disc_miss<-ifelse(is.na(GSS2018$disc)==T, 1,0) #21 missing from ballot 3


GSS2018$age<-as.numeric(GSS2018$age)

#restrict to observations that are not missing the discrimination variable

GSS2018<-GSS2018[which(GSS2018$disc_miss==0),]

########################################################

ballot3<-GSS2018[which(GSS2018$ballot==3),]


#FOUR LEVEL CATEGORICAL VAR, FOR LINEARITY ASSESSMENT

vquint = quantile(ballot3$disc, c(0:4/4), na.rm=T)


# classify values
GSS2018$disc_4cat= cut(GSS2018$disc, 
                       vquint, 
                       include.lowest = T, 
                       labels = c("0 to 25% (LOWEST DISC) ", "25% to 50% " , "50% to 75% ", "75% + (Highest DISC)"))

b<-GSS2018[c("vpsu", "vstrat", "wtssall", "nattimeok_yn", "natsat_yn",
             "nattime_yn",   "nataccess_yn", 
             "genderid", 
             "age", "income16","income_cat",
             "educ", "educ2cat", "ballot","region_cat", "region",
             "nhwhite2cat", 
             "nhwhite2catr", "nature_appreciate",
             "workcat",  
             "income_cat", "natsat_s", "nattimeok_s", "safecat", "safecatr", 
             "raclive", "disc_miss", "disc_4cat",
             'wrkstat', 'genderidr', 'genderidr2', "race_self3cat", "wrkstat", "racecen1",
             "disc", "disc_notscaled", "disc_c", "disc_4cat")]

