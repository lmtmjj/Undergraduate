#2014-18 HSE

library("haven")
hse<-read_dta("Bsc_Ecig_HSE2014-18_8Feb21.dta")
hse_old<-read_dta("Bsc_Ecig_HSE2014-2018.dta")

#adding EC outcome variables 
hse$ecigev14t18<-hse_old$ecigev14t18
hse$ecign14t18<-hse_old$ecign14t18

hse$cigsta_adjust<-ifelse((hse$cigsta3 == 1), "Current Smoker", ifelse((hse$cigsta3 == 2) & (hse$endsmokg14t18 == 1), "Ex-regular smoker (Quit within the past five year)", ifelse((hse$cigsta3 == 3), "Never smoker", NA)))


head(hse$ecign14t18) #current e-cig users
head(hse$ecigev14t18) #never/ever used e-cig
head(hse$cigsta3)
hse$genhelf2 #self-rated health
hse$topqual4 #12 education/ may wanna focus on student
hse$endsmoke #quit in the past 5 years
hse$qimd
hse$alclimit07b #drinking 



library(sjPlot)
library(tidyverse)
library(survey)

#create joined urban variables from 2014 to 2018
#make 2014 one binary
hse$urban14b_2014<-ifelse(hse$urban14==1,1,2)
hse$urban<-coalesce(hse$urban14b_2014, hse$urban14b, hse$urban14br)


design<-svydesign(id = ~psu_a, strata=~cluster_a, nest=FALSE, data = hse, weights =~wt_int)
options(survey.lonely.psu = "adjust")

#combined them and write them 
EvN_ec_xtab<-rbind(svytable(~sex+ecigev14t18,design=design),
                   svytable(~ag16g10+ecigev14t18,design=design),
                   svytable(~cigsta_adjust+ecigev14t18,design=design),
                   svytable(~topqual4+ecigev14t18,design=design),
                   svytable(~qimd+ecigev14t18,design=design),
                   svytable(~alclimit07b+ecigev14t18,design=design),
                   svytable(~genhelf2+ecigev14t18,design=design),
                   svytable(~urban+ecigev14t18,design=design),
                   svytable(~gor1+ecigev14t18,design=design),
                   svytable(~year+ecigev14t18,design=design))

EvN_ec_chisq<-rbind(svychisq(~sex+ecigev14t18,design=design),
                    svychisq(~ag16g10+ecigev14t18,design=design),
                    svychisq(~cigsta_adjust+ecigev14t18,design=design),
                    svychisq(~topqual4+ecigev14t18,design=design),
                    svychisq(~qimd+ecigev14t18,design=design),
                    svychisq(~alclimit07b+ecigev14t18,design=design),
                    svychisq(~genhelf2+ecigev14t18,design=design),
                    svychisq(~urban+ecigev14t18,design=design),
                    svychisq(~gor1+ecigev14t18,design=design),
                    svychisq(~year+ecigev14t18,design=design))

write.csv(as.data.frame(EvN_ec_xtab), "Evn.csv")
round(EvN_ec_chisq[,3], 2)


CurvNo_ec_xtab<-rbind(svytable(~sex+ecign14t18,design=design),
                   svytable(~ag16g10+ecign14t18,design=design),
                   svytable(~cigsta_adjust+ecign14t18,design=design),
                   svytable(~topqual4+ecign14t18,design=design),
                   svytable(~qimd+ecign14t18,design=design),
                   svytable(~alclimit07b+ecign14t18,design=design),
                   svytable(~genhelf2+ecign14t18,design=design),
                   svytable(~urban+ecign14t18,design=design),
                   svytable(~gor1+ecign14t18,design=design),
                   svytable(~year+ecign14t18,design=design))
CurvNo_ec_chisq<-rbind(svychisq(~sex+ecign14t18,design=design),
                    svychisq(~ag16g10+ecign14t18,design=design),
                    svychisq(~cigsta_adjust+ecign14t18,design=design),
                    svychisq(~topqual4+ecign14t18,design=design),
                    svychisq(~qimd+ecign14t18,design=design),
                    svychisq(~alclimit07b+ecign14t18,design=design),
                    svychisq(~genhelf2+ecign14t18,design=design),
                    svychisq(~urban+ecign14t18,design=design),
                    svychisq(~gor1+ecign14t18,design=design),
                    svychisq(~year+ecign14t18,design=design))

write.csv(as.data.frame(CurvNo_ec_xtab), "CurvN.csv")

#Table 2 Current EC users by Smoking Status
hse_EConly<-subset(hse, ecign14t18==1)
design_EConly<-svydesign(id = ~psu_a, strata=~cluster_a, nest=FALSE,data = hse_EConly, weights = ~wt_int)
t2_xtab<-rbind(svytable(~sex+cigsta_adjust,design=design_EConly),
               svytable(~ag16g10+cigsta_adjust,design=design_EConly),
               svytable(~topqual4+cigsta_adjust,design=design_EConly),
               svytable(~qimd+cigsta_adjust,design=design_EConly),
               svytable(~alclimit07b+cigsta_adjust,design=design_EConly),
               svytable(~genhelf2+cigsta_adjust,design=design_EConly),
               svytable(~urban+cigsta_adjust,design=design_EConly),
               svytable(~gor1+cigsta_adjust,design=design_EConly),
               svytable(~year+cigsta_adjust,design=design_EConly))
t2_chisq<-rbind(svychisq(~sex+cigsta_adjust,design=design_EConly),
                svychisq(~ag16g10+cigsta_adjust,design=design_EConly),
                svychisq(~topqual4+cigsta_adjust,design=design_EConly),
                svychisq(~qimd+cigsta_adjust,design=design_EConly),
                svychisq(~alclimit07b+cigsta_adjust,design=design_EConly),
                svychisq(~genhelf2+cigsta_adjust,design=design_EConly),
                svychisq(~urban+cigsta_adjust,design=design_EConly),
                svychisq(~gor1+cigsta_adjust,design=design_EConly),
                svychisq(~year+cigsta_adjust,design=design_EConly))

write.csv(as.data.frame(t2_xtab), "Table 2.csv")

#contingency tables 
tab_xtab(var.row =hse$sex, var.col = hse$ecigev14t18)
tab_xtab(var.row =hse$ag16g10, var.col = hse$ecigev14t18)
tab_xtab(var.row =hse$cigsta3, var.col = hse$ecigev14t18)
tab_xtab(var.row =hse$genhelf2, var.col = hse$ecign14t18)
tab_xtab(var.row =hse$topqual4, var.col = hse$ecigev14t18)
tab_xtab(var.row =hse$qimd , var.col = hse$ecigev14t18)
tab_xtab(var.row =hse$alclimit07b, var.col = hse$ecigev14t18)

tab_xtab(var.row =hse_old$gor1, var.col = hse_old$ecigev14t18)
tab_xtab(var.row =hse$urban, var.col = hse$ecigev14t18)





# Three models for analysis 
Smoke_now<-subset(hse, cigsta_adjust=="Current Smoker") #current regular smoker
d1<-svydesign(id = ~psu_a, strata=~cluster_a, nest=FALSE, data = Smoke_now, weights = ~wt_int)

Smoke_ex<-subset(hse, cigsta_adjust=="Ex-regular smoker (Quit within the past five year)") #ex-regular 
d2<-svydesign(id = ~psu_a, strata=~cluster_a, nest=FALSE, data = Smoke_ex, weights = ~wt_int)

Smoke_never<-subset(hse, cigsta_adjust=="Never smoker")#never regular
d3<-svydesign(id = ~psu_a, strata=~cluster_a, nest=FALSE, data = Smoke_never, weights = ~wt_int)

#LOG REGRESSION:

#whole sample adjusted for smoking status
#model 1
m_whole<-svyglm(ecign14t18~cigsta_adjust+as.factor(sex)+as.factor(ag16g10)+as.factor(topqual4)+as.factor(qimd)+as.factor(alclimit07b)+as.factor(genhelf2)+urban+relevel(as.factor(gor1),ref = 5)+as.factor(year), design = design, family = "binomial"(link = "logit"))
tab_model(m_whole,pred.labels = c("Intercept",  "Ex-regular smoker (ref:Current Smoker)","Never-regular smoker","Male","age 25-34 (ref:age 16-24)", "age 35-44", "age 45-54", "age 55-64", "age 65-74", "age 75+", "Below degree (ref: higher education)", "No qualification (ref: higher education)",
                                  "IMD: Quintle 2", "IMD: Quintle 3", "IMD: Quintle 4", "IMD: Quintle 5",
                                  " Alchohol: <=4 units/day (men), <=3 (women)", "Alchohol:  >4 and <= 8 (men) , >3 and less than or equal to 6 (women) (ref:None)", "Alchohol:  Greater than 8 units (men), greater than 6 units (women)", 
                                  "Self-reported general health: Fair (ref:Very good/good)", 
                                  "Self-reported general health: Very bad/bad (ref:Very good/good)", "Urban", "North East (ref: West Midlands)", "North West", "Yorkshire and The Humber",
                                  "East Midlands", "East of England", "London", "South East", "South West", "2015 (ref: 2014)", "2016", "2017", "2018"), dv.labels = "EC dummy variables; 1=EC user, 0=otherwise", title = "Whole sample regression adjusted for smoking status")
#Model 2: current smokers and current EC
m2<-svyglm(ecign14t18~as.factor(sex)+as.factor(ag16g10)+as.factor(topqual4)+as.factor(qimd)+as.factor(alclimit07b)+as.factor(genhelf2)+urban+relevel(as.factor(gor1), ref = 5)+as.factor(year), design= d1, family = "binomial"(link = "logit"))
tab_model(m1,pred.labels = c("Intercept", "Male","age 25-34 (ref:age 16-24)", "age 35-44", "age 45-54", "age 55-64", "age 65-74", "age 75+", "Below degree (ref: higher education)", "No qualification (ref: higher education)",
                             "IMD: Quintle 2", "IMD: Quintle 3", "IMD: Quintle 4", "IMD: Quintle 5",
                             " Alchohol: <=4 units/day (men), <=3 (women)", "Alchohol:  >4 and <= 8 (men) , >3 and less than or equal to 6 (women) (ref:None)", "Alchohol:  Greater than 8 units (men), greater than 6 units (women)", 
                              "Self-reported general health: Fair (ref:Very good/good)", 
                             "Self-reported general health: Very bad/bad (ref:Very good/good)", "Urban", "North East (ref: West Midlands)", "North West", "Yorkshire and The Humber",
                             "East Midlands", "East of England", "London", "South East", "South West", "2015 (ref: 2014)", "2016", "2017", "2018"), dv.labels = "EC dummy variables; 1=EC user, 0=otherise", title = "Current Smokers") 
#regular smoker: at least once a day

#Model 3 ex-smokers and current EC
m3<-svyglm(ecign14t18~as.factor(sex)+as.factor(ag16g10)+as.factor(topqual4)+as.factor(qimd)+as.factor(alclimit07b)+as.factor(genhelf2)+urban+relevel(as.factor(gor1),ref = 5)+as.factor(year), design = d2, family = "binomial"(link = "logit"))
tab_model(m2, pred.labels = c("Intercept", "Male","age 25-34 (ref:age 16-24)", "age 35-44", "age 45-54", "age 55-64", "age 65-74", "age 75+", "Below degree (ref: higher education)", "No qualification (ref: higher education)",
                              "IMD: Quintle 2", "IMD: Quintle 3", "IMD: Quintle 4", "IMD: Quintle 5",
                              " Alchohol: <=4 units/day (men), <=3 (women)", "Alchohol:  >4 and <= 8 (men) , >3 and less than or equal to 6 (women) (ref:None)", "Alchohol:  Greater than 8 units (men), greater than 6 units (women)", 
                              "Self-reported general health: Fair (ref:Very good/good)", 
                              "Self-reported general health: Very bad/bad (ref:Very good/good)", "Urban", "North East (ref: West Midlands)", "North West", "Yorkshire and The Humber",
                              "East Midlands", "East of England", "London", "South East", "South West", "2015 (ref: 2014)", "2016", "2017", "2018"), dv.labels = "EC dummy variables; 1=EC user, 0=otherise", title = "Ex-regular Smokers (quit within the past five years)") 
#Gor1 ref: West midlands
#Model 4 never smokers and ever EC
m4<-svyglm(ecigev14t18~as.factor(sex)+as.factor(ag16g10)+as.factor(topqual4)+as.factor(qimd)+as.factor(alclimit07b)+as.factor(genhelf2)+urban+relevel(as.factor(gor1),ref = 5)+as.factor(year), design= d3, family = "binomial"(link = "logit"))
tab_model(m3,pred.labels = c("Intercept", "Male","age 25-34 (ref:age 16-24)", "age 35-44", "age 45-54", "age 55-64", "age 65-74", "age 75+", "Below degree (ref: higher education)", "No qualification (ref: higher education)",
                             "IMD: Quintle 2", "IMD: Quintle 3", "IMD: Quintle 4", "IMD: Quintle 5",
                             " Alchohol: <=4 units/day (men), <=3 (women)", "Alchohol:  >4 and <= 8 (men) , >3 and less than or equal to 6 (women) (ref:None)", "Alchohol:  Greater than 8 units (men), greater than 6 units (women)", 
                             "Self-reported general health: Fair (ref:Very good/good)", 
                             "Self-reported general health: Very bad/bad (ref:Very good/good)", "Urban", "North East (ref: West Midlands)", "North West", "Yorkshire and The Humber",
                             "East Midlands", "East of England", "London", "South East", "South West", "2015 (ref: 2014)", "2016", "2017", "2018"), dv.labels = "EC dummy variables; 1=Ever used ECs, 0=Never used ECs", title = "Never-regular Smokers")


#appendix for this one 
m3_current<-svyglm(ecign14t18~as.factor(sex)+as.factor(ag16g10)+as.factor(topqual4)+as.factor(qimd)+as.factor(alclimit07b)+as.factor(genhelf2)+urban+relevel(as.factor(gor1),ref = 5)+as.factor(year), design= d3, family = "binomial"(link = "logit"))
tab_model(m3,pred.labels = c("Intercept", "Male","age 25-34 (ref:age 16-24)", "age 35-44", "age 45-54", "age 55-64", "age 65-74", "age 75+", "Below degree (ref: higher education)", "No qualification (ref: higher education)",
                             "IMD: Quintle 2", "IMD: Quintle 3", "IMD: Quintle 4", "IMD: Quintle 5",
                             " Alchohol: <=4 units/day (men), <=3 (women)", "Alchohol:  >4 and <= 8 (men) , >3 and less than or equal to 6 (women) (ref:None)", "Alchohol:  Greater than 8 units (men), greater than 6 units (women)", 
                             "Self-reported general health: Fair (ref:Very good/good)", 
                             "Self-reported general health: Very bad/bad (ref:Very good/good)", "Urban", "North East (ref: West Midlands)", "North West", "Yorkshire and The Humber",
                             "East Midlands", "East of England", "London", "South East", "South West", "2015 (ref: 2014)", "2016", "2017", "2018"), dv.labels = "EC dummy variables; 1=EC user, 0=otherise", title = "Never-regular Smokers")



