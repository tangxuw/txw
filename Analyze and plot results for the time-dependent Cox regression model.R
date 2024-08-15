library(rms)
library(survminer)
library(ggplot2)
library(ggsci)
library(survival)
#Adjust the units for air pollutants.#
data2$pm25_10<-data2$pm25/10
data2$av3pm25_10<-data2$av3pm25/10
data2$av5pm25_10<-data2$av5pm25/10
hist(data2$pm10)
data2$pm10_10<-data2$pm10/10
data2$av3pm10_10<-data2$av3pm10/10
data2$av5pm10_10<-data2$av5pm10/10
hist(data2$nox)
data2$nox_10<-data2$nox/10
data2$av3nox_10<-data2$av3nox/10
data2$av5nox_10<-data2$av5nox/10
hist(data2$no2)
data2$no2_10<-data2$no2/10
data2$av3no2_10<-data2$av3no2/10
data2$av5no2_10<-data2$av5no2/10
hist(data2$av3so2)
hist(data2$bz)
data2$bz_01<-data2$bz/0.1
data2$av3bz_01<-data2$av3bz/0.1
data2$av5bz_01<-data2$av5bz/0.1
data22 <- data2[data2$locationchange <= 10000, ]
objects <- ls()  
objects_to_remove <- setdiff(objects, "data22")  
rm(list = objects_to_remove)
###total model######################################################
#pm25#
pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ pm25group, data = data22)
pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ pm25_10, data = data22)
av3pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3pm25group, data = data22)
av3pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3pm25_10, data = data22)
av5pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5pm25group, data = data22)
av5pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5pm25_10, data = data22)
summary(pm25)
summary(pm25_10)
summary(av3pm25)
summary(av3pm25_10)
summary(av5pm25)
summary(av5pm25_10)
car::vif(pm25)
car::vif(pm25_10)
car::vif(av3pm25)
car::vif(av3pm25_10)
car::vif(av5pm25)
car::vif(av5pm25_10)
cox.zph(pm25)
cox.zph(pm25_10)
cox.zph(av3pm25)
cox.zph(av3pm25_10)
cox.zph(av5pm25)
cox.zph(av5pm25_10)
#pm10#
pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ pm10group, data = data22)
pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ pm10_10, data = data22)
av3pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av3pm10group, data = data22)
av3pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3pm10_10, data = data22)
av5pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5pm10group, data = data22)
av5pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5pm10_10, data = data22)
summary(pm10)
summary(pm10_10)
summary(av3pm10)
summary(av3pm10_10)
summary(av5pm10)
summary(av5pm10_10)
car::vif(pm10)
car::vif(pm10_10)
car::vif(av3pm10)
car::vif(av3pm10_10)
car::vif(av5pm10)
car::vif(av5pm10_10)
cox.zph(pm10)
cox.zph(pm10_10)
cox.zph(av3pm10)
cox.zph(av3pm10_10)
cox.zph(av5pm10)
cox.zph(av5pm10_10)
#no2#
no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ no2group, data = data22)
no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ no2_10, data = data22)
av3no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av3no2group, data = data22)
av3no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3no2_10, data = data22)
av5no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5no2group, data = data22)
av5no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5no2_10, data = data22)
summary(no2)
summary(no2_10)
summary(av3no2)
summary(av3no2_10)
summary(av5no2)
summary(av5no2_10)
car::vif(no2)
car::vif(no2_10)
car::vif(av3no2)
car::vif(av3no2_10)
car::vif(av5no2)
car::vif(av5no2_10)
cox.zph(no2)
cox.zph(no2_10)
cox.zph(av3no2)
cox.zph(av3no2_10)
cox.zph(av5no2)
cox.zph(av5no2_10)
#进一步调整pm25#
av5no2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2group, data = data22)
av5no2_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2_10, data = data22)
summary(av5no2_pm25)
summary(av5no2_pm25_10)
car::vif(av5no2_pm25)
car::vif(av5no2_pm25_10)
cox.zph(av5no2_pm25)
cox.zph(av5no2_pm25_10)
#nox#
nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ noxgroup, data = data22)
nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ nox_10, data = data22)
av3nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3noxgroup, data = data22)
av3nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3nox_10, data = data22)
av5nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5noxgroup, data = data22)
av5nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5nox_10, data = data22)
summary(nox)
summary(nox_10)
summary(av3nox)
summary(av3nox_10)
summary(av5nox)
summary(av5nox_10)
car::vif(nox)
car::vif(nox_10)
car::vif(av3nox)
car::vif(av3nox_10)
car::vif(av5nox)
car::vif(av5nox_10)
cox.zph(nox)
cox.zph(nox_10)
cox.zph(av3nox)
cox.zph(av3nox_10)
cox.zph(av5nox)
cox.zph(av5nox_10)
#进一步调整pm25#
av5nox_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5pm25group+av5noxgroup, data = data22)
av5nox_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                      +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                      +incomegroup +gastricreflux+METgroup+ av5pm25group+av5nox_10, data = data22)
summary(av5nox_pm25)
summary(av5nox_pm25_10)
car::vif(av5nox_pm25)
car::vif(av5nox_pm25_10)
cox.zph(av5nox_pm25)
cox.zph(av5nox_pm25_10)

#so2#
so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ so2group, data = data22)
so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ so2, data = data22)
av3so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3so2group, data = data22)
av3so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3so2, data = data22)
av5so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5so2group, data = data22)
av5so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5so2, data = data22)
summary(so2)
summary(so2_1)
summary(av3so2)
summary(av3so2_1)
summary(av5so2)
summary(av5so2_1)
car::vif(so2)
car::vif(so2_1)
car::vif(av3so2)
car::vif(av3so2_1)
car::vif(av5so2)
car::vif(av5so2_1)
cox.zph(so2)
cox.zph(so2_1)
cox.zph(av3so2)
cox.zph(av3so2_1)
cox.zph(av5so2)
cox.zph(av5so2_1)
#so2进一步调整pm25#
av5so2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2group, data = data22)
av5so2_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                      +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                      +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2, data = data22)
summary(av5so2_pm25)
summary(av5so2_pm25_1)
car::vif(av5so2_pm25)
car::vif(av5so2_pm25_10_1)
cox.zph(av5so2_pm25)
cox.zph(av5so2_pm25_10_1)
#bz#
bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ bzgroup, data = data22)
bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ bz_01, data = data22)
av3bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3bzgroup, data = data22)
av3bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3bz_01, data = data22)
av5bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5bzgroup, data = data22)
av5bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5bz_01, data = data22)
summary(bz)
summary(bz_01)
summary(av3bz)
summary(av3bz_01)
summary(av5bz)
summary(av5bz_01)
car::vif(bz)
car::vif(bz_01)
car::vif(av3bz)
car::vif(av3bz_01)
car::vif(av5bz)
car::vif(av5bz_01)
cox.zph(bz)
cox.zph(bz_01)
cox.zph(av3bz)
cox.zph(av3bz_01)
cox.zph(av5bz)
cox.zph(av5bz_01)
##
av5bz_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bzgroup, data = data22)
av5bz_pm25_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                      +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                      +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bz_01, data = data22)
summary(av5bz_pm25)
summary(av5bz_pm25_01)
car::vif(av5bz_pm25)
car::vif(av5bz_pm25_10)
cox.zph(av5bz_pm25)
cox.zph(av5bz_pm25_10)

###Stratified analysis of squamous cell carcinoma###########
data3 <- data22[!(data22$histology %in% c(1, 2)), ]
table(data3$ec)
scpm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ pm25group, data = data3)
scpm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ pm25_10, data = data3)
scav3pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av3pm25group, data = data3)
scav3pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3pm25_10, data = data3)
scav5pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5pm25group, data = data3)
scav5pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5pm25_10, data = data3)
summary(scpm25)
summary(scpm25_10)
summary(scav3pm25)
summary(scav3pm25_10)
summary(scav5pm25)
summary(scav5pm25_10)
car::vif(scpm25)
car::vif(scpm25_10)
car::vif(scav3pm25)
car::vif(scav3pm25_10)
car::vif(scav5pm25)
car::vif(scav5pm25_10)
cox.zph(scpm25)
cox.zph(scpm25_10)
cox.zph(scav3pm25)
cox.zph(scav3pm25_10)
cox.zph(scav5pm25)
cox.zph(scav5pm25_10)
#pm10#
scpm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ pm10group, data = data3)
scpm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ pm10_10, data = data3)
scav3pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av3pm10group, data = data3)
scav3pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3pm10_10, data = data3)
scav5pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5pm10group, data = data3)
scav5pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5pm10_10, data = data3)
summary(scpm10)
summary(scpm10_10)
summary(scav3pm10)
summary(scav3pm10_10)
summary(scav5pm10)
summary(scav5pm10_10)
car::vif(scpm10)
car::vif(scpm10_10)
car::vif(scav3pm10)
car::vif(scav3pm10_10)
car::vif(scav5pm10)
car::vif(scav5pm10_10)
cox.zph(scpm10)
cox.zph(scpm10_10)
cox.zph(scav3pm10)
cox.zph(scav3pm10_10)
cox.zph(scav5pm10)
cox.zph(scav5pm10_10)
#no2#
scno2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ no2group, data = data3)
scno2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ no2_10, data = data3)
scav3no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3no2group, data = data3)
scav3no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3no2_10, data = data3)
scav5no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5no2group, data = data3)
scav5no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5no2_10, data = data3)
summary(scno2)
summary(scno2_10)
summary(scav3no2)
summary(scav3no2_10)
summary(scav5no2)
summary(scav5no2_10)
car::vif(scno2)
car::vif(scno2_10)
car::vif(scav3no2)
car::vif(scav3no2_10)
car::vif(scav5no2)
car::vif(scav5no2_10)
cox.zph(scno2)
cox.zph(scno2_10)
cox.zph(scav3no2)
cox.zph(scav3no2_10)
cox.zph(scav5no2)
cox.zph(scav5no2_10)
#进一步调整pm25#
scav5no2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2group, data = data3)
scav5no2_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                        +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                        +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2_10, data = data3)
summary(scav5no2_pm25)
summary(scav5no2_pm25_10)
car::vif(scav5no2_pm25)
car::vif(scav5no2_pm25_10)
cox.zph(scav5no2_pm25)
cox.zph(scav5no2_pm25_10)

#nox#
scnox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ noxgroup, data = data3)
scnox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ nox_10, data = data3)
scav3nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3noxgroup, data = data3)
scav3nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3nox_10, data = data3)
scav5nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5noxgroup, data = data3)
scav5nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5nox_10, data = data3)
summary(scnox)
summary(scnox_10)
summary(scav3nox)
summary(scav3nox_10)
summary(scav5nox)
summary(scav5nox_10)
car::vif(scnox)
car::vif(scnox_10)
car::vif(scav3nox)
car::vif(scav3nox_10)
car::vif(scav5nox)
car::vif(scav5nox_10)
cox.zph(scnox)
cox.zph(scnox_10)
cox.zph(scav3nox)
cox.zph(scav3nox_10)
cox.zph(scav5nox)
cox.zph(scav5nox_10)
#进一步调整pm25#
scav5nox_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5noxgroup, data = data3)
scav5nox_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                        +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                        +incomegroup +gastricreflux+METgroup+ av5pm25group+av5nox_10, data = data3)
summary(scav5nox_pm25)
summary(scav5nox_pm25_10)
car::vif(scav5nox_pm25)
car::vif(scav5nox_pm25_10)
cox.zph(scav5nox_pm25)
cox.zph(scav5nox_pm25_10)


#so2#
scso2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
           +incomegroup +gastricreflux+METgroup+ so2group, data = data3)
scso2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ so2, data = data3)
scav3so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av3so2group, data = data3)
scav3so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3so2, data = data3)
scav5so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ av5so2group, data = data3)
scav5so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5so2, data = data3)
summary(scso2)
summary(scso2_1)
summary(scav3so2)
summary(scav3so2_1)
summary(scav5so2)
summary(scav5so2_1)
car::vif(scso2)
car::vif(scso2_1)
car::vif(scav3so2)
car::vif(scav3so2_1)
car::vif(scav5so2)
car::vif(scav5so2_1)
cox.zph(scso2)
cox.zph(scso2_1)
cox.zph(scav3so2)
cox.zph(scav3so2_1)
cox.zph(scav5so2)
cox.zph(scav5so2_1)
#进一步调整pm25#
scav5so2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2group, data = data3)
scav5so2_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                       +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2, data = data3)
summary(scav5so2_pm25)
summary(scav5so2_pm25_1)
car::vif(scav5so2_pm25)
car::vif(scav5so2_pm25_1)
cox.zph(scav5so2_pm25)
cox.zph(scav5so2_pm25_1)
#bz#
scbz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
          +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
          +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
          +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
          +incomegroup +gastricreflux+METgroup+ bzgroup, data = data3)
scbz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ bz_01, data = data3)
scav3bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ av3bzgroup, data = data3)
scav3bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3bz_01, data = data3)
scav5bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ av5bzgroup, data = data3)
scav5bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5bz_01, data = data3)
summary(scbz)
summary(scbz_01)
summary(scav3bz)
summary(scav3bz_01)
summary(scav5bz)
summary(scav5bz_01)
car::vif(scbz)
car::vif(scbz_01)
car::vif(scav3bz)
car::vif(scav3bz_01)
car::vif(scav5bz)
car::vif(scav5bz_01)
cox.zph(scbz)
cox.zph(scbz_01)
cox.zph(scav3bz)
cox.zph(scav3bz_01)
cox.zph(scav5bz)
cox.zph(scav5bz_01)
#进一步调整pm25#
scav5bz_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                    +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                    +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                    +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                    +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bzgroup, data = data3)
scav5bz_pm25_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                       +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bz_01, data = data3)
summary(scav5bz_pm25)
summary(scav5bz_pm25_01)
car::vif(scav5bz_pm25)
car::vif(scav5bz_pm25_01)
cox.zph(scav5bz_pm25)
cox.zph(scav5bz_pm25_01)


##Stratified analysis of adenocarcinoma##
data4 <- data22[!(data22$histology %in% c(0, 2)), ]
hist(data4$bz_01)
table(data4$ec)
acpm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ pm25group, data = data4)
acpm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ pm25_10, data = data4)
acav3pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3pm25group, data = data4)
acav3pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                    +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                    +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                    +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                    +incomegroup +gastricreflux+METgroup+ av3pm25_10, data = data4)
acav5pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5pm25group, data = data4)
acav5pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                    +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                    +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                    +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                    +incomegroup +gastricreflux+METgroup+ av5pm25_10, data = data4)
summary(acpm25)
summary(acpm25_10)
summary(acav3pm25)
summary(acav3pm25_10)
summary(acav5pm25)
summary(acav5pm25_10)
car::vif(acpm25)
car::vif(acpm25_10)
car::vif(acav3pm25)
car::vif(acav3pm25_10)
car::vif(acav5pm25)
car::vif(acav5pm25_10)
cox.zph(acpm25)
cox.zph(acpm25_10)
cox.zph(acav3pm25)
cox.zph(acav3pm25_10)
cox.zph(acav5pm25)
cox.zph(acav5pm25_10)
#pm10#
acpm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
              +incomegroup +gastricreflux+METgroup+ pm10group, data = data4)
acpm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ pm10_10, data = data4)
acav3pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av3pm10group, data = data4)
acav3pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                    +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                    +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                    +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                    +incomegroup +gastricreflux+METgroup+ av3pm10_10, data = data4)
acav5pm10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+ av5pm10group, data = data4)
acav5pm10_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                    +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                    +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                    +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                    +incomegroup +gastricreflux+METgroup+ av5pm10_10, data = data4)
summary(acpm10)
summary(acpm10_10)
summary(acav3pm10)
summary(acav3pm10_10)
summary(acav5pm10)
summary(acav5pm10_10)
car::vif(acpm10)
car::vif(acpm10_10)
car::vif(acav3pm10)
car::vif(acav3pm10_10)
car::vif(acav5pm10)
car::vif(acav5pm10_10)
cox.zph(acpm10)
cox.zph(acpm10_10)
cox.zph(acav3pm10)
cox.zph(acav3pm10_10)
cox.zph(acav5pm10)
cox.zph(acav5pm10_10)
#no2#
acno2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ no2group, data = data4)
acno2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ no2_10, data = data4)
acav3no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3no2group, data = data4)
acav3no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av3no2_10, data = data4)
acav5no2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5no2group, data = data4)
acav5no2_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5no2_10, data = data4)
summary(acno2)
summary(acno2_10)
summary(acav3no2)
summary(acav3no2_10)
summary(acav5no2)
summary(acav5no2_10)
car::vif(acno2)
car::vif(acno2_10)
car::vif(acav3no2)
car::vif(acav3no2_10)
car::vif(acav5no2)
car::vif(acav5no2_10)
cox.zph(acno2)
cox.zph(acno2_10)
cox.zph(acav3no2)
cox.zph(acav3no2_10)
cox.zph(acav5no2)
cox.zph(acav5no2_10)
#ac no2 进一步调整pm25#
acav5no2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2group, data = data4)
acav5no2_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                      +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                      +incomegroup +gastricreflux+METgroup+ av5pm25group+av5no2_10, data = data4)
summary(acav5no2_pm25)
summary(acav5no2_pm25_10)
car::vif(acav5no2_pm25)
car::vif(acav5no2_pm25_10)
cox.zph(acav5no2_pm25)
cox.zph(acav5no2_pm25_10)
#nox#
acnox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ noxgroup, data = data4)
acnox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ nox_10, data = data4)
acav3nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3noxgroup, data = data4)
acav3nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av3nox_10, data = data4)
acav5nox<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5noxgroup, data = data4)
acav5nox_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                   +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                   +incomegroup +gastricreflux+METgroup+ av5nox_10, data = data4)
summary(acnox)
summary(acnox_10)
summary(acav3nox)
summary(acav3nox_10)
summary(acav5nox)
summary(acav5nox_10)
car::vif(acnox)
car::vif(acnox_10)
car::vif(acav3nox)
car::vif(acav3nox_10)
car::vif(acav5nox)
car::vif(acav5nox_10)
cox.zph(acnox)
cox.zph(acnox_10)
cox.zph(acav3nox)
cox.zph(acav3nox_10)
cox.zph(acav5nox)
cox.zph(acav5nox_10)
#ac nox 进一步调整pm25#
acav5nox_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5noxgroup, data = data4)
acav5nox_pm25_10<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                        +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                        +incomegroup +gastricreflux+METgroup+ av5pm25group+av5nox_10, data = data4)
summary(acav5nox_pm25)
summary(acav5nox_pm25_10)
car::vif(acav5nox_pm25)
car::vif(acav5nox_pm25_10)
cox.zph(acav5nox_pm25)
cox.zph(acav5nox_pm25_10)


#so2#
acso2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
             +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
             +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
             +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
             +incomegroup +gastricreflux+METgroup+ so2group, data = data4)
acso2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ so2, data = data4)
acav3so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av3so2group, data = data4)
acav3so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3so2, data = data4)
acav5so2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                +incomegroup +gastricreflux+METgroup+ av5so2group, data = data4)
acav5so2_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5so2, data = data4)
summary(acso2)
summary(acso2_1)
summary(acav3so2)
summary(acav3so2_1)
summary(acav5so2)
summary(acav5so2_1)
car::vif(acso2)
car::vif(acso2_1)
car::vif(acav3so2)
car::vif(acav3so2_1)
car::vif(acav5so2)
car::vif(acav5so2_1)
cox.zph(acso2)
cox.zph(acso2_1)
cox.zph(acav3so2)
cox.zph(acav3so2_1)
cox.zph(acav5so2)
cox.zph(acav5so2_1)
#ac so2 进一步调整 pm25#
acav5so2_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2group, data = data4)
acav5so2_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                        +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                        +incomegroup +gastricreflux+METgroup+ av5pm25group+av5so2, data = data4)
summary(acav5so2_pm25)
summary(acav5so2_pm25_1)
car::vif(acav5so2_pm25)
car::vif(acav5so2_pm25_1)
cox.zph(acav5so2_pm25)
cox.zph(acav5so2_pm25_1)

#bz#
acbz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
            +incomegroup +gastricreflux+METgroup+ bzgroup, data = data4)
acbz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ bz_01, data = data4)
acav3bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av3bzgroup, data = data4)
acav3bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av3bz_01, data = data4)
acav5bz<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
               +incomegroup +gastricreflux+METgroup+ av5bzgroup, data = data4)
acav5bz_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+ av5bz_01, data = data4)
summary(acbz)
summary(acbz_01)
summary(acav3bz)
summary(acav3bz_01)
summary(acav5bz)
summary(acav5bz_01)
car::vif(acbz)
car::vif(acbz_01)
car::vif(acav3bz)
car::vif(acav3bz_01)
car::vif(acav5bz)
car::vif(acav5bz_01)
cox.zph(acbz)
cox.zph(acbz_01)
cox.zph(acav3bz)
cox.zph(acav3bz_01)
cox.zph(acav5bz)
cox.zph(acav5bz_01)
#调整av5pm25#
acav5bz_pm25<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bzgroup, data = data4)
acav5bz_pm25_01<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                        +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                        +incomegroup +gastricreflux+METgroup+ av5pm25group+av5bz_01, data = data4)
summary(acav5bz_pm25)
summary(acav5bz_pm25_01)
car::vif(acav5bz_pm25)
car::vif(acav5bz_pm25_01)
cox.zph(acav5bz_pm25)
cox.zph(acav5bz_pm25_01)
##Restricted cubic splines.###############
#SO2#
data22 <- data2[complete.cases(data2$av5so2), ]
ddist <- datadist(data22)
options(datadist = "ddist")
fit_av5so2 <- cph(Surv(time, time2, ec) ~ rcs(av5so2, 3)+ agegroup + gender+educationgroup
                  +urbangroup+familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                  +alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup
                 +oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                 +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux
                 +METgroup,
                 x = TRUE, y = TRUE, data = data22)
anova(fit_av5so2)
summary(data22$av5so2)
ddist$limits$av5so2[2] <- 0.3155
fit_av5so2=update(fit_av5so2)
Pre_av5so2 <-rms::Predict(fit_av5so2,av5so2,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5so2_filtered <- Pre_av5so2[Pre_av5so2$av5so2 <= 10, ]
optimized_plot <- ggplot(Pre_av5so2_filtered, aes(x = av5so2, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average SO2 (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 2.5, y = 4, 
           label = "P for nonlinear = 0.886", size = 5)+
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
print(optimized_plot)



fit_av5so2_pm25 <- cph(Surv(time, time2, ec) ~ rcs(av5so2, 3)+ agegroup + gender+educationgroup
                  +urbangroup+familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                  +alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup
                  +oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                  +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux
                  +METgroup+av5pm25group,
                  x = TRUE, y = TRUE, data = data22)
anova(fit_av5so2_pm25)
summary(data22$av5so2)
ddist$limits$av5so2[2] <- 0.3155
fit_av5so2_pm25=update(fit_av5so2_pm25)
Pre_av5so2_pm25 <-rms::Predict(fit_av5so2_pm25,av5so2,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5so2_pm25filtered <- Pre_av5so2_pm25[Pre_av5so2_pm25$av5so2 <= 10, ]
optimized_plot <- ggplot(Pre_av5so2_pm25filtered, aes(x = av5so2, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average SO2 (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 2.5, y = 4, 
           label = "P for nonlinear = 0.706", size = 5)+
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1))
print(optimized_plot)

#NOx#
data22 <- data2[data2$locationchange <= 10000, ]
ddist <- datadist(data22)
options(datadist = "ddist")
fit_av5nox <- cph(Surv(time, time2, ec) ~ rcs(av5nox, 3)+ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup,
                  x = TRUE, y = TRUE, data = data22)
anova(fit_av5nox)
summary(data22$av5nox)
ddist$limits$av5nox[2] <- 3.957
fit_av5nox=update(fit_av5nox)
Pre_av5nox <-rms::Predict(fit_av5nox,av5nox,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5nox_filtered <- Pre_av5nox[Pre_av5nox$av5nox <= 50, ]
optimized_plot <- ggplot(Pre_av5nox_filtered, aes(x = av5nox, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average NOx (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 15, y = 1.75, 
           label = "P for nonlinear = 0.157", size = 5)+
  scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))
print(optimized_plot)


###
fit_av5nox_pm25 <- cph(Surv(time, time2, ec) ~ rcs(av5nox, 3)+ agegroup + gender+educationgroup
                       +urbangroup+familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                       +alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup
                       +oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                       +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux
                       +METgroup+av5pm25group,
                       x = TRUE, y = TRUE, data = data22)
anova(fit_av5nox_pm25)
summary(data22$av5nox)
ddist$limits$av5nox[2] <- 3.957
fit_av5nox_pm25=update(fit_av5nox_pm25)
Pre_av5nox_pm25 <-rms::Predict(fit_av5nox_pm25,av5nox,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5nox_pm25filtered <- Pre_av5nox_pm25[Pre_av5nox_pm25$av5nox <= 50, ]
optimized_plot <- ggplot(Pre_av5nox_pm25filtered, aes(x = av5nox, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average NOx (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 15, y = 3, 
           label = "P for nonlinear = 0.057", size = 5)+
  scale_y_continuous(limits = c(0, 7), breaks = seq(0, 7, by = 1))
print(optimized_plot)



#no2#
fit_av5no2 <- cph(Surv(time, time2, ec) ~ rcs(av5no2, 3)+ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup,
                  x = TRUE, y = TRUE, data = data22)
anova(fit_av5no2)
summary(data22$av5no2)
ddist$limits$av5no2[2] <- 3.153
fit_av5no2=update(fit_av5no2)
Pre_av5no2 <-rms::Predict(fit_av5no2,av5no2,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5no2_filtered <- Pre_av5no2[Pre_av5no2$av5no2 <= 40, ]
optimized_plot <- ggplot(Pre_av5no2_filtered, aes(x = av5no2, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average NO2 (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 15, y = 2, 
           label = "P for nonlinear = 0.201", size = 5)+
      scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, by = 0.5))
print(optimized_plot)



fit_av5no2_pm25 <- cph(Surv(time, time2, ec) ~ rcs(av5no2, 3)+ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup+av5pm25group,
                  x = TRUE, y = TRUE, data = data22)
anova(fit_av5no2_pm25)
summary(data22$av5no2)
ddist$limits$av5no2[2] <- 3.153
fit_av5no2_pm25=update(fit_av5no2_pm25)
Pre_av5no2 <-rms::Predict(fit_av5no2_pm25,av5no2,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5no2_filtered <- Pre_av5no2[Pre_av5no2$av5no2 <= 40, ]
optimized_plot <- ggplot(Pre_av5no2_filtered, aes(x = av5no2, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average NO2 (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 15, y = 3.5, 
           label = "P for nonlinear = 0.140", size = 5)+
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
print(optimized_plot)



##bz##

fit_av5bz <- cph(Surv(time, time2, ec) ~ rcs(av5bz, 3)+ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                  +incomegroup +gastricreflux+METgroup,
                  x = TRUE, y = TRUE, data = data22)
anova(fit_av5bz)
summary(data22$av5bz)
ddist$limits$av5bz[2] <- 0.09813
fit_av5bz=update(fit_av5bz)
Pre_av5bz <-rms::Predict(fit_av5bz,av5bz,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5bz_filtered <- Pre_av5bz[Pre_av5bz$av5bz <= 0.8, ]
optimized_plot <- ggplot(Pre_av5bz_filtered, aes(x = av5bz, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average benzene (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 0.3, y = 1.75, 
           label = "P for nonlinear = 0.560", size = 5)+
  scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, by = 0.5))
print(optimized_plot)


fit_av5bz_pm25 <- cph(Surv(time, time2, ec) ~ rcs(av5bz, 3)+ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                 +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                 +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                 +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                 +incomegroup +gastricreflux+METgroup+av5pm25group,
                 x = TRUE, y = TRUE, data = data22)
anova(fit_av5bz_pm25)
summary(data22$av5bz)
ddist$limits$av5bz[2] <- 0.09813
fit_av5bz_pm25=update(fit_av5bz_pm25)
Pre_av5bz <-rms::Predict(fit_av5bz_pm25,av5bz,fun=exp,type="predictions",ref.zero=TRUE,conf.int = 0.95,digits=1);
Pre_av5bz_filtered <- Pre_av5bz[Pre_av5bz$av5bz <= 0.8, ]
optimized_plot <- ggplot(Pre_av5bz_filtered, aes(x = av5bz, y = Predicted_HR)) +
  geom_line(aes(color = "Predicted Hazard Ratio"), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = "95% Confidence Interval"), alpha = 0.3) +
  xlab("5-year average benzene (μg/m3)") +
  ylab("Hazard Ratio") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 15),
    legend.position = "top",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 13),
    axis.title.x = element_text(size = 15),  # 调整 x 轴标题字体大小
    axis.title.y = element_text(size = 15),  # 调整 y 轴标题字体大小
    axis.line = element_line(colour = "black"),  # 将轴线颜色设置为黑色
    axis.ticks = element_line(),  # 设置坐标轴刻度线为实线
    axis.ticks.length = unit(0.2, "cm")  # 设置坐标轴刻度线的长度
  ) +
  scale_color_manual(values = "blue") +
  scale_fill_manual(values = "lightblue")+
  annotate("text", x = 0.3, y = 2.5, 
           label = "P for nonlinear = 0.372", size = 5)+
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
print(optimized_plot)


#######Stratified analysis of NO₂析################
#pm25#
av5pm25group_levels <- levels(data22$av5pm25group)
for (level in av5pm25group_levels) {
  subset_data <- data22[data22$av5pm25group == level, ]
  av5no2_10_av5pm25group <- coxph(Surv(time, time2, ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                                      +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                      +incomegroup +gastricreflux+METgroup+av5no2_10, data = subset_data)
  cat("av5pm25group Level:", level, "\n")
  print(summary(av5no2_10_av5pm25group))
}


av5no2_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                                +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                +incomegroup +gastricreflux+METgroup+av5no2_10+av5pm25group, data = data22)

av5no2_pm25_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5no2_10*av5pm25group, data = data22)
anova(av5no2_pm25_1,av5no2_pm25_2)
#age#
agegroup_levels <- levels(data22$agegroup)
for (level in agegroup_levels) {
  subset_data <- data22[data22$agegroup == level, ]
  av5no2_10_agegroup <- coxph(Surv(time, time2, ec) ~ gender+educationgroup+urbangroup+familyhistory+BMIgroup
                                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                  +incomegroup +gastricreflux+METgroup+av5no2_10+av5pm25group, data = subset_data)
  cat("agegroup Level:", level, "\n")
  print(summary(av5no2_10_agegroup))
}

av5no2_agegroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5no2_10+av5pm25group, data = data22)

av5no2_agegroup_2<-coxph(Surv(time,time2,ec) ~  gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5no2_10*agegroup+av5pm25group, data = data22)
anova(av5no2_agegroup_1,av5no2_agegroup_2)
#gender#
data22$gender <- as.factor(data22$gender)
gender_levels <- levels(data22$gender)
for (level in gender_levels) {
  subset_data <- data22[data22$gender == level, ]
  av5no2_10_gender <- coxph(Surv(time, time2, ec) ~ agegroup +educationgroup+urbangroup+familyhistory+BMIgroup
                                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                  +incomegroup +gastricreflux+METgroup+av5no2_10+av5pm25group, data = subset_data)
  cat("gender Level:", level, "\n")
  print(summary(av5no2_10_gender))
}



av5no2_gender_2<-coxph(Surv(time,time2,ec) ~ agegroup+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5pm25group+av5no2_10*gender, data = data22)
summary(av5no2_gender_2)
#smoke#
table(data22$smokingstatusgroup)
data222 <- data22[data22$smokingstatusgroup != 9, ]
data222$smokingstatusgroup <- factor(data222$smokingstatusgroup, levels = c(1, 2, 3))
smokingstatusgroup_levels <- levels(data222$smokingstatusgroup)
for (level in smokingstatusgroup_levels) {
  subset_data <- data222[data222$smokingstatusgroup == level, ]
  av5no2_10_smokingstatusgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                            +familyhistory+BMIgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                            +incomegroup +gastricreflux+METgroup+av5pm25group+av5no2_10, data = subset_data)
  cat("smokingstatusgroup Level:", level, "\n")
  print(summary(av5no2_10_smokingstatusgroup))
}

av5no2_smokingstatusgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                          +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                          +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                          +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                          +incomegroup +gastricreflux+METgroup+av5pm25group+av5no2_10+smokingstatusgroup
                          , data = data222)
av5no2_smokingstatusgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                       +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+av5pm25group+av5no2_10*smokingstatusgroup
                       , data = data222)
anova(av5no2_smokingstatusgroup_1,av5no2_smokingstatusgroup_2)
remove(data222)
###
data222 <- data22[data22$alcoholintakegroup2 != 9, ]
data222$alcoholintakegroup2 <- factor(data222$alcoholintakegroup2, levels = c(1, 2))
table(data222$alcoholintakegroup2)
alcoholintakegroup2_levels <- levels(data222$alcoholintakegroup2)
for (level in alcoholintakegroup2_levels) {
  subset_data <- data222[data222$alcoholintakegroup2 == level, ]
  av5no2_10_alcoholintakegroup2 <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                        +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+cookedvegetablegroup
                                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                        +incomegroup +gastricreflux+METgroup+av5pm25group+av5no2_10, data = subset_data)
  cat("alcoholintakegroup2 Level:", level, "\n")
  print(summary(av5no2_10_alcoholintakegroup2))
}


av5no2_alcoholintakegroup2_1<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5no2_10+alcoholintakegroup2, data = data222)




av5no2_alcoholintakegroup2_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5no2_10*alcoholintakegroup2, data = data222)
summary(av5no2_alcoholintakegroup2_2)
anova(av5no2_alcoholintakegroup2_1,av5no2_alcoholintakegroup2_2)
remove(data222)
#secondhandsmoke#
data222<-data22
data222<- data22[data22$secondhandsomke != 9, ]
data222$secondhandsomke <- factor(data222$secondhandsomke, levels = c(1, 2))
secondhandsomke_levels <- levels(data222$secondhandsomke)
for (level in secondhandsomke_levels) {
  subset_data <- data222[data222$secondhandsomke == level, ]
  av5no2_10_secondhandsomke <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                         +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2+cookedvegetablegroup
                                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                         +incomegroup+gastricreflux+METgroup+av5pm25group+av5no2_10, data = subset_data)
  cat("secondhandsomke:", level, "\n")
  print(summary(av5no2_10_secondhandsomke))
}



av5no2_secondhandsomke_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5no2_10*secondhandsomke, data = data222)
summary(av5no2_secondhandsomke_2)
remove(data222)
####
data222<- data22[data22$METgrou != 9, ]
data222$METgroup <- factor(data222$METgroup, levels = c(1, 2,3,4))
METgroup_levels <- levels(data222$METgroup)
for (level in METgroup_levels) {
  subset_data <- data222[data222$METgroup == level, ]
  av5no2_10_METgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                     +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                     +incomegroup+gastricreflux+av5pm25group+av5no2_10, data = subset_data)
  cat("METgroup Level:", level, "\n")
  print(summary(av5no2_10_METgroup))
}
av5no2_METgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5no2_10+av5pm25group, data = data222)

av5no2_METgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup*av5no2_10+av5pm25group, data = data222)
anova(av5no2_METgroup_1,av5no2_METgroup_2)
remove(data222)
##Stratified analysis ofnox################
#pm25#
av5pm25group_levels <- levels(data22$av5pm25group)
for (level in av5pm25group_levels) {
  subset_data <- data22[data22$av5pm25group == level, ]
  av5nox_10_av5pm25group <- coxph(Surv(time, time2, ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                  +incomegroup +gastricreflux+METgroup+av5nox_10, data = subset_data)
  cat("av5pm25group Level:", level, "\n")
  print(summary(av5nox_10_av5pm25group))
}


av5nox_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5nox_10+av5pm25group, data = data22)

av5nox_pm25_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5nox_10*av5pm25group, data = data22)
anova(av5nox_pm25_1,av5nox_pm25_2)
#age#
agegroup_levels <- levels(data22$agegroup)
for (level in agegroup_levels) {
  subset_data <- data22[data22$agegroup == level, ]
  av5nox_10_agegroup <- coxph(Surv(time, time2, ec) ~ gender+educationgroup+urbangroup+familyhistory+BMIgroup
                              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                              +incomegroup +gastricreflux+METgroup+av5nox_10+av5pm25group, data = subset_data)
  cat("agegroup Level:", level, "\n")
  print(summary(av5nox_10_agegroup))
}

av5nox_agegroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5nox_10+av5pm25group, data = data22)

av5nox_agegroup_2<-coxph(Surv(time,time2,ec) ~  gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5nox_10*agegroup+av5pm25group, data = data22)
anova(av5nox_agegroup_1,av5nox_agegroup_2)
#gender#
data22$gender <- as.factor(data22$gender)
gender_levels <- levels(data22$gender)
for (level in gender_levels) {
  subset_data <- data22[data22$gender == level, ]
  av5nox_10_gender <- coxph(Surv(time, time2, ec) ~ agegroup +educationgroup+urbangroup+familyhistory+BMIgroup
                            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                            +incomegroup +gastricreflux+METgroup+av5nox_10+av5pm25group, data = subset_data)
  cat("gender Level:", level, "\n")
  print(summary(av5nox_10_gender))
}



av5nox_gender_2<-coxph(Surv(time,time2,ec) ~ agegroup+educationgroup+urbangroup+familyhistory+BMIgroup
                       +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+av5pm25group+av5nox_10*gender, data = data22)
summary(av5nox_gender_2)
#smoke#
data222 <- data22[data22$smokingstatusgroup != 9, ]
data222$smokingstatusgroup <- factor(data222$smokingstatusgroup, levels = c(1, 2, 3))
smokingstatusgroup_levels <- levels(data222$smokingstatusgroup)
for (level in smokingstatusgroup_levels) {
  subset_data <- data222[data222$smokingstatusgroup == level, ]
  av5nox_10_smokingstatusgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                        +familyhistory+BMIgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                        +incomegroup +gastricreflux+METgroup+av5pm25group+av5nox_10, data = subset_data)
  cat("smokingstatusgroup Level:", level, "\n")
  print(summary(av5nox_10_smokingstatusgroup))
}

av5nox_smokingstatusgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5nox_10+smokingstatusgroup
                                   , data = data222)
av5nox_smokingstatusgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5nox_10*smokingstatusgroup
                                   , data = data222)
anova(av5nox_smokingstatusgroup_1,av5nox_smokingstatusgroup_2)
remove(data222)
#alcoholintakegroup#
data222 <- data22[data22$alcoholintakegroup2 != 9, ]
data222$alcoholintakegroup2 <- factor(data222$alcoholintakegroup2, levels = c(1, 2))
table(data222$alcoholintakegroup2)
alcoholintakegroup2_levels <- levels(data222$alcoholintakegroup2)
for (level in alcoholintakegroup2_levels) {
  subset_data <- data222[data222$alcoholintakegroup2 == level, ]
  av5nox_10_alcoholintakegroup2 <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                         +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+cookedvegetablegroup
                                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                         +incomegroup +gastricreflux+METgroup+av5pm25group+av5nox_10, data = subset_data)
  cat("alcoholintakegroup2 Level:", level, "\n")
  print(summary(av5nox_10_alcoholintakegroup2))
}


av5nox_alcoholintakegroup2_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5nox_10*alcoholintakegroup2, data = data222)
summary(av5nox_alcoholintakegroup2_2)
anova(av5nox_alcoholintakegroup2_1,av5nox_alcoholintakegroup2_2)
remove(data222)
#secondhandsmoke#
data222<- data22[data22$secondhandsomke != 9, ]
data222$secondhandsomke <- factor(data222$secondhandsomke, levels = c(1, 2))
secondhandsomke_levels <- levels(data222$secondhandsomke)
for (level in secondhandsomke_levels) {
  subset_data <- data222[data222$secondhandsomke == level, ]
  av5nox_10_secondhandsomke <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                     +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2+cookedvegetablegroup
                                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                     +incomegroup+gastricreflux+METgroup+av5pm25group+av5nox_10, data = subset_data)
  cat("secondhandsomke:", level, "\n")
  print(summary(av5nox_10_secondhandsomke))
}



av5nox_secondhandsomke_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2
                                +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                +av5pm25group+av5nox_10*secondhandsomke, data = data222)
summary(av5nox_secondhandsomke_2)
remove(data222)
####
data222<- data22[data22$METgrou != 9, ]
data222$METgroup <- factor(data222$METgroup, levels = c(1, 2,3,4))
METgroup_levels <- levels(data222$METgroup)
for (level in METgroup_levels) {
  subset_data <- data222[data222$METgroup == level, ]
  av5nox_10_METgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                              +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                              +incomegroup+gastricreflux+av5pm25group+av5nox_10, data = subset_data)
  cat("METgroup Level:", level, "\n")
  print(summary(av5nox_10_METgroup))
}
av5nox_METgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5nox_10+av5pm25group, data = data222)

av5nox_METgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup*av5nox_10+av5pm25group, data = data222)
anova(av5nox_METgroup_1,av5nox_METgroup_2)
remove(data222)
##Stratified analysis of so2##############
#pm25#
av5pm25group_levels <- levels(data22$av5pm25group)
for (level in av5pm25group_levels) {
  subset_data <- data22[data22$av5pm25group == level, ]
  av5so2_av5pm25group <- coxph(Surv(time, time2, ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                                  +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                  +incomegroup +gastricreflux+METgroup+av5so2, data = subset_data)
  cat("av5pm25group Level:", level, "\n")
  print(summary(av5so2_av5pm25group))
}


av5so2_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5so2+av5pm25group, data = data22)

av5so2_pm25_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5so2*av5pm25group, data = data22)
anova(av5so2_pm25_1,av5so2_pm25_2)
#age#
agegroup_levels <- levels(data22$agegroup)
for (level in agegroup_levels) {
  subset_data <- data22[data22$agegroup == level, ]
  av5so2_agegroup <- coxph(Surv(time, time2, ec) ~ gender+educationgroup+urbangroup+familyhistory+BMIgroup
                              +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                              +incomegroup +gastricreflux+METgroup+av5so2+av5pm25group, data = subset_data)
  cat("agegroup Level:", level, "\n")
  print(summary(av5so2_agegroup))
}

av5so2_agegroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5so2+av5pm25group, data = data22)

av5so2_agegroup_2<-coxph(Surv(time,time2,ec) ~  gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5so2*agegroup+av5pm25group, data = data22)
anova(av5so2_agegroup_1,av5so2_agegroup_2)
#gender#
data22$gender <- as.factor(data22$gender)
gender_levels <- levels(data22$gender)
for (level in gender_levels) {
  subset_data <- data22[data22$gender == level, ]
  av5so2_gender <- coxph(Surv(time, time2, ec) ~ agegroup +educationgroup+urbangroup+familyhistory+BMIgroup
                            +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                            +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                            +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                            +incomegroup +gastricreflux+METgroup+av5so2+av5pm25group, data = subset_data)
  cat("gender Level:", level, "\n")
  print(summary(av5so2_gender))
}



av5so2_gender_2<-coxph(Surv(time,time2,ec) ~ agegroup+educationgroup+urbangroup+familyhistory+BMIgroup
                       +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+av5pm25group+av5so2*gender, data = data22)
summary(av5so2_gender_2)
#smoke#
data222 <- data22[data22$smokingstatusgroup != 9, ]
data222$smokingstatusgroup <- factor(data222$smokingstatusgroup, levels = c(1, 2, 3))
smokingstatusgroup_levels <- levels(data222$smokingstatusgroup)
for (level in smokingstatusgroup_levels) {
  subset_data <- data222[data222$smokingstatusgroup == level, ]
  av5so2_smokingstatusgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                        +familyhistory+BMIgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                        +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                        +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                        +incomegroup +gastricreflux+METgroup+av5pm25group+av5so2, data = subset_data)
  cat("smokingstatusgroup Level:", level, "\n")
  print(summary(av5so2_smokingstatusgroup))
}

av5so2_smokingstatusgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5so2+smokingstatusgroup
                                   , data = data222)
av5so2_smokingstatusgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5so2*smokingstatusgroup
                                   , data = data222)
anova(av5so2_smokingstatusgroup_1,av5so2_smokingstatusgroup_2)
remove(data222)
#alcoholintakegroup#
data222 <- data22[data22$alcoholintakegroup2 != 9, ]
data222$alcoholintakegroup2 <- factor(data222$alcoholintakegroup2, levels = c(1, 2))
table(data222$alcoholintakegroup2)
alcoholintakegroup2_levels <- levels(data222$alcoholintakegroup2)
for (level in alcoholintakegroup2_levels) {
  subset_data <- data222[data222$alcoholintakegroup2 == level, ]
  av5so2_alcoholintakegroup2 <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                         +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+cookedvegetablegroup
                                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                         +incomegroup +gastricreflux+METgroup+av5pm25group+av5so2, data = subset_data)
  cat("alcoholintakegroup2 Level:", level, "\n")
  print(summary(av5so2_alcoholintakegroup2))
}


av5so2_alcoholintakegroup2_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5so2*alcoholintakegroup2, data = data222)
summary(av5so2_alcoholintakegroup2_2)
remove(data222)
#secondhandsmoke#
data222<- data22[data22$secondhandsomke != 9, ]
data222$secondhandsomke <- factor(data222$secondhandsomke, levels = c(1, 2))
secondhandsomke_levels <- levels(data222$secondhandsomke)
for (level in secondhandsomke_levels) {
  subset_data <- data222[data222$secondhandsomke == level, ]
  av5so2_secondhandsomke <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                     +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2+cookedvegetablegroup
                                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                     +incomegroup+gastricreflux+METgroup+av5pm25group+av5so2, data = subset_data)
  cat("secondhandsomke:", level, "\n")
  print(summary(av5so2_secondhandsomke))
}



av5so2_secondhandsomke_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2
                                +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                +av5pm25group+av5so2*secondhandsomke, data = data222)
summary(av5so2_secondhandsomke_2)
remove(data222)
####
data222<- data22[data22$METgrou != 9, ]
data222$METgroup <- factor(data222$METgroup, levels = c(1, 2,3,4))
METgroup_levels <- levels(data222$METgroup)
for (level in METgroup_levels) {
  subset_data <- data222[data222$METgroup == level, ]
  av5so2_METgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                              +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                              +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                              +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                              +incomegroup+gastricreflux+av5pm25group+av5so2, data = subset_data)
  cat("METgroup Level:", level, "\n")
  print(summary(av5so2_METgroup))
}
av5so2_METgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5so2+av5pm25group, data = data222)

av5so2_METgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup*av5so2+av5pm25group, data = data222)
anova(av5so2_METgroup_1,av5so2_METgroup_2)
remove(data222)
##Stratified analysis of benzene######
#pm25#
av5pm25group_levels <- levels(data22$av5pm25group)
for (level in av5pm25group_levels) {
  subset_data <- data22[data22$av5pm25group == level, ]
  av5bz_01_av5pm25group <- coxph(Surv(time, time2, ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                               +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                               +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                               +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                               +incomegroup +gastricreflux+METgroup+av5bz_01, data = subset_data)
  cat("av5pm25group Level:", level, "\n")
  print(summary(av5bz_01_av5pm25group))
}


av5bz_01_pm25_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5bz_01+av5pm25group, data = data22)

av5bz_01_pm25_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                     +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                     +incomegroup +gastricreflux+METgroup+av5bz_01*av5pm25group, data = data22)
anova(av5bz_01_pm25_1,av5bz_01_pm25_2)
#age#
agegroup_levels <- levels(data22$agegroup)
for (level in agegroup_levels) {
  subset_data <- data22[data22$agegroup == level, ]
  av5bz_01_agegroup <- coxph(Surv(time, time2, ec) ~ gender+educationgroup+urbangroup+familyhistory+BMIgroup
                           +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                           +incomegroup +gastricreflux+METgroup+av5bz_01+av5pm25group, data = subset_data)
  cat("agegroup Level:", level, "\n")
  print(summary(av5bz_01_agegroup))
}

av5bz_01_agegroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5bz_01+av5pm25group, data = data22)

av5bz_01_agegroup_2<-coxph(Surv(time,time2,ec) ~  gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5bz_01*agegroup+av5pm25group, data = data22)
anova(av5bz_01_agegroup_1,av5bz_01_agegroup_2)
#gender#
data22$gender <- as.factor(data22$gender)
gender_levels <- levels(data22$gender)
for (level in gender_levels) {
  subset_data <- data22[data22$gender == level, ]
  av5bz_01_gender <- coxph(Surv(time, time2, ec) ~ agegroup +educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5bz_01+av5pm25group, data = subset_data)
  cat("gender Level:", level, "\n")
  print(summary(av5bz_01_gender))
}



av5bz_01_gender_2<-coxph(Surv(time,time2,ec) ~ agegroup+educationgroup+urbangroup+familyhistory+BMIgroup
                       +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                       +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                       +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                       +incomegroup +gastricreflux+METgroup+av5pm25group+av5bz_01*gender, data = data22)
summary(av5bz_01_gender_2)
#smoke#
data222 <- data22[data22$smokingstatusgroup != 9, ]
data222$smokingstatusgroup <- factor(data222$smokingstatusgroup, levels = c(1, 2, 3))
smokingstatusgroup_levels <- levels(data222$smokingstatusgroup)
for (level in smokingstatusgroup_levels) {
  subset_data <- data222[data222$smokingstatusgroup == level, ]
  av5bz_01_smokingstatusgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                     +familyhistory+BMIgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                     +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                     +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                     +incomegroup +gastricreflux+METgroup+av5pm25group+av5bz_01, data = subset_data)
  cat("smokingstatusgroup Level:", level, "\n")
  print(summary(av5bz_01_smokingstatusgroup))
}

av5bz_01_smokingstatusgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5bz_01+smokingstatusgroup
                                   , data = data222)
av5bz_01_smokingstatusgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup+gender+educationgroup+urbangroup
                                   +familyhistory+BMIgroup +secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                                   +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                   +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                   +incomegroup +gastricreflux+METgroup+av5pm25group+av5bz_01*smokingstatusgroup
                                   , data = data222)
anova(av5bz_01_smokingstatusgroup_1,av5bz_01_smokingstatusgroup_2)
remove(data222)
#alcoholintakegroup#
data222 <- data22[data22$alcoholintakegroup2 != 9, ]
data222$alcoholintakegroup2 <- factor(data222$alcoholintakegroup2, levels = c(1, 2))
table(data222$alcoholintakegroup2)
alcoholintakegroup2_levels <- levels(data222$alcoholintakegroup2)
for (level in alcoholintakegroup2_levels) {
  subset_data <- data222[data222$alcoholintakegroup2 == level, ]
  av5bz_01_alcoholintakegroup2 <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                      +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+cookedvegetablegroup
                                      +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                      +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                      +incomegroup +gastricreflux+METgroup+av5pm25group+av5bz_01, data = subset_data)
  cat("alcoholintakegroup2 Level:", level, "\n")
  print(summary(av5bz_01_alcoholintakegroup2))
}


av5bz_01_alcoholintakegroup2_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                    +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke
                                    +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                    +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                    +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                    +av5pm25group+av5bz_01*alcoholintakegroup2, data = data222)
summary(av5bz_01_alcoholintakegroup2_2)
remove(data222)
#secondhandsmoke#
data222<- data22[data22$secondhandsomke != 9, ]
data222$secondhandsomke <- factor(data222$secondhandsomke, levels = c(1, 2))
secondhandsomke_levels <- levels(data222$secondhandsomke)
for (level in secondhandsomke_levels) {
  subset_data <- data222[data222$secondhandsomke == level, ]
  av5bz_01_secondhandsomke <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                                  +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2+cookedvegetablegroup
                                  +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                                  +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                                  +incomegroup+gastricreflux+METgroup+av5pm25group+av5bz_01, data = subset_data)
  cat("secondhandsomke:", level, "\n")
  print(summary(av5bz_01_secondhandsomke))
}



av5bz_01_secondhandsomke_2<-coxph(Surv(time,time2,ec) ~ gender+agegroup+educationgroup+urbangroup
                                +familyhistory+BMIgroup+smokingstatusgroup+alcoholintakegroup2
                                +cookedvegetablegroup+rawvegetablegroup+processedmeatgroup
                                +freshfruitgroup+oilyfishgroup+solidfuelcookinggroup+hotdrinktempgroup
                                +sittimegroup+waisthipratiogroup+incomegroup +gastricreflux+METgroup
                                +av5pm25group+av5bz_01*secondhandsomke, data = data222)
summary(av5bz_01_secondhandsomke_2)
remove(data222)
####
data222<- data22[data22$METgrou != 9, ]
data222$METgroup <- factor(data222$METgroup, levels = c(1, 2,3,4))
METgroup_levels <- levels(data222$METgroup)
for (level in METgroup_levels) {
  subset_data <- data222[data222$METgroup == level, ]
  av5bz_01_METgroup <- coxph(Surv(time, time2, ec) ~ agegroup+gender +educationgroup+urbangroup
                           +familyhistory+BMIgroup+smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                           +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                           +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                           +incomegroup+gastricreflux+av5pm25group+av5bz_01, data = subset_data)
  cat("METgroup Level:", level, "\n")
  print(summary(av5bz_01_METgroup))
}
av5bz_01_METgroup_1<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup+av5bz_01+av5pm25group, data = data222)

av5bz_01_METgroup_2<-coxph(Surv(time,time2,ec) ~ agegroup + gender+educationgroup+urbangroup+familyhistory+BMIgroup
                         +smokingstatusgroup+secondhandsomke+alcoholintakegroup2+cookedvegetablegroup
                         +rawvegetablegroup+processedmeatgroup+freshfruitgroup+oilyfishgroup
                         +solidfuelcookinggroup+hotdrinktempgroup+sittimegroup+waisthipratiogroup
                         +incomegroup +gastricreflux+METgroup*av5bz_01+av5pm25group, data = data222)
anova(av5bz_01_METgroup_1,av5bz_01_METgroup_2)
##Cochrane’s Q test#####
install.packages("metafor")
library(metafor)
#nox-pm2.5#
hr <- c(1.108,1.354,1.203,1.097) # 风险比
ci_lower <- c(0.883,1.085,0.977,0.991) # 95% CI下限
ci_upper <- c(1.391,1.69,1.48,1.214) # 95% CI上限
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
summary(res)
#nox-age#
hr <- c(0.97,1.231,1.131,1.098) # 风险比
ci_lower <- c(0.707,1.049,0.999,0.965) # 95% CI下限
ci_upper <- c(1.331,1.444,1.281,1.25)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#nox-gender#
hr <- c(1.112,1.136) # 风险比
ci_lower <- c(0.967,1.037) # 95% CI下限
ci_upper <- c(1.278,1.243)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#nox-smoke#
hr <- c(1.076, 1.158,1.111) # 风险比
ci_lower <- c(0.932,1.041,0.934) # 95% CI下限
ci_upper <- c(1.242,1.287,1.321)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#nox-sesmoke#
hr <- c(1.123,1.221) # 风险比
ci_lower <- c(1.013,1.036) # 95% CI下限
ci_upper <- c(1.245,1.439)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#nox-Alcohol#
hr <- c(1.185,1.056) # 风险比
ci_lower <- c(1.06,0.948) # 95% CI下限
ci_upper <- c(1.325,1.176)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#nox-met#
hr <- c(1.127,1.177,1.024,1.09) # 风险比
ci_lower <- c(0.957,1,0.832,0.908) # 95% CI下限
ci_upper <- c(1.326,1.385,1.261,1.309)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-pm25#
hr <- c(1.183,1.688,1.389,1.213) # 风险比
ci_lower <- c(0.823,1.154,0.963,0.981) # 95% CI下限
ci_upper <- c(1.7,2.47,2.002,1.5)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-age#
hr <- c(0.954,1.512,1.287,1.221) # 风险比
ci_lower <- c(0.524,1.093,1.009, 0.96) # 95% CI下限
ci_upper <- c(1.734,2.09, 1.642, 1.553)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-gender#
hr <- c(1.262,1.285) # 风险比
ci_lower <- c(0.965,1.078) # 95% CI下限
ci_upper <- c(1.65,1.531)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-smoke#
hr <- c(1.179,1.371,1.166) # 风险比
ci_lower <- c(0.901,1.115,0.829) # 95% CI下限
ci_upper <- c(1.542,1.687,1.641)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-se#
hr <- c(1.288,1.528) # 风险比
ci_lower <- c(1.058,1.104) # 95% CI下限
ci_upper <- c(1.568,2.114)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-se#
hr <- c(1.415,1.124) # 风险比
ci_lower <- c(1.141,0.916) # 95% CI下限
ci_upper <- c(1.756,1.381)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#no2-met#
hr <- c(1.246,1.43,1.062,1.213) # 风险比
ci_lower <- c(0.909,1.035,0.722,0.856) # 95% CI下限
ci_upper <- c(1.708, 1.975, 1.561,1.718)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#so2-pm#
hr <- c(1.082,1.047,1.084,1.162) # 风险比
ci_lower <- c(0.894,0.889,0.963,1.058) # 95% CI下限
ci_upper <- c(1.31,1.234,1.22,1.276)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#so2-age#
hr <- c(1.07,1.01,1.143,1.139) # 风险比
ci_lower <- c(0.842,0.865,1.039,1.017) # 95% CI下限
ci_upper <- c(1.359,1.179,1.257, 1.275)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#so2-gender#
hr <- c(1.079,1.122) # 风险比
ci_lower <- c(0.949, 1.043) # 95% CI下限
ci_upper <- c(1.226,1.206)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#so2-smoke#
hr <- c(0.935,1.19,1.139) # 风险比
ci_lower <- c(0.817,1.097,0.991) # 95% CI下限
ci_upper <- c(1.07,1.291,1.309)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
##so2-Second-hand smoke exposure##
hr <- c(1.099,1.163) # 风险比
ci_lower <- c(1.008,1.02) # 95% CI下限
ci_upper <- c(1.199,1.326)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
##so2-Alcohol   ##
hr <- c(1.124,1.096) # 风险比
ci_lower <- c(1.023,1.006) # 95% CI下限
ci_upper <- c(1.236,1.195)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
##so2-met##
hr <- c(1.141,1.099,1.135,0.949) # 风险比
ci_lower <- c(0.996, 0.942, 0.974,0.799) # 95% CI下限
ci_upper <- c(1.306, 1.284,1.323, 1.126)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
##bz-pm##
hr <- c(1.11,1.072,1.106,1.071) # 风险比
ci_lower <- c(0.955,0.908, 0.943,0.946) # 95% CI下限
ci_upper <- c(1.29,1.266,1.298,1.212)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
##bz-age##
hr <- c(1.11,1.072,1.106,1.071) # 风险比
ci_lower <- c(0.955,0.908, 0.943,0.946) # 95% CI下限
ci_upper <- c(1.29,1.266,1.298,1.212)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#分型-no2# 结果：Q(df = 1) = 1.1987, p-val = 0.2736#
hr <- c(1.227,1.482) # 风险比
ci_lower <- c(1.004,1.129) # 95% CI下限
ci_upper <- c(1.500,1.945)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#分型-no2## 结果Q(df = 1) = 0.9121, p-val = 0.3395
hr <- c(1.110,1.207) 
ci_lower <- c(1.000,1.053) # 95% CI下限
ci_upper <- c(1.232,1.384)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#分型-so2## 结果Q(df = 1) = 0.1078, p-val = 0.7427
hr <- c(1.120,1.091) 
ci_lower <- c(1.032,0.954) # 95% CI下限
ci_upper <- c(1.214,1.247)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)
#分型-bz## 结果QQ(df = 1) = 1.2423, p-val = 0.2650
hr <- c(1.062,1.169) 
ci_lower <- c(0.962,1.019) # 95% CI下限
ci_upper <- c(1.172,1.340)
loghr <- log(hr)
seloghr <- (log(ci_upper) - log(ci_lower)) / (2*1.96)
res <- rma(yi=loghr, sei=seloghr, method="FE")
print(res)



##Forest plot.#####
###Before this step, we manually organized the results 
##of the Cox regression into a CSV file####
###Forest plot.no2######################
library(forestploter)
library(ggplot2)
library(grid)
#no2#
dt <- read.csv("no2.csv",check.names = F,
               strip.white =F,
               # 全部读取为字符变量
               colClasses ="character")
# 
numvar=c("HR" ,"HR_lci95","HR_uci95")
dt[,c(numvar)]<-lapply(dt[,numvar],as.numeric)
# 创建空白并移到
dt$` ` <- paste(rep(" ", 15), # 这个空格就是留着森林图的
                collapse = " ")
names(dt)
# 移动下空白行的位置
dt <- dt[ ,c(1:6,9,7:8)]

num_rows <- nrow(dt)
# 然后使用循环来交替赋值 'A' 和 'B'
dt$g <- rep_len(rep(c("A", "B"), length.out = num_rows), num_rows)

# Define theme
tm <- forest_theme(base_size = 10,
                   ci_pch = 15,
                   ci_col = "#0072b2",  # 置信区间颜色
                   ci_fill = "#0072b2", # 点颜色
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   # 红色的参考线
                   refline_col = "red", # 颜色
                   refline_lty = 2  # 0-6 线形
)

p <- forest(
  #  筛选用到的列,8为第4列在excel为空
  dt[,c(1:3, 7:9)], 
  est = dt$HR[1:28],
  lower = dt$HR_lci95[1:28],
  upper = dt$HR_uci95[1:28],
  # 这边可以更改森林图的位置
  ci_column = 4, # 第四列为空，显示森林图
  # 数值虚线，一般OR/HR=1的位置
  ref_line = 1, 
  # X轴刻度范围
  xlim = c(0, 2.5),
  # X轴刻度间隔
  ticks_at = c(0.5, 1, 1.5,2),
  # 左下角的脚注
  #footnote = "This is the demo data. \nanything you want.",
  # 自定义的主题
  theme = tm);plot(p)
####
pp <- edit_plot(add_border(p, part = "header", where = "bottom"))
pp
p_wh <- get_wh(plot = pp, unit = "in")
png('no2.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
pp
dev.off()
#或手动保存为pdf#
###Forest plot.nox#######
dt <- read.csv("nox.csv",check.names = F,
               strip.white =F,
               # 全部读取为字符变量
               colClasses ="character")
# 
numvar=c("HR" ,"HR_lci95","HR_uci95")
dt[,c(numvar)]<-lapply(dt[,numvar],as.numeric)
# 创建空白并移到
dt$` ` <- paste(rep(" ", 15), # 这个空格就是留着森林图的
                collapse = " ")
names(dt)
# 移动下空白行的位置
dt <- dt[ ,c(1:6,9,7:8)]

num_rows <- nrow(dt)
# 然后使用循环来交替赋值 'A' 和 'B'
dt$g <- rep_len(rep(c("A", "B"), length.out = num_rows), num_rows)

# Define theme
tm <- forest_theme(base_size = 10,
                   ci_pch = 15,
                   ci_col = "#0072b2",  # 置信区间颜色
                   ci_fill = "#0072b2", # 点颜色
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   # 红色的参考线
                   refline_col = "red", # 颜色
                   refline_lty = 2  # 0-6 线形
)

p <- forest(
  #  筛选用到的列,8为第4列在excel为空
  dt[,c(1:3, 7:9)], 
  est = dt$HR[1:28],
  lower = dt$HR_lci95[1:28],
  upper = dt$HR_uci95[1:28],
  # 这边可以更改森林图的位置
  ci_column = 4, # 第四列为空，显示森林图
  # 数值虚线，一般OR/HR=1的位置
  ref_line = 1, 
  # X轴刻度范围
  xlim = c(0.7, 1.7),
  # X轴刻度间隔
  ticks_at = c(0.75, 1, 1.25,1.5),
  # 左下角的脚注
  #footnote = "This is the demo data. \nanything you want.",
  # 自定义的主题
  theme = tm);plot(p)
####
pp <- edit_plot(add_border(p, part = "header", where = "bottom"))
pp
p_wh <- get_wh(plot = pp, unit = "in")
png('nox.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
pp
dev.off()
#或手动保存为pdf#

###Forest plot.so2#######
dt <- read.csv("so2.csv",check.names = F,
               strip.white =F,
               # 全部读取为字符变量
               colClasses ="character")
# 
numvar=c("HR" ,"HR_lci95","HR_uci95")
dt[,c(numvar)]<-lapply(dt[,numvar],as.numeric)
# 创建空白并移到
dt$` ` <- paste(rep(" ", 15), # 这个空格就是留着森林图的
                collapse = " ")
names(dt)
# 移动下空白行的位置
dt <- dt[ ,c(1:6,9,7:8)]

num_rows <- nrow(dt)
# 然后使用循环来交替赋值 'A' 和 'B'
dt$g <- rep_len(rep(c("A", "B"), length.out = num_rows), num_rows)

# Define theme
tm <- forest_theme(base_size = 10,
                   ci_pch = 15,
                   ci_col = "#0072b2",  # 置信区间颜色
                   ci_fill = "#0072b2", # 点颜色
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   # 红色的参考线
                   refline_col = "red", # 颜色
                   refline_lty = 2  # 0-6 线形
)

p <- forest(
  #  筛选用到的列,8为第4列在excel为空
  dt[,c(1:3, 7:9)], 
  est = dt$HR[1:28],
  lower = dt$HR_lci95[1:28],
  upper = dt$HR_uci95[1:28],
  # 这边可以更改森林图的位置
  ci_column = 4, # 第四列为空，显示森林图
  # 数值虚线，一般OR/HR=1的位置
  ref_line = 1, 
  # X轴刻度范围
  xlim = c(0.75, 1.5),
  # X轴刻度间隔
  ticks_at = c(0.75, 1, 1.25,1.5),
  # 左下角的脚注
  #footnote = "This is the demo data. \nanything you want.",
  # 自定义的主题
  theme = tm);plot(p)
####
pp <- edit_plot(add_border(p, part = "header", where = "bottom"))
pp
p_wh <- get_wh(plot = pp, unit = "in")
png('so2.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
pp
dev.off()
#或手动保存为pdf#

###Forest plot.benzene#######
dt <- read.csv("bz.csv",check.names = F,
               strip.white =F,
               # 全部读取为字符变量
               colClasses ="character")
# 
numvar=c("HR" ,"HR_lci95","HR_uci95")
dt[,c(numvar)]<-lapply(dt[,numvar],as.numeric)
# 创建空白并移到
dt$` ` <- paste(rep(" ", 15), # 这个空格就是留着森林图的
                collapse = " ")
names(dt)
# 移动下空白行的位置
dt <- dt[ ,c(1:6,9,7:8)]

num_rows <- nrow(dt)
# 然后使用循环来交替赋值 'A' 和 'B'
dt$g <- rep_len(rep(c("A", "B"), length.out = num_rows), num_rows)

# Define theme
tm <- forest_theme(base_size = 10,
                   ci_pch = 15,
                   ci_col = "#0072b2",  # 置信区间颜色
                   ci_fill = "#0072b2", # 点颜色
                   footnote_col = "#636363",
                   footnote_fontface = "italic",
                   # 红色的参考线
                   refline_col = "red", # 颜色
                   refline_lty = 2  # 0-6 线形
)

p <- forest(
  #  筛选用到的列,8为第4列在excel为空
  dt[,c(1:3, 7:9)], 
  est = dt$HR[1:28],
  lower = dt$HR_lci95[1:28],
  upper = dt$HR_uci95[1:28],
  # 这边可以更改森林图的位置
  ci_column = 4, # 第四列为空，显示森林图
  # 数值虚线，一般OR/HR=1的位置
  ref_line = 1, 
  # X轴刻度范围
  xlim = c(0.72, 1.5),
  # X轴刻度间隔
  ticks_at = c(0.75, 1, 1.25,1.5),
  # 左下角的脚注
  #footnote = "This is the demo data. \nanything you want.",
  # 自定义的主题
  theme = tm);plot(p)
####
pp <- edit_plot(add_border(p, part = "header", where = "bottom"))
pp
p_wh <- get_wh(plot = pp, unit = "in")
png('so2.png', res = 300, width = p_wh[1], height = p_wh[2], units = "in")
pp
dev.off()
#或手动保存为pdf#


