#加载需要的包#
library(readxl)
library(dplyr)
#########ukbdata, merged_data111#################
ukb677331 <- read_csv("E:/UKB数据库/ukb677331.csv")
###calculation of Change in residential address distance#####################
ukblocation <- read_csv("E:/UKB数据库/ukblocation.csv")
num_vars <- 14
for (i in 1:num_vars) {
  new_var_name <- paste0("x", i)
  ukblocation[[new_var_name]] <- 0
  missing_index <- is.na(ukblocation[[paste0("22702-0.", i)]])
  ukblocation[[new_var_name]][missing_index] <- 0
  non_missing_index <- !missing_index
  ukblocation[[new_var_name]][non_missing_index] <- abs(ukblocation[[paste0("22702-0.", i)]][non_missing_index] - ukblocation[["22702-0.0"]][non_missing_index])
}
for (i in 1:num_vars) {
  new_var_name <- paste0("y", i)
  ukblocation[[new_var_name]] <- 0
  missing_index <- is.na(ukblocation[[paste0("22704-0.", i)]])
  ukblocation[[new_var_name]][missing_index] <- 0
  non_missing_index <- !missing_index
  ukblocation[[new_var_name]][non_missing_index] <- abs(ukblocation[[paste0("22704-0.", i)]][non_missing_index] - ukblocation[["22704-0.0"]][non_missing_index])
}

for (i in 1:num_vars) {
  new_var_name <- paste0("d", i)
  ukblocation[[new_var_name]] <- sqrt(ukblocation[[paste0("x", i)]]^2 + ukblocation[[paste0("y", i)]]^2)
}

ukblocation$locationchange <- do.call(pmax, ukblocation[grep("^d", names(ukblocation))])
ukblocation <- subset(ukblocation, select = c(eid, locationchange))
##Merge the data.，merged_data111 was air pollution data##
ukbdata1 <- merge(ukb677331, merged_data111, by = "eid")
ukbdata1 <- merge(ukbdata1, ukblocation, by = "eid")
remove(ukb677331,merged_data111,ukblocation)
ukbdata<-ukbdata1
remove(ukbdata1)
#Add some variables#
ukbsup1 <- read_csv("ukbsup1.csv")
ukbsup2 <- read_excel("ukbsup2.xlsx")
ukbsup3<- read_excel("ukbsup3.xlsx")
ukbsup4 <- read_csv("ukbsup4.csv")
ukbsup5<- read_excel("ukbsup5.xlsx")
ukbdata <- merge(ukbdata, ukbsup1[, c("eid", "845-0.0", "1329-0.0", "1349-0.0", "40012-0.0")], by = "eid", all.x = TRUE)
ukbdata <- merge(ukbdata, ukbsup2[, c("eid", "189-0.0", "864-0.0", "884-0.0", "904-0.0")], by = "eid", all.x = TRUE)
#更新死亡#
ukbdata <- merge(ukbdata, ukbsup3[, c("eid","54-0.0")], by = "eid", all.x = TRUE)
ukbdata <- merge(ukbdata, ukbsup5[, c("eid","40000-0.0")], by = "eid", all.x = TRUE)

head(ukbsup5)
data<-ukbdata

table(data$`40006-0.0`,useNA = "ifany")
table(data$`40006-1.0_ec`,useNA = "ifany")

#ec1.ec1.Outcome definitions: 1 for the first occurrence of esophageal cancer, 2 for other cancers, and 0 for no cancer.#
#
data <- data %>%
  mutate_at(vars(`40006-0.0`:`40006-21.0`), list(
    ec = ~case_when(
      . %in% c("C150", "C151", "C152", "C153", "C154", "C155", "C156", "C157", "C158", "C159") ~ 1,
      TRUE ~ NA_real_
    )
  ))
table(data$`40006-0.0_ec`,useNA = "ifany")
table(data$`40006-1.0_ec`,useNA = "ifany")
table(data$`40006-2.0_ec`,useNA = "ifany")
table(data$`40006-3.0_ec`,useNA = "ifany")
table(data$`40006-4.0_ec`,useNA = "ifany")
table(data$`40006-5.0_ec`,useNA = "ifany")
table(data$`40006-6.0_ec`,useNA = "ifany")
table(data$`40006-7.0_ec`,useNA = "ifany")
table(data$`40006-8.0_ec`,useNA = "ifany")
table(data$`40006-9.0_ec`,useNA = "ifany")
table(data$`40006-10.0_ec`,useNA = "ifany")
table(data$`40006-11.0_ec`,useNA = "ifany")
table(data$`40006-12.0_ec`,useNA = "ifany")
table(data$`40006-13.0_ec`,useNA = "ifany")
table(data$`40006-14.0_ec`,useNA = "ifany")
table(data$`40006-15.0_ec`,useNA = "ifany")
table(data$`40006-16.0_ec`,useNA = "ifany")
table(data$`40006-17.0_ec`,useNA = "ifany")
table(data$`40006-18.0_ec`,useNA = "ifany")
table(data$`40006-19.0_ec`,useNA = "ifany")
table(data$`40006-20.0_ec`,useNA = "ifany")
table(data$`40006-21.0_ec`,useNA = "ifany")

#Check for any cases where the diagnosis date for confirmed patients is missing.#
count_rows <- nrow(subset(data, data$`40006-0.0_ec` == 1 & is.na(data$`40005-0.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-1.0_ec` == 1 & is.na(data$`40005-1.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-2.0._ec` == 1 & is.na(data$`40005-2.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-3.0._ec` == 1 & is.na(data$`40005-3.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-4.0._ec` == 1 & is.na(data$`40005-4.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-5.0._ec` == 1 & is.na(data$`40005-5.0`)))
cat(count_rows)
count_rows <- nrow(subset(data, data$`40006-6.0._ec` == 1 & is.na(data$`40005-6.0`)))
cat(count_rows)
#Define esophageal cancer#
data$ec <- ifelse(!is.na(data$`40006-0.0_ec`), data$`40006-0.0_ec`,
                  ifelse(!is.na(data$`40006-1.0_ec`), data$`40006-1.0_ec`,
                         ifelse(!is.na(data$`40006-2.0_ec`), data$`40006-2.0_ec`,
                                ifelse(!is.na(data$`40006-3.0_ec`), data$`40006-3.0_ec`,
                                       ifelse(!is.na(data$`40006-4.0_ec`), data$`40006-4.0_ec`, NA)))))


table(data$ec,useNA = "ifany")
#Organize the variables related to the onset time of esophageal cancer#
data$`40005-0.0_ec` <- ifelse(!is.na(data$`40006-0.0_ec`) & data$`40006-0.0_ec` == 1,
                              as.numeric(format(as.Date(data$`40005-0.0`, format="%Y-%m-%d"), "%Y")),
                              NA)
data$`40005-1.0_ec` <- ifelse(!is.na(data$`40006-1.0_ec`) & data$`40006-1.0_ec` == 1,
                              as.numeric(format(as.Date(data$`40005-1.0`, format="%Y-%m-%d"), "%Y")),
                              NA)
data$`40005-2.0_ec` <- ifelse(!is.na(data$`40006-2.0_ec`) & data$`40006-2.0_ec` == 1,
                              as.numeric(format(as.Date(data$`40005-2.0`, format="%Y-%m-%d"), "%Y")),
                              NA)
data$`40005-3.0_ec` <- ifelse(!is.na(data$`40006-3.0_ec`) & data$`40006-3.0_ec` == 1,
                              as.numeric(format(as.Date(data$`40005-3.0`, format="%Y-%m-%d"), "%Y")),
                              NA)
data$`40005-4.0_ec` <- ifelse(!is.na(data$`40006-4.0_ec`) & data$`40006-4.0_ec` == 1,
                              as.numeric(format(as.Date(data$`40005-4.0`, format="%Y-%m-%d"), "%Y")),
                              NA)
table(data$`40005-0.0_ec`,useNA = "ifany")
table(data$`40005-1.0_ec`,useNA = "ifany")
table(data$`40005-2.0_ec`,useNA = "ifany")
table(data$`40005-3.0_ec`,useNA = "ifany")
table(data$`40005-4.0_ec`,useNA = "ifany")
data$ectime <- ifelse(!is.na(data$`40005-0.0_ec`), data$`40005-0.0_ec`,
                      ifelse(!is.na(data$`40005-1.0_ec`), data$`40005-1.0_ec`,
                             ifelse(!is.na(data$`40005-2.0_ec`), data$`40005-2.0_ec`,
                                    ifelse(!is.na(data$`40005-3.0_ec`), data$`40005-3.0_ec`,  
                                           ifelse(!is.na(data$`40005-4.0_ec`), data$`40005-4.0_ec`,NA)))))
table(data$ectime)
count <- sum(data$ectime < 2010 & !is.na(data$ectime))
print(count)
#Organize variables related to tumor pathological classification.#
data$`40011-0.0_ec` <- ifelse(is.na(data$`40006-0.0_ec`), NA,
                              ifelse(data$`40006-0.0_ec` == 1, data$`40011-0.0`, NA))
data$`40011-1.0_ec` <- ifelse(is.na(data$`40006-1.0_ec`), NA,
                              ifelse(data$`40006-1.0_ec` == 1, data$`40011-1.0`, NA))
data$`40011-2.0_ec` <- ifelse(is.na(data$`40006-2.0_ec`), NA,
                              ifelse(data$`40006-2.0_ec` == 1, data$`40011-2.0`, NA))
data$`40011-3.0_ec` <- ifelse(is.na(data$`40006-3.0_ec`), NA,
                              ifelse(data$`40006-3.0_ec` == 1, data$`40011-3.0`, NA))
data$`40011-4.0_ec` <- ifelse(is.na(data$`40006-4.0_ec`), NA,
                              ifelse(data$`40006-4.0_ec` == 1, data$`40011-4.0`, NA))
table(data$`40011-0.0_ec`,useNA = "ifany")
table(data$`40011-1.0_ec`,useNA = "ifany")
table(data$`40011-2.0_ec`,useNA = "ifany")
table(data$`40011-3.0_ec`,useNA = "ifany")
table(data$`40011-4.0_ec`,useNA = "ifany")
data$histology <- ifelse(!is.na(data$`40011-0.0_ec`), data$`40011-0.0_ec`,
                            ifelse(!is.na(data$`40011-1.0_ec`), data$`40011-1.0_ec`,
                                   ifelse(!is.na(data$`40011-2.0_ec`), data$`40011-2.0_ec`,
                                          ifelse(!is.na(data$`40011-3.0_ec`), data$`40011-3.0_ec`, NA))))
table(data$histology,useNA = "ifany")
#umor classification (histology): 0 for squamous cell carcinoma, 1 for adenocarcinoma, and 2 for other types.#
data$histology <- ifelse(data$histology == 8070, 0,
                         ifelse(data$histology == 8140, 1,
                                ifelse(!is.na(data$histology), 2, NA)))
table(data$histology,useNA = "ifany")
#End time: the endpoint, either death or diagnosis of esophageal cancer#
data$deathtime <- as.integer(format(as.Date(data$`40000-0.0`, format = "%Y-%m-%d"), format = "%Y"))
table(data$deathtime,useNA = "ifany")
count <- sum(data$deathtime < 2010 & !is.na(data$deathtime))
print(count)
data$endtime <- ifelse(is.na(data$ectime) & is.na(data$deathtime), 2020,
                       ifelse(is.na(data$ectime), data$deathtime,
                              ifelse(is.na(data$deathtime), data$ectime, pmin(data$ectime, data$deathtime))))
table(data$endtime,useNA = "ifany")
data$endtime[data$endtime > 2020] <- 2020


bz_na <- sum(rowSums(is.na(data[, c("bz2010", "bz2011", "bz2012", "bz2013", "bz2014", "bz2015", "bz2016", "bz2017", "bz2018", "bz2019", "bz2020", "bz2021")])) >= 12)
cat(bz_na)
#Exclude  individuals###################

count <- sum(data$endtime < 2010)
cat(count)
data <- data[data$endtime >= 2010, ]

count<- sum(data$x == -500 & data$y == -500, na.rm = TRUE)
cat(count)
count_missing <- sum(is.na(data$x) & is.na(data$y))
cat(count_missing )
data <- data[!(data$x == -500 & data$y == -500), ]
data <- data[!(is.na(data$x) | is.na(data$y)), ]
data<- data[!(rowSums(is.na(data[, c("bz2006","bz2007","bz2008","bz2009","bz2010", "bz2011", "bz2012", "bz2013", "bz2014", "bz2015", "bz2016", "bz2017", "bz2018", "bz2019", "bz2020", "bz2021")])) == 16), ]
#Calculate the 3-year and 5-year moving averages of air pollutants.#
for (year in 2010:2020) {
  av_var_name <- paste0("av3bz", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("bz", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#5年#
for (year in 2010:2020) {
  av_var_name <- paste0("av5bz", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("bz", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars], na.rm = TRUE)
}
#dgt按规则重命名#
data <- data %>%
  rename(dgt12005 = dgt120_2005r,
         dgt12006 = dgt120_06,
         dgt12007 = dgt120_07,
         dgt12008 = dgt120_08,
         dgt12009 = dgt120_09,
         dgt12010 = dgt120_10,
         dgt12011 = dgt120_11,
         dgt12012 = dgt120_12)
#dgt3年#
for (year in 2010:2020) {
  av_var_name <- paste0("av3dgt1", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("dgt1", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#dgt5年#
for (year in 2010:2020) {
  av_var_name <- paste0("av5dgt1", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("dgt1", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#No2#
#No23年#
for (year in 2010:2020) {
  av_var_name <- paste0("av3no2", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("no2", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#No25年#
for (year in 2010:2020) {
  av_var_name <- paste0("av5no2", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("no2", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#nox3#
for (year in 2010:2020) {
  av_var_name <- paste0("av3nox", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("nox", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#nox5#
for (year in 2010:2020) {
  av_var_name <- paste0("av5nox", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("nox", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#pm10#
data <- data %>%
  rename( pm102006=pm102006gh,
          pm102007=pm102007g,
          pm102008=pm102008g,
          pm102009=pm102009g,
          pm102010=pm102010g,
          pm102011=pm102011g,
          pm102012=pm102012g,
          pm102013=pm102013g,
          pm102014=pm102014g,
          pm102015=pm102015g,
          pm102016=pm102016g,
          pm102017=pm102017g,
          pm102018=pm102018g,
          pm102019=pm102019g,
          pm102020=pm102020g,
          pm102021=pm102021g)
#pm103 5年#
for (year in 2010:2020) {
  av_var_name <- paste0("av3pm10", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("pm10", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
for (year in 2010:2020) {
  av_var_name <- paste0("av5pm10", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("pm10", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#pm25#
data <- data %>%
  rename( pm252003=pm252003grav,
          pm252004=pm252004g,
          pm252005=pm2505ac,
          pm252006=pm252006gh,
          pm252007=pm252007g,
          pm252008=pm252008g,
          pm252009=pm252009g,
          pm252010=pm252010g,
          pm252011=pm252011g,
          pm252012=pm252012g,
          pm252013=pm252013g,
          pm252014=pm252014g,
          pm252015=pm252015g,
          pm252016=pm252016g,
          pm252017=pm252017g,
          pm252018=pm252018g,
          pm252019=pm252019g,
          pm252020=pm252020g,
          pm252021=pm252021g)
#av3\5pm25 #
for (year in 2010:2020) {
  av_var_name <- paste0("av3pm25", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("pm25", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
for (year in 2010:2020) {
  av_var_name <- paste0("av5pm25", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("pm25", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#so2#
data <- data %>%
  rename(so22002=so202annd,
         so22003=so203ann,
         so22004=so204ann,
         so22005=so205ann,
         so22006=so206ann,
         so22007=so207ann,
         so22008=so208ann,
         so22009=so209ann,
         so22010=so210ann,
         so22011=so211ann,
         so22012=so212ann,
         so22013=so213ann,
         so22014=so214ann)
#so2 3/5#
for (year in 2010:2020) {
  av_var_name <- paste0("av3so2", year)
  start_year <- year - 2
  end_year <- year
  bz_vars <- paste0("so2", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
for (year in 2010:2020) {
  av_var_name <- paste0("av5so2", year)
  start_year <- year - 4
  end_year <- year
  bz_vars <- paste0("so2", start_year:end_year)
  
  data[[av_var_name]] <- rowMeans(data[bz_vars],na.rm = TRUE)
}
#Organize the covariates.#
#gender、birth、hip_cir、waist_cir#
data <- data %>%
  rename(gender=`31-0.0`,)
data <- data %>%
  rename(birth=`34-0.0`,)
data <- data %>%
  rename(hip_cir=`49-0.0`,
         waist_cir=`48-0.0`)
data$waisthipratio<-NA
data$waisthipratio <- ifelse(is.na(data$waist_cir) | is.na(data$hip_cir), NA, data$waist_cir / data$hip_cir)
hist(data$waisthipratio)
#income#
data <- data %>%
  rename(income=`738-0.0`)
data$income[data$income == -3 | data$income == -1] <- NA
table(data$income, useNA = "ifany")
#townsend#
data <- data %>%
  rename(townsend=`22189-0.0`)
#walk#
data <- data %>%
  rename(walk=`864-0.0`)
#moderateactivity#
data <- data %>%
  rename(moderateactivity=`884-0.0`)
#vigorousactivity#
data <- data %>%
  rename(vigorousactivity=`904-0.0`)
#centre#
data <- data %>%
  rename(centre=`54-0.0`)
#education#
data <- data %>%
  rename(education=`6138-0.0`)
table(data$education, useNA = "ifany")
data$education[data$education == -3 ] <- NA
data$education[data$education == -7] <- 0
data$education[data$education > 1] <- 0
#tv computer drive#
data <- data %>%
  rename(tv_time=`1070-0.0`)
table(data$tv_time, useNA = "ifany")
data$tv_time[data$tv_time == -10] <- 0.5
data$tv_time[data$tv_time == -3 | data$tv_time == -1] <- NA
table(data$tv_time, useNA = "ifany")
data <- data %>%
  rename(computer_time=`1080-0.0`)
table(data$computer_time, useNA = "ifany")
data$computer_time[data$computer_time == -10] <- 0.5
data$computer_time[data$computer_time == -3 | data$computer_time == -1] <- NA
data <- data %>%
  rename(driving_time=`1090-0.0`)
table(data$driving_time, useNA = "ifany")
data$driving_time[data$driving_time == -10] <- 0.5
data$driving_time[data$driving_time == -3 | data$driving_time == -1] <- NA
#sittime#
data$sittime <- ifelse(is.na(data$tv_time) | is.na(data$computer_time) | is.na(data$driving_time),
                       NA,
                       data$tv_time + data$computer_time + data$driving_time)
table(data$sittime, useNA = "ifany")
#smoke#
data <- data %>%
  rename(homesmoke=`1269-0.0`)
table(data$homesmoke, useNA = "ifany")
data$homesmoke[data$homesmoke == -3 | data$homesmoke == -1] <- NA
data <- data %>%
  rename(outsidesmoke=`1279-0.0`)
table(data$outsidesmoke, useNA = "ifany")
data$outsidesmoke[data$outsidesmoke == -3 | data$outsidesmoke == -1] <- NA
#cookedvegetable#
data <- data %>%
  rename(cookedvegetable=`1289-0.0`)
table(data$cookedvegetable, useNA = "ifany")
data$cookedvegetable[data$cookedvegetable == -10] <- 0.5
data$cookedvegetable[data$cookedvegetable == -3 | data$cookedvegetable == -1] <- NA
#rawvegetable#
data <- data %>%
  rename(rawvegetable=`1299-0.0`)
table(data$rawvegetable, useNA = "ifany")
data$rawvegetable[data$rawvegetable == -10] <- 0.5
data$rawvegetable[data$rawvegetable == -3 | data$rawvegetable == -1] <- NA
#freshfruit
data <- data %>%
  rename(freshfruit=`1309-0.0`)
table(data$freshfruit, useNA = "ifany")
data$freshfruit[data$freshfruit == -10] <- 0.5
data$freshfruit[data$freshfruit == -3 | data$freshfruit == -1] <- NA
#processedmeat#
data <- data %>%
  rename(processedmeat=`1349-0.0`)
table(data$processedmeat, useNA = "ifany")
data$processedmeat[data$processedmeat == -3 | data$processedmeat == -1] <- NA
#oilyfish#
data <- data %>%
  rename(oilyfish=`1329-0.0`)
table(data$oilyfish, useNA = "ifany")
data$oilyfish[data$oilyfish == -3 | data$oilyfish == -1] <- NA
#hotdrink
data <- data %>%
  rename(hotdrinktemp=`1518-0.0`)
table(data$hotdrinktemp, useNA = "ifany")
data$hotdrinktemp[data$hotdrinktemp == -2] <- 0
data$hotdrinktemp[data$hotdrinktemp == -3] <- NA
#alcoholintake#
data <- data %>%
  rename(alcoholintake=`1558-0.0`)
table(data$alcoholintake, useNA = "ifany")
data$alcoholintake[data$alcoholintake == -3] <- NA
#solidfuelcooking 0无使用，1使用#
data <- data %>%
  rename(solidfuelcooking=`6139-0.0`)
table(data$solidfuelcooking, useNA = "ifany")
data$solidfuelcooking[data$solidfuelcooking %in% c(1, 2, -7)] <- 0
data$solidfuelcooking[data$solidfuelcooking %in% c(-3, -1)] <- NA
data$solidfuelcooking[data$solidfuelcooking == 3] <- 1
#smokingstatus#
data <- data %>%
  rename(smokingstatus=`20116-0.0`)
table(data$smokingstatus, useNA = "ifany")
data$smokingstatus[data$smokingstatus == -3] <- NA
#urban 0 rural 1 town 2 urban#
data$urban <- ifelse(data$`20118-0.0` %in% c(3, 4, 7, 8, 16, 17, 18), 0,
                     ifelse(data$`20118-0.0` %in% c(2, 6, 13, 14, 15), 1,
                            ifelse(data$`20118-0.0` %in% c(1, 5, 11, 12), 2, NA)))
table(data$urban, useNA = "ifany")
#generalhappiness,缺失太多，不能用#
data <- data %>%
  rename(generalhappiness=`20459-0.0`)
table(data$generalhappiness, useNA = "ifany")
data$generalhappiness[data$generalhappiness == -818 | data$generalhappiness == -121] <- NA

#ethnic 0白人 1亚洲人 2 黑人 3 其他#
data$ethnic <- ifelse(data$`21000-0.0` %in% c(1,1001,2001,3001,4001), 0,
                      ifelse(data$`21000-0.0` %in% c(3,1003,2003,3003,40031,5), 1,
                             ifelse(data$`21000-0.0` %in% c(4,2004,3004), 2, 
                                    ifelse(data$`21000-0.0` %in% c(2,1002,2002,3002,4002,6), 3,NA))))
table(data$`ethnic`, useNA = "ifany")
#MET#
data$MET <- rowSums(data[, c("22037-0.0", "22038-0.0", "22039-0.0")], na.rm = TRUE)
data$MET[is.na(data$`22037-0.0`) & is.na(data$`22038-0.0`) & is.na(data$`22039-0.0`)] <- NA
#BMI Underweight 0 Normal1 Overweight2 Obese3#
data$BMI <- cut(data$`23104-0.0`, breaks = c(0, 18.5, 24.9, 29.9, Inf),
                labels = c(0, 1, 2, 3),
                include.lowest = TRUE, na.allow = TRUE)
table(data$BMI, useNA = "ifany")
#age#
data$age <- ifelse(is.na(data$birth), NA, 2010 - data$birth)
#familyhistory#
data$familyhistory <- ifelse(data$`20107-0.0` %in% c(3, 4, 5) | 
                               data$`20110-0.0` %in% c(3, 4, 5) |
                               data$`20111-0.0` %in% c(3, 4, 5), 1, 0)
table(data$familyhistory, useNA = "ifany")
#Establish the study start time.#
data$startime <- 2010
data1 <- data[, c("eid" , "startime","locationchange", "endtime","ectime", "bz2010", "bz2011", "bz2012", "bz2013", "bz2014",
                  "bz2015", "bz2016", "bz2017", "bz2018", "bz2019","bz2020", "bz2021","av3bz2010", "av3bz2011",
                  "av3bz2012", "av3bz2013", "av3bz2014", "av3bz2015", "av3bz2016", "av3bz2017",
                  "av3bz2018", "av3bz2019", "av3bz2020","av5bz2010", "av5bz2011", "av5bz2012", "av5bz2013",
                  "av5bz2014", "av5bz2015", "av5bz2016", "av5bz2017", "av5bz2018", "av5bz2019","av5bz2020",
                  "dgt12010", "dgt12011", "dgt12012", "dgt12013", "dgt12014", "dgt12015", "dgt12016",
                  "dgt12017", "dgt12018", "dgt12019","dgt12020", "av3dgt12010", "av3dgt12011", "av3dgt12012",
                  "av3dgt12013", "av3dgt12014", "av3dgt12015", "av3dgt12016", "av3dgt12017",
                  "av3dgt12018", "av3dgt12019", "av3dgt12020","av5dgt12010", "av5dgt12011", "av5dgt12012",
                  "av5dgt12013", "av5dgt12014", "av5dgt12015", "av5dgt12016", "av5dgt12017",
                  "av5dgt12018", "av5dgt12019","av5dgt12020", "no22010", "no22011", "no22012", "no22013",
                  "no22014", "no22015", "no22016", "no22017", "no22018", "no22019","no22020", "av3no22010",
                  "av3no22011", "av3no22012", "av3no22013", "av3no22014", "av3no22015", "av3no22016",
                  "av3no22017", "av3no22018", "av3no22019","av3no22020", "av5no22010", "av5no22011", "av5no22012",
                  "av5no22013", "av5no22014", "av5no22015", "av5no22016", "av5no22017", "av5no22018",
                  "av5no22019","av5no22020", "nox2010", "nox2011", "nox2012", "nox2013", "nox2014", "nox2015",
                  "nox2016", "nox2017", "nox2018", "nox2019","nox2020",  "av3nox2010", "av3nox2011", "av3nox2012",
                  "av3nox2013", "av3nox2014", "av3nox2015", "av3nox2016", "av3nox2017", "av3nox2018",
                  "av3nox2019","av3nox2020", "av5nox2010", "av5nox2011", "av5nox2012", "av5nox2013", "av5nox2014",
                  "av5nox2015", "av5nox2016", "av5nox2017", "av5nox2018", "av5nox2019","av5nox2020","pm102010",
                  "pm102011", "pm102012", "pm102013", "pm102014", "pm102015", "pm102016", "pm102017",
                  "pm102018", "pm102019", "pm102020","av3pm102010", "av3pm102011", "av3pm102012", "av3pm102013",
                  "av3pm102014", "av3pm102015", "av3pm102016", "av3pm102017", "av3pm102018", "av3pm102019","av3pm102020",
                  "av5pm102010", "av5pm102011", "av5pm102012", "av5pm102013", "av5pm102014", "av5pm102015",
                  "av5pm102016", "av5pm102017", "av5pm102018", "av5pm102019", "av5pm102020","pm252010", "pm252011",
                  "pm252012", "pm252013", "pm252014", "pm252015", "pm252016", "pm252017", "pm252018",
                  "pm252019","pm252020","av3pm252010", "av3pm252011", "av3pm252012", "av3pm252013", "av3pm252014",
                  "av3pm252015", "av3pm252016", "av3pm252017", "av3pm252018", "av3pm252019", "av3pm252020","av5pm252010",
                  "av5pm252011", "av5pm252012", "av5pm252013", "av5pm252014", "av5pm252015", "av5pm252016",
                  "av5pm252017", "av5pm252018", "av5pm252019","av5pm252020", "so22010", "so22011", "so22012", "so22013",
                  "so22014", "so22015", "so22016", "so22017", "so22018", "so22019", "so22020","av3so22010", "av3so22011",
                  "av3so22012", "av3so22013", "av3so22014", "av3so22015", "av3so22016", "av3so22017",
                  "av3so22018", "av3so22019","av3so22020", "av5so22010", "av5so22011", "av5so22012", "av5so22013",
                  "av5so22014", "av5so22015", "av5so22016", "av5so22017", "av5so22018", "av5so22019","av5so22020",
                  "age","birth", "gender", "ethnic", "urban", "generalhappiness", "BMI", "education", "waisthipratio",
                  "homesmoke", "outsidesmoke", "smokingstatus", "solidfuelcooking", "alcoholintake",
                  "cookedvegetable", "rawvegetable", "freshfruit", "processedmeat", "oilyfish", "hotdrinktemp",
                  "MET", "sittime", "familyhistory","income","histology","walk","moderateactivity","vigorousactivity","centre","23104-0.0")]

#Converting data to person-years format is a time-consuming step.#
library(parallel)
remove(data)
table(data1$endtime)

num_chunks <- 20
chunk_size <- nrow(data1) / num_chunks
process_chunk <- function(chunk) {
  total_rows <- sum(chunk$endtime - chunk$startime + 1)
 data2 <- data.frame(eid = integer(total_rows),
                      time = integer(total_rows),
                      ectime = integer(total_rows),
                      bz = integer(total_rows),
                      av3bz = integer(total_rows),
                      av5bz = integer(total_rows),
                      dgt1 = integer(total_rows),
                      av3dgt1 = integer(total_rows),
                      av5dgt1 = integer(total_rows),
                      no2 = integer(total_rows),
                      av3no2 = integer(total_rows),
                      av5no2 = integer(total_rows),
                      nox = integer(total_rows),
                      av3nox = integer(total_rows),
                      av5nox = integer(total_rows),
                      pm10 = integer(total_rows),
                      av3pm10 = integer(total_rows),
                      av5pm10 = integer(total_rows),
                      pm25 = integer(total_rows),
                      av3pm25 = integer(total_rows),
                      av5pm25 = integer(total_rows),
                      so2 = integer(total_rows),
                      av3so2 = integer(total_rows),
                      av5so2 = integer(total_rows),
                      age = integer(total_rows),
                      birth = integer(total_rows),
                      gender = integer(total_rows),
                      ethnic = integer(total_rows),
                      urban = integer(total_rows),
                      generalhappiness = integer(total_rows),
                      BMI = integer(total_rows),
                      education = integer(total_rows),
                      waisthipratio = integer(total_rows),
                      homesmoke = integer(total_rows),
                      outsidesmoke = integer(total_rows),
                      smokingstatus = integer(total_rows),
                      solidfuelcooking = integer(total_rows),
                      alcoholintake = integer(total_rows),
                      cookedvegetable = integer(total_rows),
                      rawvegetable = integer(total_rows),
                      freshfruit = integer(total_rows),
                      processedmeat = integer(total_rows),
                      oilyfish = integer(total_rows),
                      hotdrinktemp = integer(total_rows),
                      MET = integer(total_rows),
                      sittime = integer(total_rows),
                      familyhistory = integer(total_rows),
                      income = integer(total_rows),
                      histology = integer(total_rows),
                      walk = integer(total_rows),
                      moderateactivity = integer(total_rows),
                      vigorousactivity = integer(total_rows),
                      centre = integer(total_rows),
                      locationchange = integer(total_rows))
  
  current_row <- 1
  for (i in 1:nrow(chunk)) {
    eid <- chunk$eid[i]
    ectime <- chunk$ectime[i]
    age <- chunk$age[i]
    birth <- chunk$birth[i]
    gender <- chunk$gender[i]
    ethnic <- chunk$ethnic[i]
    urban <- chunk$urban[i]
    generalhappiness <- chunk$generalhappiness[i]
    BMI <- chunk$BMI[i]
    education <- chunk$education[i]
    waisthipratio <- chunk$waisthipratio[i]
    homesmoke <- chunk$homesmoke[i]
    outsidesmoke <- chunk$outsidesmoke[i]
    smokingstatus <- chunk$smokingstatus[i]
    solidfuelcooking <- chunk$solidfuelcooking[i]
    alcoholintake <- chunk$alcoholintake[i]
    cookedvegetable <- chunk$cookedvegetable[i]
    rawvegetable <- chunk$rawvegetable[i]
    freshfruit <- chunk$freshfruit[i]
    processedmeat <- chunk$processedmeat[i]
    oilyfish <- chunk$oilyfish[i]
    hotdrinktemp <- chunk$hotdrinktemp[i]
    MET <- chunk$MET[i]
    sittime <- chunk$sittime[i]
    familyhistory <- chunk$familyhistory[i]
    income <- chunk$income[i]
    histology <- chunk$histology[i]
    walk <- chunk$walk[i]
    moderateactivity <- chunk$moderateactivity[i]
    vigorousactivity <- chunk$vigorousactivity[i]
    centre <- chunk$centre[i]
    locationchange <- chunk$locationchange[i]
    start_year <- chunk$startime[i]
    end_year <- chunk$endtime[i]
    
    for (year in start_year:end_year) {
      bz_col <- paste0("bz", year)
      av3bz_col <- paste0("av3bz", year)
      av5bz_col <- paste0("av5bz", year)
      dgt1_col <- paste0("dgt1", year)
      av3dgt1_col <- paste0("av3dgt1", year)
      av5dgt1_col <- paste0("av5dgt1", year)
      no2_col <- paste0("no2", year)
      av3no2_col <- paste0("av3no2", year)
      av5no2_col <- paste0("av5no2", year)
      nox_col <- paste0("nox", year)
      av3nox_col <- paste0("av3nox", year)
      av5nox_col <- paste0("av5nox", year)
      pm10_col <- paste0("pm10", year)
      av3pm10_col <- paste0("av3pm10", year)
      av5pm10_col <- paste0("av5pm10", year)
      pm25_col <- paste0("pm25", year)
      av3pm25_col <- paste0("av3pm25", year)
      av5pm25_col <- paste0("av5pm25", year)
      so2_col <- paste0("so2", year)
      av3so2_col <- paste0("av3so2", year)
      av5so2_col <- paste0("av5so2", year)
      
      bz <- chunk[[bz_col]][i]
      av3bz <- chunk[[av3bz_col]][i]
      av5bz <- chunk[[av5bz_col]][i]
      dgt1 <- chunk[[dgt1_col]][i]
      av3dgt1 <- chunk[[av3dgt1_col]][i]
      av5dgt1 <- chunk[[av5dgt1_col]][i]
      no2 <- chunk[[no2_col]][i]
      av3no2 <- chunk[[av3no2_col]][i]
      av5no2 <- chunk[[av5no2_col]][i]
      nox <- chunk[[nox_col]][i]
      av3nox <- chunk[[av3nox_col]][i]
      av5nox <- chunk[[av5nox_col]][i]
      pm10 <- chunk[[pm10_col]][i]
      av3pm10 <- chunk[[av3pm10_col]][i]
      av5pm10 <- chunk[[av5pm10_col]][i]
      pm25 <- chunk[[pm25_col]][i]
      av3pm25 <- chunk[[av3pm25_col]][i]
      av5pm25 <- chunk[[av5pm25_col]][i]
      so2 <- chunk[[so2_col]][i]
      av3so2 <- chunk[[av3so2_col]][i]
      av5so2 <- chunk[[av5so2_col]][i]
      
      data2[current_row, ] <- c(eid, year, ectime,bz,av3bz,av5bz,dgt1,av3dgt1,av5dgt1,no2,av3no2,
                                av5no2,nox,av3nox,av5nox,pm10,av3pm10,av5pm10,pm25,av3pm25,
                                av5pm25,so2,av3so2,av5so2,age,birth,gender,ethnic,urban,
                                generalhappiness,BMI,education,waisthipratio,homesmoke,
                                outsidesmoke,smokingstatus,solidfuelcooking,alcoholintake,
                                cookedvegetable,rawvegetable,freshfruit,processedmeat,
                                oilyfish,hotdrinktemp,MET,sittime,familyhistory,income,
                                histology,walk,moderateactivity,vigorousactivity,centre,locationchange)
      current_row <- current_row + 1
    }
  }
  return(data2)
}
chunks <- split(data1, cut(1:nrow(data1), num_chunks, labels = FALSE))
cl <- makeCluster(num_chunks)  # 创建并行计算集群
clusterExport(cl, c("process_chunk"))  # 导出函数
result_list <- parLapply(cl, chunks, process_chunk)  # 并行处理
stopCluster(cl)  # 停止并行计算集群
data2 <- do.call(rbind, result_list)
####Add some variables.##########
ukbsup4[is.na(ukbsup4)] <- -1
ukbsup4$gastricreflux <- 0
variable_names <- grep("^20002-0.", names(ukbsup4), value = TRUE)
ukbsup4$gastricreflux <- apply(ukbsup4[variable_names] == 1138, 1, function(x) any(x, na.rm = TRUE))
ukbsup4$gastricreflux <- as.integer(ukbsup4$gastricreflux)
table(ukbsup4$gastricreflux,useNA = "ifany")
ukbsup4$oesophagitis <- 0
ukbsup4$oesophagitis <- apply(ukbsup4[variable_names] == 1139, 1, function(x) any(x, na.rm = TRUE))
ukbsup4$oesophagitis <- as.integer(ukbsup4$oesophagitis)
ukbsup4$stricture <- 0
ukbsup4$stricture <- apply(ukbsup4[variable_names] == 1140, 1, function(x) any(x, na.rm = TRUE))
ukbsup4$stricture <- as.integer(ukbsup4$stricture)
ukbsup4$varicies <- 0
ukbsup4$varicies <- apply(ukbsup4[variable_names] == 1141, 1, function(x) any(x, na.rm = TRUE))
ukbsup4$varicies <- as.integer(ukbsup4$varicies)
table(ukbsup4$stricture)
table(ukbsup4$varicies)
table(ukbsup4$oesophagitis)
ukbsup4_selected <- ukbsup4 %>%
  select(eid, gastricreflux, stricture, varicies, oesophagitis)
merged_data <- left_join(data2, ukbsup4_selected, by = "eid")
filtered_merged_data <- merged_data %>%
  filter(!is.na(time))
remove(merged_data)
data2<-filtered_merged_data
remove(variable_names)
table(data2$gastricreflux,useNA = "ifany")

# the outcome variable ec for Cox regression#
table(data2$ec, useNA = "ifany")
data2$ec <- ifelse(is.na(data2$ectime) | data2$time != data2$ectime, 0, 1)

#Exclude participants with a change in residential address exceeding 10 km.#
data2 <- data2[data2$locationchange <= 10000, ]

#Organize the covariates for person-years data#
##年龄分组#
data2$agetimevary <- data2$time - data2$birth
cat(q25_agetimevary,q50_agetimevary,q75_agetimevary)
q25_agetimevary<- quantile(data2$agetimevary, probs = 0.25, na.rm = TRUE)
q50_agetimevary<- quantile(data2$agetimevary, probs = 0.50, na.rm = TRUE)
q75_agetimevary<- quantile(data2$agetimevary, probs = 0.75, na.rm = TRUE)
data2$agegroup <- cut(data2$agetimevary,
                      breaks = c(min(data2$agetimevary),q25_agetimevary, q50_agetimevary,q75_agetimevary, max(data2$agetimevary)),
                      labels = c(0,1,2,3),
                      include.lowest = TRUE)
#pm10分组#
q25 <- quantile(data2$pm10, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$pm10, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$pm10, probs = 0.75, na.rm = TRUE)
data2$pm10group <- cut(data2$pm10,
                       breaks = c(-Inf, q25, q50, q75, Inf),
                       labels = c(0, 1, 2, 3),
                       include.lowest = TRUE)
data2$pm10group[is.na(data2$pm10)] <- NA
#av3pm10group#
q25 <- quantile(data2$av3pm10, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$av3pm10, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$av3pm10, probs = 0.75, na.rm = TRUE)
data2$av3pm10group <- cut(data2$av3pm10,
                          breaks = c(-Inf, q25, q50, q75, Inf),
                          labels = c(0, 1, 2, 3),
                          include.lowest = TRUE)
data2$av3pm10group[is.na(data2$av3pm10)] <- NA
#av5pm10group#
q25 <- quantile(data2$av5pm10, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$av5pm10, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$av5pm10, probs = 0.75, na.rm = TRUE)
data2$av5pm10group <- cut(data2$av5pm10,
                          breaks = c(-Inf, q25, q50, q75, Inf),
                          labels = c(0, 1, 2, 3),
                          include.lowest = TRUE)
data2$av5pm10group[is.na(data2$av5pm10)] <- NA
#pm25group#
q25 <- quantile(data2$pm25, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$pm25, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$pm25, probs = 0.75, na.rm = TRUE)
data2$pm25group <- cut(data2$pm25,
                       breaks = c(-Inf, q25, q50, q75, Inf),
                       labels = c(0, 1, 2, 3),
                       include.lowest = TRUE)
data2$pm25group[is.na(data2$pm25)] <- NA
#av3pm25group#
q25 <- quantile(data2$av3pm25, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$av3pm25, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$av3pm25, probs = 0.75, na.rm = TRUE)
data2$av3pm25group <- cut(data2$av3pm25,
                          breaks = c(-Inf, q25, q50, q75, Inf),
                          labels = c(0, 1, 2, 3),
                          include.lowest = TRUE)
data2$av3pm25group[is.na(data2$av3pm25)] <- NA
#av5pm25group#
q25 <- quantile(data2$av5pm25, probs = 0.25, na.rm = TRUE)
q50 <- quantile(data2$av5pm25, probs = 0.50, na.rm = TRUE)
q75 <- quantile(data2$av5pm25, probs = 0.75, na.rm = TRUE)
data2$av5pm25group <- cut(data2$av5pm25,
                          breaks = c(-Inf, q25, q50, q75, Inf),
                          labels = c(0, 1, 2, 3),
                          include.lowest = TRUE)
data2$av5pm25group[is.na(data2$av5pm25)] <- NA
###
data2$av5pm25group2 <- cut(data2$av5pm25,
                          breaks = c(-Inf, q50, Inf),
                          labels = c(0, 1),
                          include.lowest = TRUE)
data2$av5pm25group2[is.na(data2$av5pm25)] <- NA
#bzgroup#
# 创建 bzgroup 变量
q25_bz <- quantile(data2$bz, probs = 0.25, na.rm = TRUE)
q50_bz <- quantile(data2$bz, probs = 0.50, na.rm = TRUE)
q75_bz <- quantile(data2$bz, probs = 0.75, na.rm = TRUE)
data2$bzgroup <- cut(data2$bz,
                     breaks = c(-Inf, q25_bz, q50_bz, q75_bz, Inf),
                     labels = c(0, 1, 2, 3),
                     include.lowest = TRUE)
data2$bzgroup[is.na(data2$bz)] <- NA
# #av3bzgroup#创建 av3bzgroup 变量
q25_av3bz <- quantile(data2$av3bz, probs = 0.25, na.rm = TRUE)
q50_av3bz <- quantile(data2$av3bz, probs = 0.50, na.rm = TRUE)
q75_av3bz <- quantile(data2$av3bz, probs = 0.75, na.rm = TRUE)
data2$av3bzgroup <- cut(data2$av3bz,
                        breaks = c(-Inf, q25_av3bz, q50_av3bz, q75_av3bz, Inf),
                        labels = c(0, 1, 2, 3),
                        include.lowest = TRUE)
data2$av3bzgroup[is.na(data2$av3bz)] <- NA

#av5bzgroup#
q25_av5bz <- quantile(data2$av5bz, probs = 0.25, na.rm = TRUE)
q50_av5bz <- quantile(data2$av5bz, probs = 0.50, na.rm = TRUE)
q75_av5bz <- quantile(data2$av5bz, probs = 0.75, na.rm = TRUE)
data2$av5bzgroup <- cut(data2$av5bz,
                        breaks = c(-Inf, q25_av5bz, q50_av5bz, q75_av5bz, Inf),
                        labels = c(0, 1, 2, 3),
                        include.lowest = TRUE)
cat(q25_av5bz,q50_av5bz,q75_av5bz)
data2$av5bzgroup[is.na(data2$av5bz)] <- NA
#no2group#
q25_no2 <- quantile(data2$no2, probs = 0.25, na.rm = TRUE)
q50_no2 <- quantile(data2$no2, probs = 0.50, na.rm = TRUE)
q75_no2 <- quantile(data2$no2, probs = 0.75, na.rm = TRUE)
data2$no2group <- cut(data2$no2,
                      breaks = c(-Inf, q25_no2, q50_no2, q75_no2, Inf),
                      labels = c(0, 1, 2, 3),
                      include.lowest = TRUE)
data2$no2group[is.na(data2$no2)] <- NA
#av3no2group#
q25_av3no2 <- quantile(data2$av3no2, probs = 0.25, na.rm = TRUE)
q50_av3no2 <- quantile(data2$av3no2, probs = 0.50, na.rm = TRUE)
q75_av3no2 <- quantile(data2$av3no2, probs = 0.75, na.rm = TRUE)
data2$av3no2group <- cut(data2$av3no2,
                         breaks = c(-Inf, q25_av3no2, q50_av3no2, q75_av3no2, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av3no2group[is.na(data2$av3no2)] <- NA
#av5no2group#
q25_av5no2 <- quantile(data2$av5no2, probs = 0.25, na.rm = TRUE)
q50_av5no2 <- quantile(data2$av5no2, probs = 0.50, na.rm = TRUE)
q75_av5no2 <- quantile(data2$av5no2, probs = 0.75, na.rm = TRUE)
data2$av5no2group <- cut(data2$av5no2,
                         breaks = c(-Inf, q25_av5no2, q50_av5no2, q75_av5no2, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av5no2group[is.na(data2$av5no2)] <- NA
#nox#
q25_nox <- quantile(data2$nox, probs = 0.25, na.rm = TRUE)
q50_nox <- quantile(data2$nox, probs = 0.50, na.rm = TRUE)
q75_nox <- quantile(data2$nox, probs = 0.75, na.rm = TRUE)
data2$noxgroup <- cut(data2$nox,
                      breaks = c(-Inf, q25_nox, q50_nox, q75_nox, Inf),
                      labels = c(0, 1, 2, 3),
                      include.lowest = TRUE)
data2$noxgroup[is.na(data2$nox)] <- NA
#av3nox#
q25_av3nox <- quantile(data2$av3nox, probs = 0.25, na.rm = TRUE)
q50_av3nox <- quantile(data2$av3nox, probs = 0.50, na.rm = TRUE)
q75_av3nox <- quantile(data2$av3nox, probs = 0.75, na.rm = TRUE)
data2$av3noxgroup <- cut(data2$av3nox,
                         breaks = c(-Inf, q25_av3nox, q50_av3nox, q75_av3nox, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av3noxgroup[is.na(data2$av3nox)] <- NA
#av5nox#
q25_av5nox <- quantile(data2$av5nox, probs = 0.25, na.rm = TRUE)
q50_av5nox <- quantile(data2$av5nox, probs = 0.50, na.rm = TRUE)
q75_av5nox <- quantile(data2$av5nox, probs = 0.75, na.rm = TRUE)
data2$av5noxgroup <- cut(data2$av5nox,
                         breaks = c(-Inf, q25_av5nox, q50_av5nox, q75_av5nox, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av5noxgroup[is.na(data2$av5nox)] <- NA
#so2#
q25_so2 <- quantile(data2$so2, probs = 0.25, na.rm = TRUE)
q50_so2 <- quantile(data2$so2, probs = 0.50, na.rm = TRUE)
q75_so2 <- quantile(data2$so2, probs = 0.75, na.rm = TRUE)
data2$so2group <- cut(data2$so2,
                      breaks = c(-Inf, q25_so2, q50_so2, q75_so2, Inf),
                      labels = c(0, 1, 2, 3),
                      include.lowest = TRUE)
data2$so2group[is.na(data2$so2)] <- NA
#av3so2#
q25_av3so2 <- quantile(data2$av3so2, probs = 0.25, na.rm = TRUE)
q50_av3so2 <- quantile(data2$av3so2, probs = 0.50, na.rm = TRUE)
q75_av3so2 <- quantile(data2$av3so2, probs = 0.75, na.rm = TRUE)
data2$av3so2group <- cut(data2$av3so2,
                         breaks = c(-Inf, q25_av3so2, q50_av3so2, q75_av3so2, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av3so2group[is.na(data2$av3so2)] <- NA
#av5so2#
q25_av5so2 <- quantile(data2$av5so2, probs = 0.25, na.rm = TRUE)
q50_av5so2 <- quantile(data2$av5so2, probs = 0.50, na.rm = TRUE)
q75_av5so2 <- quantile(data2$av5so2, probs = 0.75, na.rm = TRUE)
data2$av5so2group <- cut(data2$av5so2,
                         breaks = c(-Inf, q25_av5so2, q50_av5so2, q75_av5so2, Inf),
                         labels = c(0, 1, 2, 3),
                         include.lowest = TRUE)
data2$av5so2group[is.na(data2$av5so2)] <- NA
#waisthipratiogroup#
q50_waisthipratio <- quantile(data2$waisthipratio, probs = 0.50, na.rm = TRUE)
data2$waisthipratiogroup <- ifelse(is.na(data2$waisthipratio), 9,
                                   cut(data2$waisthipratio,
                                       breaks = c(-Inf,  q50_waisthipratio, Inf),
                                       labels = c(1, 2),
                                       include.lowest = TRUE))
data2$waisthipratiogroup <- factor(data2$waisthipratiogroup, levels = c(1, 2, 9))
table(data2$waisthipratiogroup)

#homesmoke#
table(data2$homesmoke,useNA = "ifany")
#outsidesmoke#
table(data2$outsidesmoke,useNA = "ifany")
#secondhandsomke#
data2$secondhandsomke <- ifelse(is.na(data2$homesmoke) & is.na(data2$outsidesmoke), NA, 
                                ifelse(data2$homesmoke + data2$outsidesmoke >= 1, 2, 1))
data2$secondhandsomke <- ifelse(is.na(data2$secondhandsomke), 9, data2$secondhandsomke)
table(data2$secondhandsomke,useNA = "ifany")
data2$secondhandsomke <- factor(data2$secondhandsomke, levels = c(1, 2, 9))

#alcoholintake#
table(data2$alcoholintake,useNA = "ifany")
data2$alcoholintakegroup <- ifelse(is.na(data2$alcoholintake), 9,
                                cut(data2$alcoholintake,
                                breaks = c(-Inf,2,4, Inf),
                                labels = c(1,2,3),
                                include.lowest = TRUE))
data2$alcoholintakegroup <- factor(data2$alcoholintakegroup, levels = c(1, 2,3, 9))
table(data2$alcoholintakegroup,useNA = "ifany")



data2$alcoholintakegroup2 <- ifelse(is.na(data2$alcoholintake), 9,
                               cut(data2$alcoholintake,
                                breaks = c(-Inf ,2, Inf),
                                labels = c(1,2),
                                include.lowest = TRUE))
table(data2$alcoholintakegroup2,useNA = "ifany")
data2$alcoholintakegroup2 <- factor(data2$alcoholintakegroup2, levels = c(1, 2, 9))
#cookedvegetable#
table(data2$cookedvegetable,useNA = "ifany")
table(data2$cookedvegetablegroup,useNA = "ifany")
q50_cookedvegetable <- quantile(data2$cookedvegetable, probs = 0.50, na.rm = TRUE)
data2$cookedvegetablegroup <- ifelse(is.na(data2$cookedvegetable), 9,
                                  cut(data2$cookedvegetable,
                                  breaks = c(-Inf ,q50_cookedvegetable, Inf),
                                  labels = c(1, 2),
                                  include.lowest = TRUE))
data2$cookedvegetablegroup <- factor(data2$cookedvegetablegroup, levels = c(1, 2, 9))
#rawvegetable#
table(data2$rawvegetable,useNA = "ifany")
q50_rawvegetable <- quantile(data2$rawvegetable, probs = 0.50, na.rm = TRUE)
data2$rawvegetablegroup <- ifelse(is.na(data2$rawvegetable), 9,
                               cut(data2$rawvegetable,
                               breaks = c(-Inf ,q50_rawvegetable, Inf),
                               labels = c(1,2),
                               include.lowest = TRUE))
data2$rawvegetablegroup <- factor(data2$rawvegetablegroup, levels = c(1, 2, 9))
#freshfruit#
table(data2$freshfruitgroup,useNA = "ifany")
q50_freshfruit <- quantile(data2$freshfruit, probs = 0.50, na.rm = TRUE)
data2$freshfruitgroup <- ifelse(is.na(data2$freshfruit), 9,
                             cut(data2$freshfruit,
                             breaks = c(-Inf ,q50_freshfruit, Inf),
                             labels = c(1,2),
                             include.lowest = TRUE))
data2$freshfruitgroup <- factor(data2$freshfruitgroup, levels = c(1, 2, 9))
#processedmeat#
table(data2$processedmeat,useNA = "ifany")
q50_processedmeat <- quantile(data2$processedmeat, probs = 0.50, na.rm = TRUE)
data2$processedmeatgroup <- ifelse(is.na(data2$processedmeat), 9,
                                cut(data2$processedmeat,
                                breaks = c(-Inf ,q50_processedmeat, Inf),
                                labels = c(1,2),
                                include.lowest = TRUE))
data2$processedmeatgroup <- factor(data2$processedmeatgroup, levels = c(1, 2, 9))
table(data2$processedmeatgroup,useNA = "ifany")
#oilyfish#
table(data2$oilyfish,useNA = "ifany")
data2$oilyfishgroup <- ifelse(is.na(data2$oilyfish), 9,
                           cut(data2$oilyfish,
                           breaks = c(-Inf ,0,1,2, Inf),
                           labels = c(1,2,3,4),
                           include.lowest = TRUE))
data2$oilyfishgroup <- factor(data2$oilyfishgroup, levels = c(1, 2,3,4, 9))
table(data2$oilyfishgroup,useNA = "ifany")
#hotdrinktemp#
table(data2$hotdrinktemp,useNA = "ifany")
data2$hotdrinktempgroup<-data2$hotdrinktemp+1
table(data2$hotdrinktempgroup,useNA = "ifany")
data2$hotdrinktempgroup <- replace(data2$hotdrinktempgroup, is.na(data2$hotdrinktempgroup), 9)
data2$hotdrinktempgroup <- factor(data2$hotdrinktempgroup, levels = c(1, 2,3,4,9))

#MET#
table(data2$MET,useNA = "ifany")
q25_MET <- quantile(data2$MET, probs = 0.25, na.rm = TRUE)
q50_MET <- quantile(data2$MET, probs = 0.50, na.rm = TRUE)
q75_MET <- quantile(data2$MET, probs = 0.75, na.rm = TRUE)
data2$METgroup <- ifelse(is.na(data2$MET), 9,
                      cut(data2$MET,
                      breaks = c(-Inf, q25_MET, q50_MET, q75_MET, Inf),
                      labels = c(1,2,3,4),
                      include.lowest = TRUE))
data2$METgroup <- factor(data2$METgroup, levels = c(1, 2,3,4,9))
table(data2$METgroup,useNA = "ifany")
#moderateactivity#
table(data2$moderateactivity,useNA = "ifany")
#sittime#
table(data2$sittime,useNA = "ifany")
q25_sittime <- quantile(data2$sittime, probs = 0.25, na.rm = TRUE)
q50_sittime <- quantile(data2$sittime, probs = 0.50, na.rm = TRUE)
q75_sittime <- quantile(data2$sittime, probs = 0.75, na.rm = TRUE)
data2$sittimegroup <- ifelse(is.na(data2$sittime), 9,
                         cut(data2$sittime,
                          breaks = c(-Inf, q25_sittime, q50_sittime, q75_sittime, Inf),
                          labels = c( 1,2,3,4),
                          include.lowest = TRUE))
data2$sittimegroup <- factor(data2$sittimegroup, levels = c(1, 2,3,4,9))
table(data2$sittimegroup,useNA = "ifany")
#income#
table(data2$income,useNA = "ifany")
data2$incomegroup <- ifelse(is.na(data2$income), 9,
                             cut(data2$income,
                                 breaks = c(-Inf, 2, Inf),
                                 labels = c( 1,2),
                                 include.lowest = TRUE))
data2$incomegroup <- factor(data2$incomegroup, levels = c(1,2,9))
table(data2$incomegroup,useNA = "ifany")

#BMI#
data$BMIgroup<-data$BMI
data$BMIgroup <- as.numeric(data$BMIgroup)
data$BMIgroup[is.na(data$BMIgroup)] <- 9
data$BMIgroup <- factor(data$BMIgroup, levels = c(1, 2,3,4,9)) 
table(data$BMIgroup,useNA = "ifany")
table(data$BMIgroup)

#education#
table(data2$education,useNA = "ifany")
data2$educationgroup<-data2$education+1
data2$educationgroup <- replace(data2$educationgroup, is.na(data2$educationgroup), 9)
data2$educationgroup <- factor(data2$educationgroup , levels = c(1, 2, 9))
table(data2$educationgroup,useNA = "ifany")
#urban#
table(data2$urban,useNA = "ifany")
data2$urbangroup<-data2$urban+1
data2$urbangroup <- replace(data2$urbangroup, is.na(data2$urbangroup), 9)
data2$urbangroup <- factor(data2$urbangroup , levels = c(1,2,3,9))
table(data2$urbangroup,useNA = "ifany")
#smokingstatus#
table(data2$smokingstatus,useNA = "ifany")
data2$smokingstatusgroup<-data2$smokingstatus+1
data2$smokingstatusgroup <- replace(data2$smokingstatusgroup, is.na(data2$smokingstatusgroup), 9)
data2$smokingstatusgroup <- factor(data2$smokingstatusgroup , levels = c(1,2,3,9))
table(data2$smokingstatusgroup,useNA = "ifany")
#solidfuelcooking#
table(data2$solidfuelcooking,useNA = "ifany")
data2$solidfuelcookinggroup<-data2$solidfuelcooking+1
data2$solidfuelcookinggroup <- replace(data2$solidfuelcookinggroup, is.na(data2$solidfuelcookinggroup), 9)
data2$solidfuelcookinggroup <- factor(data2$solidfuelcookinggroup , levels = c(1,2,9))
table(data2$solidfuelcookinggroup,useNA = "ifany")
##
######完成生存分析前整理,savedata#######################
save(data2, file="data2整理完成.RData")
save(data2, file="data2.RData")



