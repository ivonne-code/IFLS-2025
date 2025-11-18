
setwd("C://Users//mcbre//OneDrive - University of Illinois - Urbana//DATA LAB//DATA")

library(readr)
library(tidyr)
library(dplyr)
library(stringr)

#####LOAD COMBINED DATASET 

comb_dt <- read.csv("combined_dataset_file.csv")
## COMB_DT START WITH 21381 obs.

##### CLEAN SELECTED VARIABLES

##created data set with transferable variable
reg_dt <- comb_dt %>%
  select(pidlink, w5_rt, w4_rt, w5_tp, w4_tp, w5_age)


###CREATE RT (1/0) DUMMY

##calculate change in RT
comb_dt$rt_change = comb_dt$w4_rt - comb_dt$w5_rt

reg_dt <- reg_dt %>%
  mutate(more_rt = case_when(
    comb_dt$rt_change > 0 ~ "1",
    comb_dt$rt_change <= 0 ~ "0",
    
    TRUE ~ NA_character_
  )) %>%
  mutate(less_rt = case_when(
    comb_dt$rt_change >= 0 ~ "0",
    comb_dt$rt_change < 0 ~ "1",
    
    TRUE ~ NA_character_
  ))

###CREATE TP (1/0) DUMMY

##calculate change in TP
comb_dt$tp_change = comb_dt$w4_tp - comb_dt$w5_tp

reg_dt <- reg_dt %>%
  mutate(more_tp = case_when(
    comb_dt$tp_change >= 0 ~ "0",
    comb_dt$tp_change < 0 ~ "1",
    
    TRUE ~ NA_character_
  )) %>%
  mutate(less_tp = case_when(
    comb_dt$tp_change > 0 ~ "1",
    comb_dt$tp_change <= 0 ~ "0",
    
    TRUE ~ NA_character_
  ))


###SIMPLIFY EMPLOYMENT STATUS VARIABLE (create 3 dummy variables)

###combines tk16c1, re02, re08
## 1-employed, 2-unemployed, 3-retired

comb_dt <- comb_dt %>%
  mutate(emply_4 = case_when(
    
    w4_re02 == 1 ~ "1",
    w4_re08 == 1 ~ "3",
    w4_re08 == 2 ~ "2",
    w4_re08 == 3 ~ "2",
    w4_re02 == 3 ~ "2",
    w4_tk16c1 < 9  ~ "1",

    TRUE ~ NA_character_
  ))

comb_dt <- comb_dt %>%
  mutate(emply_5 = case_when(
    w5_re2 == "1:Working" ~ "1",
    w5_re8 == "1:Retired" ~ "3",
    w5_re8 == "2:Partly retired" ~ "2",
    w5_re8 == "3:Not retired" ~ "2",
    w5_re2 == "3:Not working" ~ "2",
    w5_tk16c1 %in% c("1:Very satisfied", "2:Satisfied", 
                   "3:Unsatisfied", "4:Very unsatisfied") ~ "1",
    
    TRUE ~ NA_character_
  ))


##create dummy variables

#4th wave
comb_dt <- comb_dt %>% 
  mutate(emp4 = case_when(
    comb_dt$emply_4 == "1" ~ "1",
    comb_dt$emply_4 == "2" ~ "0",
    comb_dt$emply_4 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%

  mutate(unemp4 = case_when(
    comb_dt$emply_4 == "1" ~ "0",
    comb_dt$emply_4 == "2" ~ "1",
    comb_dt$emply_4 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%

  mutate(ret4 = case_when(
    comb_dt$emply_4 == "1" ~ "0",
    comb_dt$emply_4 == "2" ~ "0",
    comb_dt$emply_4 == "3" ~ "1",
    TRUE ~ NA_character_
    
  ))


#5th wave
comb_dt <- comb_dt %>% 
  mutate(emp5 = case_when(
    comb_dt$emply_5 == "1" ~ "1",
    comb_dt$emply_5 == "2" ~ "0",
    comb_dt$emply_5 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(unemp5 = case_when(
    comb_dt$emply_5 == "1" ~ "0",
    comb_dt$emply_5 == "2" ~ "1",
    comb_dt$emply_5 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(ret5 = case_when(
    comb_dt$emply_5 == "1" ~ "0",
    comb_dt$emply_5 == "2" ~ "0",
    comb_dt$emply_5 == "3" ~ "1",
    TRUE ~ NA_character_
    
  )) 

### CREATE DUMMY REG VARIABLES FOR CHANGE IN EMPLOYMENT  STATUS (3 dummy variables)
## 1 - changed to || 0 - no change/not changed to
# regression variables [emp, unemp, ret]

reg_dt <- reg_dt %>% 
  mutate(emp = case_when(
    ((comb_dt$emp5 == "1") & (comb_dt$emp4 == "1")) ~ "0",
    comb_dt$emp5 == "1" ~ "1",
    comb_dt$emp5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(unemp = case_when(
    ((comb_dt$unemp5 == "1") & (comb_dt$unemp4 == "1")) ~ "0",
    comb_dt$unemp5 == "1" ~ "1",
    comb_dt$unemp5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(ret = case_when(
    ((comb_dt$ret5 == "1") & (comb_dt$ret4 == "1")) ~ "0",
    comb_dt$ret5 == "1" ~ "1",
    comb_dt$ret5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) 




###SIMPLIFY MARRIAGE VARIABLE (create 3 dummy variables)
## 1-single/widowed, 2-married/cohabitate, 3-seperated/divorced

comb_dt <- comb_dt %>% 
  mutate(mar4 = case_when(
    
    comb_dt$w4_marstat %in% c(1,5) ~ "1",
    comb_dt$w4_marstat %in% c(2,6)  ~ "2",
    comb_dt$w4_marstat %in% c(3,4) ~ "3",
    
    TRUE ~ NA_character_
  ))

comb_dt <- comb_dt %>% 
  mutate(mar5 = case_when(
    
    comb_dt$w5_marstat %in% c("1:Not yet married", "5:Widowed") ~ "1",
    comb_dt$w5_marstat %in% c("2:Married", "6:Cohabitate")  ~ "2",
    comb_dt$w5_marstat %in% c("3:Separated", "4:Divorced ") ~ "3",
    
    TRUE ~ NA_character_
  ))

## create dummy variables
#4th wave
comb_dt <- comb_dt %>% 
  mutate(sing4 = case_when(
    mar4 == "1" ~ "1",
    mar4 == "2" ~ "0",
    mar4 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(marr4 = case_when(
    mar4 == "1" ~ "0",
    mar4 == "2" ~ "1",
    mar4 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(sep4 = case_when(
    mar4 == "1" ~ "0",
    mar4 == "2" ~ "0",
    mar4 == "3" ~ "1",
    TRUE ~ NA_character_
    
  ))

#5th wave
comb_dt <- comb_dt %>% 
  mutate(sing5 = case_when(
    mar5 == "1" ~ "1",
    mar5 == "2" ~ "0",
    mar5 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(marr5 = case_when(
    mar5 == "1" ~ "0",
    mar5 == "2" ~ "1",
    mar5 == "3" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(sep5 = case_when(
    mar5 == "1" ~ "0",
    mar5 == "2" ~ "0",
    mar5 == "3" ~ "1",
    TRUE ~ NA_character_
    
  ))

### CREATE DUMMY REG VARIABLES FOR CHANGE IN MARRIAGE  STATUS (3 dummy variables)
## 1 - changed to || 0 - no change/not changed to
# regression variables [sing, marr, sep]

reg_dt <- reg_dt %>% 
  mutate(sing = case_when(
    ((comb_dt$sing5 == "1") & (comb_dt$sing4 == "1")) ~ "0",
    comb_dt$sing5 == "1" ~ "1",
    comb_dt$sing5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(marr = case_when(
    ((comb_dt$marr5 == "1") & (comb_dt$marr4 == "1")) ~ "0",
    comb_dt$marr5 == "1" ~ "1",
    comb_dt$marr5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) %>%
  
  mutate(sep = case_when(
    ((comb_dt$sep5 == "1") & (comb_dt$sep4 == "1")) ~ "0",
    comb_dt$sep5 == "1" ~ "1",
    comb_dt$sep5 == "0" ~ "0",
    TRUE ~ NA_character_
    
  )) 


###TURN GENDER INTO A 01 VARIABLE
## 0-male, 1-female

reg_dt <- reg_dt %>% 
  mutate(gender = case_when(
    
    comb_dt$w5_sex == "1:Male" ~ "0",
    comb_dt$w5_sex == "3:Female"  ~ "1",
    
    TRUE ~ NA_character_
  ))


###CHANGE EDUCATION LEVEL TO YEARS OF EDUCATION VARIABLE


extract_code <- function(x) {
  if (is.numeric(x)) {
    return(x)
  } else if (is.character(x)) {
    code <- sub(":.*", "", x)  # Get number before ":"
    return(as.numeric(code))
  } else {
    return(NA)
  }
}

comb_dt$w5_dl06_code <- sapply(comb_dt$w5_dl06, extract_code)
table(comb_dt$w5_dl06)
table(comb_dt$w5_dl06_code)

map_to_years <- function(code) {
  case_when(
    code == 2  ~ "6",   # Elementary
    code == 3  ~ "9",   # Junior high general
    code == 4  ~ "9",   # Junior high vocational
    code == 5  ~ "12",  # Senior high general
    code == 6  ~ "12",  # Senior high vocational
    code == 11 ~ "9",   # Adult education A
    code == 12 ~ "12",  # Adult education B
    code == 13 ~ "16",  # Open university
    code == 14 ~ "12",  # Pesantren
    code == 15 ~ "12",  # Adult education C
    code == 17 ~ "12",  # School for Disabled
    code == 60 ~ "15",  # College (D1-D3)
    code == 61 ~ "16",  # Bachelor (S1)
    code == 62 ~ "18",  # Master (S2)
    code == 63 ~ "21",  # Doctoral (S3)
    code == 72 ~ "6",   # Islamic Elementary
    code == 73 ~ "9",   # Islamic Junior High
    code == 74 ~ "12",  # Islamic Senior High
    code == 90 ~ "0",   # Kindergarten
    TRUE ~ NA_character_   # Other/Missing/Don't Know
  )
}

comb_dt$edu <- map_to_years(comb_dt$w5_dl06_code)

reg_dt <- reg_dt %>%
  mutate(edu = comb_dt$edu)

###REMOVE ANY ROWS WITH N/A VARIABLES

reg_dt <- reg_dt %>%
  drop_na()

###LIMIT AGE TO EVERYONE OLDER THAT 15 
reg_dt <- reg_dt %>%
  filter(w5_age >= 15)

###ENSURE ALL ARE NUMERIC
lapply(reg_dt, as.numeric)

###COUNT 
16426/21381

## WRITE TO FILE
write.csv(reg_dt, file = "regression_variables_dataset_file.csv", row.names = FALSE)



