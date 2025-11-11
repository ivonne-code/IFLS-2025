
setwd("C://Users//mcbre//OneDrive - University of Illinois - Urbana//DATA LAB//DATA")

library(readr)
library(tidyr)
library(dplyr)
library(stringr)

#####LOAD COMBINED DATASET 

comb_dt <- read.csv("combined_dataset_file.csv")




##### CLEAN SELECTED VARIABLES

##created dataset with transferable varaiable
reg_dt <- comb_dt %>%
  select(pidlink, w5_rt, w4_rt, w5_tp, w4_tp, w5_age, w4_age)



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
reg_dt <- reg_dt %>% 
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
reg_dt <- reg_dt %>% 
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



###SIMPLIFY MARRIAGE VARIABLE (create 3 dummy variables)
## 1-single/widowed, 2-married/cohabitate, 3-seperated/divorced

reg_dt <- reg_dt %>% 
  mutate(mar4 = case_when(
    
    comb_dt$w4_marstat %in% c(1,5) ~ "1",
    comb_dt$w4_marstat %in% c(2,6)  ~ "2",
    comb_dt$w4_marstat %in% c(3,4) ~ "3",
    
    TRUE ~ NA_character_
  ))

reg_dt <- reg_dt %>% 
  mutate(mar5 = case_when(
    
    comb_dt$w5_marstat %in% c("1:Not yet married", "5:Widowed") ~ "1",
    comb_dt$w5_marstat %in% c("2:Married", "6:Cohabitate")  ~ "2",
    comb_dt$w5_marstat %in% c("3:Separated", "4:Divorced ") ~ "3",
    
    TRUE ~ NA_character_
  ))

## create dummy variables
#4th wave
reg_dt <- reg_dt %>% 
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
reg_dt <- reg_dt %>% 
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

###TURN GENDER INTO A 01 VARIABLE
## 0-male, 1-female

reg_dt <- reg_dt %>% 
  mutate(gender4 = case_when(
    
    comb_dt$w4_sex == 1 ~ "0",
    comb_dt$w4_sex == 3  ~ "1",

    TRUE ~ NA_character_
  ))

reg_dt <- reg_dt %>% 
  mutate(gender5 = case_when(
    
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

map_to_years <- function(code) {
  case_when(
    code == 2  ~ 6,   # Elementary
    code == 3  ~ 9,   # Junior high general
    code == 4  ~ 9,   # Junior high vocational
    code == 5  ~ 12,  # Senior high general
    code == 6  ~ 12,  # Senior high vocational
    code == 11 ~ 9,   # Adult education A
    code == 12 ~ 12,  # Adult education B
    code == 13 ~ 16,  # Open university
    code == 14 ~ 12,  # Pesantren
    code == 15 ~ 12,  # Adult education C
    code == 17 ~ 12,  # School for Disabled
    code == 60 ~ 15,  # College (D1-D3)
    code == 61 ~ 16,  # Bachelor (S1)
    code == 62 ~ 18,  # Master (S2)
    code == 63 ~ 21,  # Doctoral (S3)
    code == 72 ~ 6,   # Islamic Elementary
    code == 73 ~ 9,   # Islamic Junior High
    code == 74 ~ 12,  # Islamic Senior High
    code == 90 ~ 0,   # Kindergarten
    TRUE ~ NA_real_   # Other/Missing/Don't Know
  )
}

comb_dt$w4_edu_years <- map_to_years(comb_dt$w4_dl06)
comb_dt$w5_edu_years <- map_to_years(comb_dt$w5_dl06_code)

reg_dt <- reg_dt %>%
  mutate(edu4 = comb_dt$w4_edu_years,
         edu5 = comb_dt$w5_edu_years)

comb_dt <- comb_dt %>% select(-w5_dl06_code)


###REMOVE ANY ROWS WITH N/A VARIABLES

reg_dt <- reg_dt %>%
  drop_na()

###LIMIT AGE TO EVERYONE OLDER THAT 15 
reg_dt <- reg_dt %>%
  filter(w4_age >= 15)

##check proportion of data remaining post N/A removal
perc = 14054/21381







