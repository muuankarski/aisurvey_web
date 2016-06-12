
# /There are three versions of ‘class group’ –variable.
# 
# /First type. Classification on occupation (merged file).
# /Variable “autonomia”.
# 
# gen occupation=v14_CODE_2015

apply_class_coding <- function(dataframe){
  
  recode_occup <- function(data,varname,newvarname){
    
    d <- data
    d$oldvar <- data[[varname]]
    d$oldvar2[d$oldvar>=1100 & d$oldvar<=1120] <- 11
    d$oldvar2[d$oldvar>=1311 & d$oldvar<=1324] <- 13
    d$oldvar2[d$oldvar>=1342 & d$oldvar<=1349] <- 53
    d$oldvar2[d$oldvar>=1412 & d$oldvar<=1431] <- 13
    d$oldvar2[d$oldvar>=2112 & d$oldvar<=2133] <- 21
    d$oldvar2[d$oldvar>=2161 & d$oldvar<=2165] <- 21
    d$oldvar2[d$oldvar>=2141 & d$oldvar<=2153] <- 3
    d$oldvar2[d$oldvar>=2212 & d$oldvar<=2222] <- 53
    d$oldvar2[d$oldvar>=2261 & d$oldvar<=2266] <- 22
    d$oldvar2[d$oldvar>=2310 & d$oldvar<=2330] <- 23
    d$oldvar2[d$oldvar>=2341 & d$oldvar<=2342] <- 53
    d$oldvar2[d$oldvar>=2411 & d$oldvar<=2431] <- 24
    d$oldvar2[d$oldvar>=2512 & d$oldvar<=2522] <- 25
    d$oldvar2[d$oldvar>=2611 & d$oldvar<=2653] <- 26
    d$oldvar2[d$oldvar>=3111 & d$oldvar<=3154] <- 31
    d$oldvar2[d$oldvar>=3211 & d$oldvar<=3258] <- 32
    d$oldvar2[d$oldvar>=3311 & d$oldvar<=3323] <- 33
    d$oldvar2[d$oldvar>=3333 & d$oldvar<=3334] <- 12
    d$oldvar2[d$oldvar>=3341 & d$oldvar<=3344] <- 4
    d$oldvar2[d$oldvar>=3351 & d$oldvar<=3359] <- 33
    d$oldvar2[d$oldvar>=3411 & d$oldvar<=3435] <- 34
    d$oldvar2[d$oldvar>=3511 & d$oldvar<=3521] <- 35
    d$oldvar2[d$oldvar>=4110 & d$oldvar<=4132] <- 41
    d$oldvar2[d$oldvar>=4211 & d$oldvar<=4227] <- 42
    d$oldvar2[d$oldvar>=4311 & d$oldvar<=4323] <- 43
    d$oldvar2[d$oldvar>=4411 & d$oldvar<=4419] <- 44
    d$oldvar2[d$oldvar>=5111 & d$oldvar<=5169] <- 52
    d$oldvar2[d$oldvar>=5211 & d$oldvar<=5245] <- 52
    d$oldvar2[d$oldvar>=5311 & d$oldvar<=5321] <- 53
    d$oldvar2[d$oldvar>=5411 & d$oldvar<=5419] <- 11
    d$oldvar2[d$oldvar>=6210 & d$oldvar<=6223] <- 6
    d$oldvar2[d$oldvar>=7111 & d$oldvar<=7133] <- 7
    d$oldvar2[d$oldvar>=7211 & d$oldvar<=7234] <- 7
    d$oldvar2[d$oldvar>=7311 & d$oldvar<=7323] <- 7
    d$oldvar2[d$oldvar>=7411 & d$oldvar<=7421] <- 7
    d$oldvar2[d$oldvar>=7511 & d$oldvar<=7549] <- 7
    d$oldvar2[d$oldvar>=8111 & d$oldvar<=8189] <- 8
    d$oldvar2[d$oldvar>=8211 & d$oldvar<=8219] <- 8
    d$oldvar2[d$oldvar>=8311 & d$oldvar<=8343] <- 8
    d$oldvar2[d$oldvar>=9112 & d$oldvar<=9129] <- 91
    d$oldvar2[d$oldvar>=9211 & d$oldvar<=9212] <- 92
    d$oldvar2[d$oldvar>=9312 & d$oldvar<=9334] <- 92
    d$oldvar2[d$oldvar==9412] <- 92
    d$oldvar2[d$oldvar>=9613 & d$oldvar<=9629] <- 92
    d$oldvar2[d$oldvar>=9997 & d$oldvar<=9999] <- 99
    d$oldvar2[d$oldvar==110] <- 11
    d$oldvar2[d$oldvar==210] <- 21
    d$oldvar2[d$oldvar>=2351 & d$oldvar<=2359] <- 23
    d$oldvar2[d$oldvar==3] <- 35
    d$oldvar2[d$oldvar==52] <- 5
    d$oldvar2[d$oldvar==62] <- 6
    names(d)[names(d) %in% "oldvar2"] <- newvarname
    return(d)
  }
  
  f <- recode_occup(data=dataframe, varname = "v14_CODE_2015", newvarname="occup_groups")
  
  
  f$newvar[f$occup_groups %in% 3 ] <- "3-Technicians and associate professionals" 
  f$newvar[f$occup_groups %in% 4 ] <- "4-Clerical support workers" 
  f$newvar[f$occup_groups %in% 5 ] <- "5-Service and sales workers" 
  f$newvar[f$occup_groups %in% 6 ] <- "6-Skilled agricultural, forestry and fishery workers" 
  f$newvar[f$occup_groups %in% 7 ] <- "7-Craft and related trades workers" 
  f$newvar[f$occup_groups %in% 8 ] <- "8-Plant and machine operators, and assemblers" 
  f$newvar[f$occup_groups %in% 9 ] <- "9-Elementary occupations" 
  f$newvar[f$occup_groups %in% 11 ] <- "11-Chief executives, senior officials and legislators" 
  f$newvar[f$occup_groups %in% 12 ] <- "12-Administrative and commercial managers" 
  f$newvar[f$occup_groups %in% 13 ] <- "13-Production and specialized services managers" 
  f$newvar[f$occup_groups %in% 14 ] <- "14-Hospitality, retail and other services managers" 
  f$newvar[f$occup_groups %in% 21 ] <- "21-Science and engineering professionals" 
  f$newvar[f$occup_groups %in% 22 ] <- "22-Health professionals" 
  f$newvar[f$occup_groups %in% 23 ] <- "23-Teaching professionals" 
  f$newvar[f$occup_groups %in% 24 ] <- "24-Business and administration professionals" 
  f$newvar[f$occup_groups %in% 25 ] <- "25-Information and communications technology professionals" 
  f$newvar[f$occup_groups %in% 26 ] <- "26-Legal, social and cultural professionals" 
  f$newvar[f$occup_groups %in% 31 ] <- "31-Science and engineering associate professionals" 
  f$newvar[f$occup_groups %in% 32 ] <- "32-Health associate professionals" 
  f$newvar[f$occup_groups %in% 33 ] <- "33-Business and administration associate professionals" 
  f$newvar[f$occup_groups %in% 34 ] <- "34-Legal, social, cultural and related associate professionals" 
  f$newvar[f$occup_groups %in% 35 ] <- "35-Information and communications technicians" 
  f$newvar[f$occup_groups %in% 41 ] <- "41-General and keyboard clerks" 
  f$newvar[f$occup_groups %in% 42 ] <- "42-Customer services clerks" 
  f$newvar[f$occup_groups %in% 43 ] <- "43-Numerical and material recording clerks" 
  f$newvar[f$occup_groups %in% 44 ] <- "44-Other clerical support workers" 
  f$newvar[f$occup_groups %in% 51 ] <- "51-Personal service workers" 
  f$newvar[f$occup_groups %in% 52 ] <- "52-Sales workers" 
  f$newvar[f$occup_groups %in% 53 ] <- "53-Personal care workers" 
  f$newvar[f$occup_groups %in% 54 ] <- "54-Protective services workers" 
  f$newvar[f$occup_groups %in% 62 ] <- "62-Market-oriented skilled forestry, fishing and hunting workers" 
  f$newvar[f$occup_groups %in% 71 ] <- "71-Building and related trades workers, excluding electricians" 
  f$newvar[f$occup_groups %in% 72 ] <- "72-Metal, machinery and related trades workers" 
  f$newvar[f$occup_groups %in% 73 ] <- "73-Handicraft and printing workers" 
  f$newvar[f$occup_groups %in% 74 ] <- "74-Electrical and electronic trades workers" 
  f$newvar[f$occup_groups %in% 75 ] <- "75-Food processing, wood working, garment and other craft and related trades workers" 
  f$newvar[f$occup_groups %in% 81 ] <- "81-Stationary plant and machine operators" 
  f$newvar[f$occup_groups %in% 82 ] <- "82-Assemblers" 
  f$newvar[f$occup_groups %in% 83 ] <- "83-Drivers and mobile plant operators" 
  f$newvar[f$occup_groups %in% 91 ] <- "91-Cleaners and helpers" 
  f$newvar[f$occup_groups %in% 92 ] <- "92-Agricultural, forestry and fishery labourers" 
  f$newvar[f$occup_groups %in% 93 ] <- "93-Labourers in mining, construction, manufacturing and transport" 
  f$newvar[f$occup_groups %in% 94 ] <- "94-Food preparation assistants" 
  f$newvar[f$occup_groups %in% 96 ] <- "96-Refuse workers and other elementary workers"
  f$occup_groups_labelled <- f$newvar
  f$newvar <- NULL
  
  d <- f
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==12) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 12] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==13) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 13] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==11) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 11:13] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==91) replace autonomia=11
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 91] <- 11
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==21 | d$occup_groups==22 | d$occup_groups==23 | d$occup_groups==24 | d$occup_groups==25 | d$occup_groups==26) replace autonomia=5
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 21:26] <- 5
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==31 | d$occup_groups==32 | d$occup_groups==33 | d$occup_groups==34 | d$occup_groups==35) replace autonomia=6
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 31:35] <- 6
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==53) replace autonomia=7
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 53] <- 7
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==4 | d$occup_groups==41 | d$occup_groups==42 | d$occup_groups==43 | d$occup_groups==44) replace autonomia=8
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% c(4,41:44)] <- 8
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==5) replace autonomia=9
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 5] <- 9
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==6 | d$occup_groups==7 | d$occup_groups==8) replace autonomia=10
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 6:8] <- 10
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==92) replace autonomia=11
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & d$occup_groups %in% 92] <- 10
  # if (v21_1998_2007_2015==2) replace autonomia=12
  d$autonomia[d$v21_1998_2007_2015==2] <- 12
  f <- d
  
  label_autonomia <- function(var){
    
    d$oldvar <- var
    d$newvar[d$oldvar %in% 1 ] <- "1-managerial" 
    d$newvar[d$oldvar %in% 5 ] <- "5-professional" 
    d$newvar[d$oldvar %in% 6 ] <- "6-scientific-technical" 
    d$newvar[d$oldvar %in% 7 ] <- "7-care work" 
    d$newvar[d$oldvar %in% 8 ] <- "8-clerical" 
    d$newvar[d$oldvar %in% 9 ] <- "9-sales" 
    d$newvar[d$oldvar %in% 10 ] <- "10-craftsman qualified" 
    d$newvar[d$oldvar %in% 11 ] <- "11-craftsman unqualified" 
    d$newvar[d$oldvar %in% 12 ] <- "12-entrepreneurs" 
    d$newvar[d$oldvar %in% 13 ] <- "13-missing data"
    return(d$newvar)
  }
  
  
  f$autonomia_labelled <- label_autonomia(f$autonomia)
  return(f)
}

load("./data/merged_4wave20151110N.RData")
d_merge_N_class <- apply_class_coding(dataframe=d_merge_N)
save(d_merge_N_class, file="./data/d_merge_N_class.RData")

load("./data/merged_4wave20151110L.RData")
d_merge_L_class <- apply_class_coding(dataframe=d_merge_L)
save(d_merge_L_class, file="./data/d_merge_L_class.RData")


# /Second type. Classification on decision-making and autonomy (SDMR2015)

# /Indexes of decision-making and autonomy
# /There are a number of questions in the study that gauge the level of participation in the decision-making. 
#  Each question defines individual input into decision-making using a three-point scale: 
## 1 decision taken by the respondent without prior consultations with his or her superiors, 
## 2 decision taken in consort with other employees and no influence upon decisions in the designated area. 
# The questions serves as the basis for an index of decision-making (DM index) normalized to range from 1 to 100.  
# The autonomy index (AUT-index) is measured in the same way as the decision-making power.

/Syntax for calculation of DMi_15022016
/Questions v51_1c1 … v51_13c1 (decision-making)

mvencode V51_1c1 V51_2c1 V51_3c1 V51_4c1 V51_5c1 V51_6c1 V51_7c1 V51_8c1 V51_9c1 V51_10c1 V51_11c1 V51_12c1 V51_13c1, mv(0)
gen v51_whole_for_13=V51_1c1 + V51_2c1 + V51_3c1 + V51_4c1 + V51_5c1 + V51_6c1 + V51_7c1 + V51_8c1 + V51_9c1 + V51_10c1 + V51_11c1 + V51_12c1 + V51_13c1
/Dmax=3*13=39
gen DMi_for13=( v51_whole_for_13/39)*100
gen v51_whole_for_12=V51_1c1 + V51_2c1 + V51_3c1 + V51_4c1 + V51_5c1 + V51_6c1 + V51_7c1 + V51_8c1 + V51_9c1 + V51_10c1 + V51_11c1 + V51_12c1
/Dmax=3*12=36
gen DMi_for12=( v51_whole_for_12/36)*100
/The DM index and the Wright variable
tabstat DMi_for12, statistics( mean sd ) by(wright)
tabstat DMi_for13, statistics( mean sd ) by(wright)

/Questions v46_1c1…v46_6c1 (autonomy)
gen v46_1= V46_1c1
gen v46_2= V46_2c1
gen v46_3= V46_3c1
gen v46_4= V46_4c1
gen v46_5= V46_5c1
gen v46_6= V46_6c1
mvencode v46_1 v46_2 v46_3 v46_4 v46_5 v46_6, mv(0)
/ All 9s (“Hard to say”) have been replaced to 0s.
replace v46_1=0 if v46_1==9
replace v46_2=0 if v46_2==9
replace v46_3=0 if v46_3==9
replace v46_4=0 if v46_4==9
replace v46_5=0 if v46_5==9
replace v46_6=0 if v46_6==9
gen v46_for6= v46_1 + v46_2 + v46_3 + v46_4 + v46_5 + v46_6
/Dmax=3*6=18
gen AUT_index= (v46_for6/18)*100
/The DM index and the Wright variable
tabstat AUT_index, statistics( mean sd ) by(wright)
