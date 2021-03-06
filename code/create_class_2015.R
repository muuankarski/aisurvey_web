#' ---


#' This script imitates the class variable creation implemented in 'class group.do' -script
#' 
#' It assumes that we have the raw longitudinal file 'merged_4wave20151110N.RData' is used as a base data
#' 
# load("./data/merged_4wave20151110N.RData")



#+ occup_groups
d[[oldvar2]][d[[oldvar]]>=1100 & d[[oldvar]]<=1120] <- 11
d[[oldvar2]][d[[oldvar]]>=1311 & d[[oldvar]]<=1324] <- 13
d[[oldvar2]][d[[oldvar]]>=1342 & d[[oldvar]]<=1349] <- 53
d[[oldvar2]][d[[oldvar]]>=1412 & d[[oldvar]]<=1431] <- 13
d[[oldvar2]][d[[oldvar]]>=2112 & d[[oldvar]]<=2133] <- 21
d[[oldvar2]][d[[oldvar]]>=2161 & d[[oldvar]]<=2165] <- 21
d[[oldvar2]][d[[oldvar]]>=2141 & d[[oldvar]]<=2153] <- 3
d[[oldvar2]][d[[oldvar]]>=2212 & d[[oldvar]]<=2222] <- 53
d[[oldvar2]][d[[oldvar]]>=2261 & d[[oldvar]]<=2266] <- 22
d[[oldvar2]][d[[oldvar]]>=2310 & d[[oldvar]]<=2330] <- 23
d[[oldvar2]][d[[oldvar]]>=2341 & d[[oldvar]]<=2342] <- 53
d[[oldvar2]][d[[oldvar]]>=2411 & d[[oldvar]]<=2431] <- 24
d[[oldvar2]][d[[oldvar]]>=2512 & d[[oldvar]]<=2522] <- 25
d[[oldvar2]][d[[oldvar]]>=2611 & d[[oldvar]]<=2653] <- 26
d[[oldvar2]][d[[oldvar]]>=3111 & d[[oldvar]]<=3154] <- 31
d[[oldvar2]][d[[oldvar]]>=3211 & d[[oldvar]]<=3258] <- 32
d[[oldvar2]][d[[oldvar]]>=3311 & d[[oldvar]]<=3323] <- 33
d[[oldvar2]][d[[oldvar]]>=3333 & d[[oldvar]]<=3334] <- 12
d[[oldvar2]][d[[oldvar]]>=3341 & d[[oldvar]]<=3344] <- 4
d[[oldvar2]][d[[oldvar]]>=3351 & d[[oldvar]]<=3359] <- 33
d[[oldvar2]][d[[oldvar]]>=3411 & d[[oldvar]]<=3435] <- 34
d[[oldvar2]][d[[oldvar]]>=3511 & d[[oldvar]]<=3521] <- 35
d[[oldvar2]][d[[oldvar]]>=4110 & d[[oldvar]]<=4132] <- 41
d[[oldvar2]][d[[oldvar]]>=4211 & d[[oldvar]]<=4227] <- 42
d[[oldvar2]][d[[oldvar]]>=4311 & d[[oldvar]]<=4323] <- 43
d[[oldvar2]][d[[oldvar]]>=4411 & d[[oldvar]]<=4419] <- 44
d[[oldvar2]][d[[oldvar]]>=5111 & d[[oldvar]]<=5169] <- 52
d[[oldvar2]][d[[oldvar]]>=5211 & d[[oldvar]]<=5245] <- 52
d[[oldvar2]][d[[oldvar]]>=5311 & d[[oldvar]]<=5321] <- 53
d[[oldvar2]][d[[oldvar]]>=5411 & d[[oldvar]]<=5419] <- 11
d[[oldvar2]][d[[oldvar]]>=6210 & d[[oldvar]]<=6223] <- 6
d[[oldvar2]][d[[oldvar]]>=7111 & d[[oldvar]]<=7133] <- 7
d[[oldvar2]][d[[oldvar]]>=7211 & d[[oldvar]]<=7234] <- 7
d[[oldvar2]][d[[oldvar]]>=7311 & d[[oldvar]]<=7323] <- 7
d[[oldvar2]][d[[oldvar]]>=7411 & d[[oldvar]]<=7421] <- 7
d[[oldvar2]][d[[oldvar]]>=7511 & d[[oldvar]]<=7549] <- 7
d[[oldvar2]][d[[oldvar]]>=8111 & d[[oldvar]]<=8189] <- 8
d[[oldvar2]][d[[oldvar]]>=8211 & d[[oldvar]]<=8219] <- 8
d[[oldvar2]][d[[oldvar]]>=8311 & d[[oldvar]]<=8343] <- 8
d[[oldvar2]][d[[oldvar]]>=9112 & d[[oldvar]]<=9129] <- 91
d[[oldvar2]][d[[oldvar]]>=9211 & d[[oldvar]]<=9212] <- 92
d[[oldvar2]][d[[oldvar]]>=9312 & d[[oldvar]]<=9334] <- 92
d[[oldvar2]][d$oldvar==9412] <- 92
d[[oldvar2]][d[[oldvar]]>=9613 & d[[oldvar]]<=9629] <- 92
d[[oldvar2]][d[[oldvar]]>=9997 & d[[oldvar]]<=9999] <- 99
d[[oldvar2]][d$oldvar==110] <- 11
d[[oldvar2]][d$oldvar==210] <- 21
d[[oldvar2]][d[[oldvar]]>=2351 & d[[oldvar]]<=2359] <- 23
d[[oldvar2]][d$oldvar==3] <- 35
d[[oldvar2]][d$oldvar==52] <- 5
d[[oldvar2]][d$oldvar==62] <- 6

# table(d$occup_groups, useNA="ifany")
  
d$occup_groups_labelled[d$occup_groups %in% 3 ] <- "3-Technicians and associate professionals" 
d$occup_groups_labelled[d$occup_groups %in% 4 ] <- "4-Clerical support workers" 
d$occup_groups_labelled[d$occup_groups %in% 5 ] <- "5-Service and sales workers" 
d$occup_groups_labelled[d$occup_groups %in% 6 ] <- "6-Skilled agricultural, forestry and fishery workers" 
d$occup_groups_labelled[d$occup_groups %in% 7 ] <- "7-Craft and related trades workers" 
d$occup_groups_labelled[d$occup_groups %in% 8 ] <- "8-Plant and machine operators, and assemblers" 
d$occup_groups_labelled[d$occup_groups %in% 9 ] <- "9-Elementary occupations" 
d$occup_groups_labelled[d$occup_groups %in% 11 ] <- "11-Chief executives, senior officials and legislators" 
d$occup_groups_labelled[d$occup_groups %in% 12 ] <- "12-Administrative and commercial managers" 
d$occup_groups_labelled[d$occup_groups %in% 13 ] <- "13-Production and specialized services managers" 
d$occup_groups_labelled[d$occup_groups %in% 14 ] <- "14-Hospitality, retail and other services managers" 
d$occup_groups_labelled[d$occup_groups %in% 21 ] <- "21-Science and engineering professionals" 
d$occup_groups_labelled[d$occup_groups %in% 22 ] <- "22-Health professionals" 
d$occup_groups_labelled[d$occup_groups %in% 23 ] <- "23-Teaching professionals" 
d$occup_groups_labelled[d$occup_groups %in% 24 ] <- "24-Business and administration professionals" 
d$occup_groups_labelled[d$occup_groups %in% 25 ] <- "25-Information and communications technology professionals" 
d$occup_groups_labelled[d$occup_groups %in% 26 ] <- "26-Legal, social and cultural professionals" 
d$occup_groups_labelled[d$occup_groups %in% 31 ] <- "31-Science and engineering associate professionals" 
d$occup_groups_labelled[d$occup_groups %in% 32 ] <- "32-Health associate professionals" 
d$occup_groups_labelled[d$occup_groups %in% 33 ] <- "33-Business and administration associate professionals" 
d$occup_groups_labelled[d$occup_groups %in% 34 ] <- "34-Legal, social, cultural and related associate professionals" 
d$occup_groups_labelled[d$occup_groups %in% 35 ] <- "35-Information and communications technicians" 
d$occup_groups_labelled[d$occup_groups %in% 41 ] <- "41-General and keyboard clerks" 
d$occup_groups_labelled[d$occup_groups %in% 42 ] <- "42-Customer services clerks" 
d$occup_groups_labelled[d$occup_groups %in% 43 ] <- "43-Numerical and material recording clerks" 
d$occup_groups_labelled[d$occup_groups %in% 44 ] <- "44-Other clerical support workers" 
d$occup_groups_labelled[d$occup_groups %in% 51 ] <- "51-Personal service workers" 
d$occup_groups_labelled[d$occup_groups %in% 52 ] <- "52-Sales workers" 
d$occup_groups_labelled[d$occup_groups %in% 53 ] <- "53-Personal care workers" 
d$occup_groups_labelled[d$occup_groups %in% 54 ] <- "54-Protective services workers" 
d$occup_groups_labelled[d$occup_groups %in% 62 ] <- "62-Market-oriented skilled forestry, fishing and hunting workers" 
d$occup_groups_labelled[d$occup_groups %in% 71 ] <- "71-Building and related trades workers, excluding electricians" 
d$occup_groups_labelled[d$occup_groups %in% 72 ] <- "72-Metal, machinery and related trades workers" 
d$occup_groups_labelled[d$occup_groups %in% 73 ] <- "73-Handicraft and printing workers" 
d$occup_groups_labelled[d$occup_groups %in% 74 ] <- "74-Electrical and electronic trades workers" 
d$occup_groups_labelled[d$occup_groups %in% 75 ] <- "75-Food processing, wood working, garment and other craft and related trades workers" 
d$occup_groups_labelled[d$occup_groups %in% 81 ] <- "81-Stationary plant and machine operators" 
d$occup_groups_labelled[d$occup_groups %in% 82 ] <- "82-Assemblers" 
d$occup_groups_labelled[d$occup_groups %in% 83 ] <- "83-Drivers and mobile plant operators" 
d$occup_groups_labelled[d$occup_groups %in% 91 ] <- "91-Cleaners and helpers" 
d$occup_groups_labelled[d$occup_groups %in% 92 ] <- "92-Agricultural, forestry and fishery labourers" 
d$occup_groups_labelled[d$occup_groups %in% 93 ] <- "93-Labourers in mining, construction, manufacturing and transport" 
d$occup_groups_labelled[d$occup_groups %in% 94 ] <- "94-Food preparation assistants" 
d$occup_groups_labelled[d$occup_groups %in% 96 ] <- "96-Refuse workers and other elementary workers"
d$occup_groups_labelled <- factor(d$occup_groups_labelled, levels=c("3-Technicians and associate professionals" ,
                                                                   "4-Clerical support workers" ,
                                                                   "5-Service and sales workers" ,
                                                                   "6-Skilled agricultural, forestry and fishery workers" ,
                                                                   "7-Craft and related trades workers" ,
                                                                   "8-Plant and machine operators, and assemblers" ,
                                                                   "9-Elementary occupations" ,
                                                                   "11-Chief executives, senior officials and legislators" ,
                                                                   "12-Administrative and commercial managers" ,
                                                                   "13-Production and specialized services managers" ,
                                                                   "14-Hospitality, retail and other services managers" ,
                                                                   "21-Science and engineering professionals" ,
                                                                   "22-Health professionals" ,
                                                                   "23-Teaching professionals" ,
                                                                   "24-Business and administration professionals" ,
                                                                   "25-Information and communications technology professionals" ,
                                                                   "26-Legal, social and cultural professionals" ,
                                                                   "31-Science and engineering associate professionals" ,
                                                                   "32-Health associate professionals" ,
                                                                   "33-Business and administration associate professionals" ,
                                                                   "34-Legal, social, cultural and related associate professionals" ,
                                                                   "35-Information and communications technicians" ,
                                                                   "41-General and keyboard clerks" ,
                                                                   "42-Customer services clerks" ,
                                                                   "43-Numerical and material recording clerks" ,
                                                                   "44-Other clerical support workers" ,
                                                                   "51-Personal service workers" ,
                                                                   "52-Sales workers" ,
                                                                   "53-Personal care workers" ,
                                                                   "54-Protective services workers" ,
                                                                   "62-Market-oriented skilled forestry, fishing and hunting workers" ,
                                                                   "71-Building and related trades workers, excluding electricians" ,
                                                                   "72-Metal, machinery and related trades workers" ,
                                                                   "73-Handicraft and printing workers" ,
                                                                   "74-Electrical and electronic trades workers" ,
                                                                   "75-Food processing, wood working, garment and other craft and related trades workers" ,
                                                                   "81-Stationary plant and machine operators" ,
                                                                   "82-Assemblers" ,
                                                                   "83-Drivers and mobile plant operators" ,
                                                                   "91-Cleaners and helpers" ,
                                                                   "92-Agricultural, forestry and fishery labourers" ,
                                                                   "93-Labourers in mining, construction, manufacturing and transport" ,
                                                                   "94-Food preparation assistants" ,
                                                                   "96-Refuse workers and other elementary workers"))
tbl <- as.data.frame(table(d$occup_groups_labelled, useNA="ifany"))
DT::datatable(tbl)


#+ autonomia  
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==12) replace autonomia=1
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 12] <- 1
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==13) replace autonomia=1
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 13] <- 1
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==11) replace autonomia=1
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 11:13] <- 1
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==91) replace autonomia=11
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 91] <- 11
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==21 | d$occup_groups==22 | d$occup_groups==23 | d$occup_groups==24 | d$occup_groups==25 | d$occup_groups==26) replace autonomia=5
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 21:26] <- 5
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==31 | d$occup_groups==32 | d$occup_groups==33 | d$occup_groups==34 | d$occup_groups==35) replace autonomia=6
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 31:35] <- 6
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==53) replace autonomia=7
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 53] <- 7
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==4 | d$occup_groups==41 | d$occup_groups==42 | d$occup_groups==43 | d$occup_groups==44) replace autonomia=8
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% c(4,41:44)] <- 8
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==5) replace autonomia=9
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 5] <- 9
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==6 | d$occup_groups==7 | d$occup_groups==8) replace autonomia=10
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 6:8] <- 10
# if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==92) replace autonomia=11
d$autonomia[d$V21==1 & d$V19==1 & d$occup_groups %in% 92] <- 10
# if (v21_1998_2007_2015==2) replace autonomia=12
d$autonomia[d$V21==2] <- 12
d$autonomia[d$V19==2] <- 14


d$autonomia_labelled[d$autonomia %in% 1 ] <- "1-managerial" 
d$autonomia_labelled[d$autonomia %in% 5 ] <- "5-professional" 
d$autonomia_labelled[d$autonomia %in% 6 ] <- "6-scientific-technical" 
d$autonomia_labelled[d$autonomia %in% 7 ] <- "7-care work" 
d$autonomia_labelled[d$autonomia %in% 8 ] <- "8-clerical" 
d$autonomia_labelled[d$autonomia %in% 9 ] <- "9-sales" 
d$autonomia_labelled[d$autonomia %in% 10 ] <- "10-craftsman qualified" 
d$autonomia_labelled[d$autonomia %in% 11 ] <- "11-craftsman unqualified" 
d$autonomia_labelled[d$autonomia %in% 12 ] <- "12-entrepreneurs" 
d$autonomia_labelled[is.na(d$autonomia) ] <- "13-missing data"
d$autonomia_labelled[d$autonomia %in% 14 ] <- "14-not working"

d$autonomia_labelled <- factor(d$autonomia_labelled, levels=c("1-managerial",
                                                              "5-professional",
                                                              "6-scientific-technical",
                                                              "7-care work",
                                                              "8-clerical",
                                                              "10-craftsman qualified",
                                                              "11-craftsman unqualified",
                                                              "12-entrepreneurs",
                                                              "13-missing data",
                                                              "14-not working"))

tbl <- as.data.frame(table(d$autonomia_labelled, useNA="ifany"))
DT::datatable(tbl)

# View(as.data.frame(table(d$autonomia_labelled, useNA="ifany")))





# # /Second type. Classification on decision-making and autonomy (SDMR2015)
# 
# # /Indexes of decision-making and autonomy
# # /There are a number of questions in the study that gauge the level of participation in the decision-making. 
# #  Each question defines individual input into decision-making using a three-point scale: 
# ## 1 decision taken by the respondent without prior consultations with his or her superiors, 
# ## 2 decision taken in consort with other employees and no influence upon decisions in the designated area. 
# # The questions serves as the basis for an index of decision-making (DM index) normalized to range from 1 to 100.  
# # The autonomy index (AUT-index) is measured in the same way as the decision-making power.
# 
# /Syntax for calculation of DMi_15022016
# /Questions v51_1c1 … v51_13c1 (decision-making)
# 
# mvencode V51_1c1 V51_2c1 V51_3c1 V51_4c1 V51_5c1 V51_6c1 V51_7c1 V51_8c1 V51_9c1 V51_10c1 V51_11c1 V51_12c1 V51_13c1, mv(0)
# gen v51_whole_for_13=V51_1c1 + V51_2c1 + V51_3c1 + V51_4c1 + V51_5c1 + V51_6c1 + V51_7c1 + V51_8c1 + V51_9c1 + V51_10c1 + V51_11c1 + V51_12c1 + V51_13c1
# /Dmax=3*13=39
# gen DMi_for13=( v51_whole_for_13/39)*100
# gen v51_whole_for_12=V51_1c1 + V51_2c1 + V51_3c1 + V51_4c1 + V51_5c1 + V51_6c1 + V51_7c1 + V51_8c1 + V51_9c1 + V51_10c1 + V51_11c1 + V51_12c1
# /Dmax=3*12=36
# gen DMi_for12=( v51_whole_for_12/36)*100
# /The DM index and the Wright variable

# mvencode V51_1c1 V51_2c1 V51_3c1 V51_4c1 V51_5c1 V51_6c1 V51_7c1 V51_8c1 V51_9c1 V51_10c1 V51_11c1 V51_12c1 V51_13c1, mv(0)
# gen 

# d <- d15n
#+ DMindex
d$v51_whole_for_13 <- rowSums(d[c("V51_1c1",  "V51_2c1", "V51_3c1", "V51_4c1", "V51_5c1", 
                          "V51_6c1", "V51_7c1", "V51_8c1", "V51_9c1", "V51_10c1",
                          "V51_11c1", "V51_12c1", "V51_13c1")],na.rm = TRUE)

d$DMi_for13 <- d$v51_whole_for_13/39*100
# /Dmax=3*13=39
# gen DMi_for13=( v51_whole_for_13/39)*100
# gen v51_whole_for_12=V51_1c1 + V51_2c1 + V51_3c1 + V51_4c1 + V51_5c1 + V51_6c1 + V51_7c1 + V51_8c1 + V51_9c1 + V51_10c1 + V51_11c1 + V51_12c1
# /Dmax=3*12=36
# gen DMi_for12=( v51_whole_for_12/36)*100
# /The DM index and the Wright variable

# tabstat DMi_for12, statistics( mean sd ) by(wright)
# tabstat DMi_for13, statistics( mean sd ) by(wright)
# 
# /Questions v46_1c1…v46_6c1 (autonomy)
# gen v46_1= V46_1c1
# gen v46_2= V46_2c1
# gen v46_3= V46_3c1
# gen v46_4= V46_4c1
# gen v46_5= V46_5c1
# gen v46_6= V46_6c1
# mvencode v46_1 v46_2 v46_3 v46_4 v46_5 v46_6, mv(0)
# / All 9s (“Hard to say”) have been replaced to 0s.
# replace v46_1=0 if v46_1==9
# replace v46_2=0 if v46_2==9
# replace v46_3=0 if v46_3==9
# replace v46_4=0 if v46_4==9
# replace v46_5=0 if v46_5==9
# replace v46_6=0 if v46_6==9
# gen v46_for6= v46_1 + v46_2 + v46_3 + v46_4 + v46_5 + v46_6
# /Dmax=3*6=18
# gen AUT_index= (v46_for6/18)*100
# /The DM index and the Wright variable
# tabstat AUT_index, statistics( mean sd ) by(wright)

d$V46_1c1[d$V46_1c1 == 9] <- 0
d$V46_2c1[d$V46_2c1 == 9] <- 0
d$V46_3c1[d$V46_3c1 == 9] <- 0
d$V46_4c1[d$V46_4c1 == 9] <- 0
d$V46_5c1[d$V46_5c1 == 9] <- 0
d$V46_6c1[d$V46_6c1 == 9] <- 0

d$v46_for6 <- rowSums(d[c("V46_1c1",
                          "V46_2c1",
                          "V46_3c1",
                          "V46_4c1",
                          "V46_5c1",
                          "V46_6c1")],na.rm = TRUE)

d$AUT_index <- d$v46_for6/18*100

## ********************************************* ##
## ********************************************* ##
## Melko hyvä wrigth
# 
# ***LUOKKIEN LUONTI:
  
# COUNT yesto38=v38_1 to v38_5 (1).
# COUNT yesto39=v39_1 to v39_5 (1).
# COUNT direct40=v40_1 to v40_12 (3).
# COUNT discus40=v40_1 to v40_12 (2).
# COUNT unappl40=v40_1 to v40_12 (9).

# COUNT yesto38=v38_1 to v38_5 (1).

# countcases2 <- function(x, n) { rowSums(x == n) }



# COUNT yesto39=v39_1 to v39_5 (1).
# COUNT direct40=v40_1 to v40_12 (3).
# COUNT discus40=v40_1 to v40_12 (2).
# COUNT unappl40=v40_1 to v40_12 (9).


# EXECUTE.
# VAR LABEL yesto38 'number of "yes" answers in Q38 (decision making)'/yesto39 'number of "yes" answers in Q39 (influence)'/ direct40 'number of "directly" answers in Q40 (decision)'/discus40 'number of "in discussion" answers in Q40 (decision)'/unappl40 'number of "not applicable" answers in Q40'.

# COMPUTE dirper40=direct40 / (12-unappl40).
# VARIABLE LABELS dirper40 'proportion of "directly" answers of all applicable variables in question 40 '.
# EXECUTE.

# COMPUTE disper40=discus40 / (12-unappl40).
# VARIABLE LABELS disper40 'proportion of "in discussion" answers of all applicable variables in question 40 '.
# EXECUTE.

# compute occr=v13_code.

d$occr[d$V14_CODE %in% c(2212, 2221, 2222, 2223, 2224, 2220)] <- 1
d$occr[d$V14_CODE %in% c(2229, 2230, 3220, 3223, 3224, 3226, 3229, 3231, 3232)] <- 2
d$occr[d$V14_CODE %in% c(2411)] <- 3
d$occr[d$V14_CODE %in% c(2320, 2331, 2332, 2340, 2351, 2359, 3310, 3320, 3330, 3340, 2300, 2330, 2350, 3300)] <- 4
d$occr[d$V14_CODE %in% c(2310, 2400, 2410, 2431, 2432, 2430)] <- 5
d$occr[d$V14_CODE %in% c(2211, 2212, 2213, 2412, 2419,2441:2445,2000, 2210, 2440)] <- 5.5
d$occr[d$V14_CODE %in% c(2111, 2112, 2113, 2121, 2122, 2131, 2139, 2141, 2142, 2143, 2144, 2145, 2146, 2147, 2148, 2149, 3112,
                         3141, 3434, 2100, 2110, 2114, 2130, 2140)] <- 6
d$occr[d$V14_CODE %in% c(3114, 3117, 3118, 3121, 3122, 3123, 3131, 3132, 3133, 3139, 3142, 3143, 3144, 3145, 3211, 3212,
                         3213, 3111, 3113, 3115, 3116, 3119, 3151)] <- 7
d$occr[d$V14_CODE %in% c(2446, 2460, 2470, 3423, 3429, 3432, 3460, 3480, 5150)] <- 8
d$occr[d$V14_CODE %in% c(2421, 2422, 2429)] <- 9
d$occr[d$V14_CODE %in% c(2451, 2452, 2453, 2454, 2455, 3471, 3472, 3473, 3474, 3475, 5210, 2450, 3470, 3000, 3100, 3110, 3120,
                         3130, 3140, 3150, 3200, 3210)] <- 10
d$occr[d$V14_CODE %in% c(1110, 1141, 1142, 1143, 2352, 3152, 1140)] <- 11
d$occr[d$V14_CODE %in% c(1210, 1231, 1232, 1234, 1235, 1236, 1237, 1238, 1239, 1316, 1317, 1318, 1319,
                         3416)] <- 12
d$occr[d$V14_CODE %in% c(1225, 1233, 1314, 1315)] <- 13
d$occr[d$V14_CODE %in% c(3431, 3233, 4115)] <- 14
d$occr[d$V14_CODE %in% c(4111, 4112, 4113, 4114, 4121, 4122, 4131, 4132, 4133, 4141, 4142, 4143, 4144, 4190, 4211,
                         4212, 4213, 4214, 4215, 4221, 4222, 4223, 3430, 4000, 4100, 4110, 4120, 4130, 4140, 4200, 4210,
                         4220)] <- 15
d$occr[d$V14_CODE %in% c(3411, 3412, 3413, 3414, 3415, 3417, 3419, 3421, 3422, 5220, 5221, 5222, 5223, 5230, 9111, 9113, 3400,
                         3410, 3420, 5200, 9100, 9110)] <- 16
d$occr[d$V14_CODE %in% c(1221, 1222, 1226, 1227, 1228, 1229, 1223, 1224,1311, 1312, 1313, 1200, 1220, 1300, 1310)] <- 17
d$occr[d$V14_CODE %in% c(7111:7113, 7121:7137, 7139:7143, 7211:7216, 7221:7224, 7231:7245,
                         7311:7313, 7321:7324, 7331, 7332, 7341:7343, 7345, 7346, 7411:7413, 7415, 7421,
                         7422, 7433:7437, 7441, 7442, 8124, 7000, 7100, 7110, 7120, 7200, 7210, 7220, 7230, 7300, 7310,
                         7320, 7330, 7340, 7400)] <- 18
d$occr[d$V14_CODE %in% c(3441:3444, 3449, 3450, 5161:5163, 5169, 0100, 5160)] <- 19
d$occr[d$V14_CODE %in% c(5112, 8311, 8312, 8321:8324, 8331:8334, 8340, 8300, 8310, 8320, 8330)] <- 20
d$occr[d$V14_CODE %in% c(7344, 7414, 7416, 7423, 7424, 7431, 7432, 8111, 8112, 8113, 8121:8123, 8131, 8139:8143,
                         8151:8155, 8159:8163, 8170, 8211, 8212, 8221:8224, 8229:8232, 8240, 8251:8253,
                         8261:8266, 8269:8287, 8290, 9320, 7410, 7420, 7430, 8000, 8100, 8110, 8120, 8130, 8150, 8200,
                         8210, 8220, 8250, 8260)] <- 21
d$occr[d$V14_CODE %in% c(6121:6129, 6142, 9142, 9161,
                         9311:9313, 9330, 9300, 9310)] <- 22
d$occr[d$V14_CODE %in% c(6141, 6151:6154, 9211, 9212, 9213, 6140, 6150, 9200, 9210)] <- 23
d$occr[d$V14_CODE %in% c(3221, 3222, 3225, 3227, 3228)] <- 24
d$occr[d$V14_CODE %in% c(5113, 5122, 5141, 5140)] <- 25
d$occr[d$V14_CODE %in% c(5111, 5121, 5131:5139, 5142, 5143, 5149, 9120, 9131:9133, 9141, 9151:9153, 9162, 5000,
                         5120, 5123, 5130, 9000,9130, 9140, 9150, 9160)] <- 26
d$occr[d$V14_CODE %in% c(6121:6130, 6111, 6112, 6000, 6100, 6110, 6120)] <- 27
d$occr[d$V14_CODE %in% c(1100)] <- 11
d$occr[d$V14_CODE %in% c(1230)] <- 12
d$occr[d$V14_CODE %in% c(2420)] <- 9
d$occr[d$V14_CODE %in% c(3230)] <- 2
d$occr[d$V14_CODE %in% c(3433)] <- 14
d$occr[d$V14_CODE %in% c(3440)] <- 19
d$occr[d$V14_CODE %in% c(5100)] <- NA


d$occr_labeled[d$occr == 1 ] <-  'Physicians and dentists'
d$occr_labeled[d$occr == 2 ] <-  'Other medical and paramedical'
d$occr_labeled[d$occr == 3 ] <-  'Accountants, auditors, actuaries'
d$occr_labeled[d$occr == 4 ] <-  'Teachers: elementary and secondary'
d$occr_labeled[d$occr == 5 ] <-  'Teachers: university, social sc, librarians'
d$occr_labeled[d$occr == 5.5]<-  'Other univ professionals'
d$occr_labeled[d$occr == 6 ] <-  'Mathematicians, engineers etc'
d$occr_labeled[d$occr == 7 ] <-  'Technicians etc'
d$occr_labeled[d$occr == 8 ] <-  'Public advisors'
d$occr_labeled[d$occr == 9 ] <-  'Lawyers and judges'
d$occr_labeled[d$occr == 10 ] <-  'Arts and entertainment'
d$occr_labeled[d$occr == 11 ] <-  'Managers: public and quasi-public'
d$occr_labeled[d$occr == 12 ] <-  'Managers: corporate'
d$occr_labeled[d$occr == 13 ] <-  'Managers: other'
d$occr_labeled[d$occr == 14 ] <-  'Secretaries'
d$occr_labeled[d$occr == 15 ] <-  'Other clerical'
d$occr_labeled[d$occr == 16 ] <-  'Sales'
d$occr_labeled[d$occr == 17 ] <-  'Foremen'
d$occr_labeled[d$occr == 18 ] <-  'Crafts'
d$occr_labeled[d$occr == 19 ] <-  'Government protective workers'
d$occr_labeled[d$occr == 20 ] <-  'Transportation workers'
d$occr_labeled[d$occr == 21 ] <-  'Operatives, except transportation'
d$occr_labeled[d$occr == 22 ] <-  'Laborers, except farm laborers'
d$occr_labeled[d$occr == 23 ] <-  'Farm Workers'
d$occr_labeled[d$occr == 24 ] <-  'White collar services'
d$occr_labeled[d$occr == 25 ] <-  'Skilled manual services'
d$occr_labeled[d$occr == 26 ] <-  'Lowskilled services'
d$occr_labeled[d$occr == 27 ] <-  'Farmers and related profession'

d$skill2[d$occr %in% c(1, 3, 5, 5.5, 6, 9, 11, 12)] <- 1
d$skill2[d$occr %in% c(2, 4, 7, 8, 10, 13, 17, 18, 19, 25, 27)] <- 2
d$skill2[d$occr %in% c(14, 15, 16, 20, 21, 22, 23,24, 26)] <- 3

d$skill2_labeled[d$skill2 == 1] <- 'Expert'
d$skill2_labeled[d$skill2 == 2] <- 'Skilled'
d$skill2_labeled[d$skill2 == 3] <- 'Low/semi skilled'


# 
# 
# IF v16=2 wrightAV=1.
# IF v16=1 and v37=1 and yesto38+yesto39>5 and dirper40+disper40>0.5 wrightAV=2.
# IF v16=1 and v37=1 and (yesto38+yesto39<6 or (dirper40+disper40<0.5 or dirper40+disper40=0.5)) wrightAV=3.
# IF v16=1 and v37=2 and skill2=1 wrightAV=4.
# IF v16=1 and v37=2 and skill2=2 wrightAV=5.
# IF v16=1 and v37=2 and skill2=3 wrightAV=6.
# VAR LABEL wrightAV 'Wright class typology ammatin vaativuus kriteerinä'.
# VAL LABELS wrightAV
# 1 'entrepreneurs'
# 2 'managers'
# 3 'supervisors'
# 4 'expert workers'
# 5 'skilled workers'
# 6 'low/semi skilled workers'
# 7 'others'.
# 
# COMPUTE wrightK=7.
# IF v16=2 wrightK=1.
# IF v16=1 and v37=1 and yesto38+yesto39>5 and dirper40+disper40>0.5 wrightK=2.
# IF v16=1 and v37=1 and (yesto38+yesto39<6 or (dirper40+disper40<0.5 or dirper40+disper40=0.5)) wrightK=3.
# IF v16=1 and v37=2 and v102>6 wrightK=4.
# IF v16=1 and v37=2 and (v102=4 or v102=5 or v102=6) wrightK=5.
# IF v16=1 and v37=2 and v102<4 wrightK=6.
# VAR LABEL wrightK 'Wright class typology koulutuksen taso kriteerinä'.
# VAL LABELS wrightK
# 1 'entrepreneurs'
# 2 'managers'
# 3 'supervisors'
# 4 'expert workers'
# 5 'skilled workers'
# 6 'low/semi skilled workers'
# 7 'others'.
# 
# ## ********************************************* ##
# ## ********************************************* ##
# #### KIVISEN LUOKAT
# 
# 
# ***LUOKKIEN LUONTI:
#   
#   COMPUTE autonomia=13.
# IF v16=1 and v11=1 and autotype=1 and (v25 NE 1) and (v25 NE 7) autonomia=1.
# IF v16=1 and v11=1 and autotype=6 autonomia=2.
# IF v16=1 and v11=1 and ((autotype=2) or (autotype=1 and (v25=1 or v25=7))) autonomia=3.
# IF v16=1 and v11=1 and autotype=3 autonomia=4.
# IF v16=1 and v11=1 and autotype=4 autonomia=5.
# IF v16=1 and v11=1 and autotype=5 autonomia=6.
# IF v16=1 and v11=1 and autotype=9 autonomia=7.
# IF v16=1 and v11=1 and autotype=8 autonomia=8.
# IF v16=1 and v11=1 and autotype=7 autonomia=9.
# IF v16=1 and v11=1 and autotype=10 autonomia=10.
# IF v16=1 and v11=1 and autotype=11 autonomia=11.
# IF v16=2 autonomia=12.
# EXECUTE.
# VAR LABEL autonomia 'Autonomien tyyppi'.
# VAL LABELS autonomia
# 1 'manageriaalinen'
# 2 'pienyritysten'
# 3 'byrokraattis-hallinnollinen I (johtavat virkamiehet)'
# 4 'byrokraattis-hallinnollinen II (vahtimestarit ym.)'
# 5 'professionaalinen'
# 6 'tieteellis-tekninen'
# 7 'hoivatyön'
# 8 'toimistotyön'
# 9 'myyntityön'
# 10 'ammattimiehen I (kvalifioitu)'
# 11 'ammattimiehen II (kvalifioimaton)'
# 12 'yrittäjät'
# 13 'puuttuva tieto'.
# 
# RECODE v28 (1=1) (ELSE=0) INTO v28index.
# RECODE v29 (3=1) (ELSE=0) INTO v29index.
# RECODE v30 (3=1) (ELSE=0) INTO v30index.
# EXECUTE.
# 
# COMPUTE autoindex=v28index + v29index + v30index.
# EXECUTE.
# 
# COMPUTE autoluok=5.
# IF autonomia=1 or autonomia=3 or autonomia=5 or autonomia=6 autoluok=1.
# IF autonomia=4 or autonomia=8 or autonomia=9 or autonomia=11 autoluok=2.
# IF autonomia=2 or autonomia=7 or autonomia=10 autoluok=3.
# IF autonomia=12 autoluok=4.
# exe.
# VAR LABELS autoluok 'Autonomian tyyppien luokitus'.
# VAL LABELS autoluok
# 1 'manageriaalinen, byrokraattis-halllinnollinen I, professionaalinen, tieteellis-tekninen'
# 2 'byrokraattis-hallinnollinen II, toimistotyön, myyntityön, ammattimiehen II'
# 3 'pienyritysten, hoivatyön, ammattimiehen I'
# 4 'yrittäjät'
# 5 'puuttuva tieto'.
# 
# COMPUTE luokkarakenne2=7.
# IF autoluok=1 and autoindex>1 luokkarakenne2=1.
# IF autoluok=1 and autoindex<2 luokkarakenne2=3.
# IF autoluok=2 and autoindex>1 luokkarakenne2=4.
# IF autoluok=2 and autoindex<2 luokkarakenne2=5.
# IF autoluok=3 and autoindex>1 luokkarakenne2=2.
# IF autoluok=3 and autoindex<2 luokkarakenne2=5.
# IF autoluok=4 luokkarakenne2=6.
# exe.
# VAR LABELS luokkarakenne2 'Luokkarakenne autonomian kriteerillä autonomiaindeksi=2'.
# VAL LABELS luokkarakenne2
# 1 'keskiluokkien ydin'
# 2 'reunaryhmä 1'
# 3 'reunaryhmä 2'
# 4 'reunaryhmä 3'
# 5 'työväenluokka'
# 6 'yrittäjät'
# 7 'puuttuva tieto'.

