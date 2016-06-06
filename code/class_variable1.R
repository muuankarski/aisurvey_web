
# /There are three versions of ‘class group’ –variable.
# 
# /First type. Classification on occupation (merged file).
# /Variable “autonomia”.
# 
# gen occupation=v14_CODE_2015

recode_occup <- function(data,varname,newvarname){
  
  d <- data
  d$occup_groups <- data[[varname]]
  d$occup_groups2[d$occup_groups>=1100 & d$occup_groups<=1120] <- 11
  d$occup_groups2[d$occup_groups>=1311 & d$occup_groups<=1324] <- 13
  d$occup_groups2[d$occup_groups>=1342 & d$occup_groups<=1349] <- 53
  d$occup_groups2[d$occup_groups>=1412 & d$occup_groups<=1431] <- 13
  d$occup_groups2[d$occup_groups>=2112 & d$occup_groups<=2133] <- 21
  d$occup_groups2[d$occup_groups>=2161 & d$occup_groups<=2165] <- 21
  d$occup_groups2[d$occup_groups>=2141 & d$occup_groups<=2153] <- 3
  d$occup_groups2[d$occup_groups>=2212 & d$occup_groups<=2222] <- 53
  d$occup_groups2[d$occup_groups>=2261 & d$occup_groups<=2266] <- 22
  d$occup_groups2[d$occup_groups>=2310 & d$occup_groups<=2330] <- 23
  d$occup_groups2[d$occup_groups>=2341 & d$occup_groups<=2342] <- 53
  d$occup_groups2[d$occup_groups>=2411 & d$occup_groups<=2431] <- 24
  d$occup_groups2[d$occup_groups>=2512 & d$occup_groups<=2522] <- 25
  d$occup_groups2[d$occup_groups>=2611 & d$occup_groups<=2653] <- 26
  d$occup_groups2[d$occup_groups>=3111 & d$occup_groups<=3154] <- 31
  d$occup_groups2[d$occup_groups>=3211 & d$occup_groups<=3258] <- 32
  d$occup_groups2[d$occup_groups>=3311 & d$occup_groups<=3323] <- 33
  d$occup_groups2[d$occup_groups>=3333 & d$occup_groups<=3334] <- 12
  d$occup_groups2[d$occup_groups>=3341 & d$occup_groups<=3344] <- 4
  d$occup_groups2[d$occup_groups>=3351 & d$occup_groups<=3359] <- 33
  d$occup_groups2[d$occup_groups>=3411 & d$occup_groups<=3435] <- 34
  d$occup_groups2[d$occup_groups>=3511 & d$occup_groups<=3521] <- 35
  d$occup_groups2[d$occup_groups>=4110 & d$occup_groups<=4132] <- 41
  d$occup_groups2[d$occup_groups>=4211 & d$occup_groups<=4227] <- 42
  d$occup_groups2[d$occup_groups>=4311 & d$occup_groups<=4323] <- 43
  d$occup_groups2[d$occup_groups>=4411 & d$occup_groups<=4419] <- 44
  d$occup_groups2[d$occup_groups>=5111 & d$occup_groups<=5169] <- 52
  d$occup_groups2[d$occup_groups>=5211 & d$occup_groups<=5245] <- 52
  d$occup_groups2[d$occup_groups>=5311 & d$occup_groups<=5321] <- 53
  d$occup_groups2[d$occup_groups>=5411 & d$occup_groups<=5419] <- 11
  d$occup_groups2[d$occup_groups>=6210 & d$occup_groups<=6223] <- 6
  d$occup_groups2[d$occup_groups>=7111 & d$occup_groups<=7133] <- 7
  d$occup_groups2[d$occup_groups>=7211 & d$occup_groups<=7234] <- 7
  d$occup_groups2[d$occup_groups>=7311 & d$occup_groups<=7323] <- 7
  d$occup_groups2[d$occup_groups>=7411 & d$occup_groups<=7421] <- 7
  d$occup_groups2[d$occup_groups>=7511 & d$occup_groups<=7549] <- 7
  d$occup_groups2[d$occup_groups>=8111 & d$occup_groups<=8189] <- 8
  d$occup_groups2[d$occup_groups>=8211 & d$occup_groups<=8219] <- 8
  d$occup_groups2[d$occup_groups>=8311 & d$occup_groups<=8343] <- 8
  d$occup_groups2[d$occup_groups>=9112 & d$occup_groups<=9129] <- 91
  d$occup_groups2[d$occup_groups>=9211 & d$occup_groups<=9212] <- 92
  d$occup_groups2[d$occup_groups>=9312 & d$occup_groups<=9334] <- 92
  d$occup_groups2[d$occup_groups==9412] <- 92
  d$occup_groups2[d$occup_groups>=9613 & d$occup_groups<=9629] <- 92
  d$occup_groups2[d$occup_groups>=9997 & d$occup_groups<=9999] <- 99
  d$occup_groups2[d$occup_groups==110] <- 11
  d$occup_groups2[d$occup_groups==210] <- 21
  d$occup_groups2[d$occup_groups>=2351 & d$occup_groups<=2359] <- 23
  d$occup_groups2[d$occup_groups==3] <- 35
  d$occup_groups2[d$occup_groups==52] <- 5
  d$occup_groups2[d$occup_groups==62] <- 6
  names(d)[names(d) %in% "occup_groups2"] <- newvarname
  return(d)
}

f <- recode_occup(data=d_merge_N, varname = "v14_CODE_2015", newvarname="occup_groups")


label_occup <- function(data,varname){
  
  
  rm(d)
  d$code <- varname
  d$code_labelled[d$code %in% 3 ] <- "3-Technicians and associate professionals" 
  d$code_labelled[d$code %in% 4 ] <- "4-Clerical support workers" 
  d$code_labelled[d$code %in% 5 ] <- "5-Service and sales workers" 
  d$code_labelled[d$code %in% 6 ] <- "6-Skilled agricultural, forestry and fishery workers" 
  d$code_labelled[d$code %in% 7 ] <- "7-Craft and related trades workers" 
  d$code_labelled[d$code %in% 8 ] <- "8-Plant and machine operators, and assemblers" 
  d$code_labelled[d$code %in% 9 ] <- "9-Elementary occupations" 
  d$code_labelled[d$code %in% 11 ] <- "11-Chief executives, senior officials and legislators" 
  d$code_labelled[d$code %in% 12 ] <- "12-Administrative and commercial managers" 
  d$code_labelled[d$code %in% 13 ] <- "13-Production and specialized services managers" 
  d$code_labelled[d$code %in% 14 ] <- "14-Hospitality, retail and other services managers" 
  d$code_labelled[d$code %in% 21 ] <- "21-Science and engineering professionals" 
  d$code_labelled[d$code %in% 22 ] <- "22-Health professionals" 
  d$code_labelled[d$code %in% 23 ] <- "23-Teaching professionals" 
  d$code_labelled[d$code %in% 24 ] <- "24-Business and administration professionals" 
  d$code_labelled[d$code %in% 25 ] <- "25-Information and communications technology professionals" 
  d$code_labelled[d$code %in% 26 ] <- "26-Legal, social and cultural professionals" 
  d$code_labelled[d$code %in% 31 ] <- "31-Science and engineering associate professionals" 
  d$code_labelled[d$code %in% 32 ] <- "32-Health associate professionals" 
  d$code_labelled[d$code %in% 33 ] <- "33-Business and administration associate professionals" 
  d$code_labelled[d$code %in% 34 ] <- "34-Legal, social, cultural and related associate professionals" 
  d$code_labelled[d$code %in% 35 ] <- "35-Information and communications technicians" 
  d$code_labelled[d$code %in% 41 ] <- "41-General and keyboard clerks" 
  d$code_labelled[d$code %in% 42 ] <- "42-Customer services clerks" 
  d$code_labelled[d$code %in% 43 ] <- "43-Numerical and material recording clerks" 
  d$code_labelled[d$code %in% 44 ] <- "44-Other clerical support workers" 
  d$code_labelled[d$code %in% 51 ] <- "51-Personal service workers" 
  d$code_labelled[d$code %in% 52 ] <- "52-Sales workers" 
  d$code_labelled[d$code %in% 53 ] <- "53-Personal care workers" 
  d$code_labelled[d$code %in% 54 ] <- "54-Protective services workers" 
  d$code_labelled[d$code %in% 62 ] <- "62-Market-oriented skilled forestry, fishing and hunting workers" 
  d$code_labelled[d$code %in% 71 ] <- "71-Building and related trades workers, excluding electricians" 
  d$code_labelled[d$code %in% 72 ] <- "72-Metal, machinery and related trades workers" 
  d$code_labelled[d$code %in% 73 ] <- "73-Handicraft and printing workers" 
  d$code_labelled[d$code %in% 74 ] <- "74-Electrical and electronic trades workers" 
  d$code_labelled[d$code %in% 75 ] <- "75-Food processing, wood working, garment and other craft and related trades workers" 
  d$code_labelled[d$code %in% 81 ] <- "81-Stationary plant and machine operators" 
  d$code_labelled[d$code %in% 82 ] <- "82-Assemblers" 
  d$code_labelled[d$code %in% 83 ] <- "83-Drivers and mobile plant operators" 
  d$code_labelled[d$code %in% 91 ] <- "91-Cleaners and helpers" 
  d$code_labelled[d$code %in% 92 ] <- "92-Agricultural, forestry and fishery labourers" 
  d$code_labelled[d$code %in% 93 ] <- "93-Labourers in mining, construction, manufacturing and transport" 
  d$code_labelled[d$code %in% 94 ] <- "94-Food preparation assistants" 
  d$code_labelled[d$code %in% 96 ] <- "96-Refuse workers and other elementary workers"
  d$code_labelled[is.na(d$code) ]  <- NA
  
  return(d$code_labelled)
}

ff <- label_occup(f$occup_groups)


recode_autonomia <- function(data){
  
  d <- data
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==12) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 12] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==13) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 13] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==11) replace autonomia=1
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 11:13] <- 1
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==91) replace autonomia=11
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 91] <- 11
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==21 | occup_groups==22 | occup_groups==23 | occup_groups==24 | occup_groups==25 | occup_groups==26) replace autonomia=5
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 21:26] <- 5
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==31 | occup_groups==32 | occup_groups==33 | occup_groups==34 | occup_groups==35) replace autonomia=6
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 31:35] <- 6
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==53) replace autonomia=7
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 53] <- 7
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==4 | occup_groups==41 | occup_groups==42 | occup_groups==43 | occup_groups==44) replace autonomia=8
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% c(4,41:44)] <- 8
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==5) replace autonomia=9
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 5] <- 9
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==6 | occup_groups==7 | occup_groups==8) replace autonomia=10
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 6:8] <- 10
  # if (v21_1998_2007_2015==1) & (v19_1998_2007_2015==1) & (occup_groups==92) replace autonomia=11
  d$autonomia[d$v21_1998_2007_2015==1 & d$v19_1998_2007_2015==1 & occup_groups %in% 92] <- 10
  # if (v21_1998_2007_2015==2) replace autonomia=12
  d$autonomia[d$v21_1998_2007_2015==2] <- 12
  return(d)
}


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




d15n <- recode_autonomia(d15n)

d15l$fff <- recode_occup(d15l$V14_CODE)


d15n$fff_lab <- label_occup(d15n$fff)
d15l$fff_lab <- label_occup(d15l$fff)




/Second type. Classification on decision-making and autonomy (SDMR2015)

/Indexes of decision-making and autonomy
/There are a number of questions in the study that gauge the level of participation in the decision-making. Each question defines individual input into decision-making using a three-point scale: decision taken by the respondent without prior consultations with his or her superiors, decision taken in consort with other employees and no influence upon decisions in the designated area. The questions serves as the basis for an index of decision-making (DM index) normalized to range from 1 to 100.  The autonomy index (AUT-index) is measured in the same way as the decision-making power.

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
