#' This script was used to convert the R-scripts I had translated from Oksanas stata-scripts into
#' r-scripts with variables are recoded based on labels. This is to have recoding understandable for humans


d <- readLines(con = "./code/professions_into_labeles.R")

load("./data/label_data.RData")
ld <- label_data[label_data$code %in% "V14_CODE",]
library(stringr)

for (i in 1:nrow(ld)){
  d <- str_replace_all(string = d, pattern = paste0("[\\s|,|\\(]",ld[i,"values"],"[\\s|,|\\)|\\]]"), 
                       replacement = paste0('"',ld[i,"labels"],'",'))
}
writeLines(text = d, con = "./code/professions_into_labeles2.R")

# Manuaalisesti muutin vaiks mitä

#professions_into_labeles2.R- >professions_into_labeles2_edit.R

d <- readLines(con = "./code/professions_into_labeles2_edit.R")

for (i in 1:nrow(ld)){
  d <- str_replace_all(string = d, pattern = '","', 
                       replacement = '",\n"')
}
writeLines(text = d, con = "./code/professions_into_labeles3.R")


# mnuaalisesti uudelleenkoodaan stringi
d <-  "5211,5212,5213,5214,5215,5216,5217,5218,5219,5220,5221,5222,5223,5224,5225,5226,5227,5228,5229,5230,5231,5232,5233,5234,5235,5236,5237,5238,5239,5240,5241,5242,5243,5244,5245"

d <- 'd$occr[d$V14_CODE %in% c(2212, 2221, 2222, 2223, 2224, 2220)] <- 1
d$occr[d$V14_CODE %in% c(2229, 2230, 3220, 3223, 3224, 3226, 3229, 3231, 3232)] <- 2
d$occr[d$V14_CODE %in% c(2411)] <- 3
d$occr[d$V14_CODE %in% c(2320, 2331, 2332, 2340, 2351, 2359, 3310, 3320, 3330, 3340, 2300, 2330, 2350, 3300)] <- 4
d$occr[d$V14_CODE %in% c(2310, 2400, 2410, 2431, 2432, 2430)] <- 5
d$occr[d$V14_CODE %in% c(2211, 2212, 2213, 2412, 2419, 2441, 2442, 2443, 2444, 2445, 2000, 2210, 2440)] <- 5.5
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
d$occr[d$V14_CODE %in% c(1221, 1222, 1226, 1227, 1228, 1229, 1223, 1224, 1311, 1312, 1313, 1200, 1220, 1300, 1310)] <- 17
d$occr[d$V14_CODE %in% c(7111, 7112, 7113, 7121,7122,7123,7124,7125,7126,7127,7128,7129,7130,7131,7132,7133,7134,7135,7136,7137, 
7139,7140,7141,7142,7143, 7211,7212,7213,7214,7215,7216, 7221,7222,7223,7224, 7231,7232,7233,7234,7235,7236,7237,7238,7239,7240,7241,7242,7243,7244,7245,
7311,7312,7313, 7321,7322,7323,7324, 7331, 7332, 7341,7342,7343, 7345, 7346, 7411,7412,7413, 7415, 7421,
7422, 7433,7434,7435,7436,7437, 7441, 7442, 8124, 7000, 7100, 7110, 7120, 7200, 7210, 7220, 7230, 7300, 7310,
7320, 7330, 7340, 7400)] <- 18
d$occr[d$V14_CODE %in% c(3441,3442,3443,3444, 3449, 3450, 5161,5162,5163, 5169, 0100, 5160)] <- 19
d$occr[d$V14_CODE %in% c(5112, 8311, 8312, 8321,8322,8323,8324, 8331,8332,8333,8334, 8340, 8300, 8310, 8320, 8330)] <- 20
d$occr[d$V14_CODE %in% c(7344, 7414, 7416, 7423, 7424, 7431, 7432, 8111, 8112, 8113, 8121,8122,8123, 8131, 8139,8140,8141,8142,8143,
8151,8152,8153,8154,8155, 
8159,8160,8161,8162,8163, 
8170, 8211, 8212, 
8221,8222,8223,8224, 
8229,8230,8231,8232, 8240, 
8251,8252,8253,
8261,8262,8263,8264,8265,8266, 
8269,8270,8271,8272,8273,8274,8275,8276,8277,8278,8279,8280,8281,8282,8283,8284,8285,8286,8287, 
8290, 9320, 7410, 7420, 7430, 8000, 8100, 8110, 8120, 8130, 8150, 8200,
8210, 8220, 8250, 8260)] <- 21
d$occr[d$V14_CODE %in% c(6121,6122,6123,6124,6125,6126,6127,6128,6129, 
6142, 9142, 9161,
9311,9312,9313, 
9330, 9300, 9310)] <- 22
d$occr[d$V14_CODE %in% c(6141, 
6151,6152,6153,6154, 
9211, 9212, 9213, 6140, 6150, 9200, 9210)] <- 23
d$occr[d$V14_CODE %in% c(3221, 3222, 3225, 3227, 3228)] <- 24
d$occr[d$V14_CODE %in% c(5113, 5122, 5141, 5140)] <- 25
d$occr[d$V14_CODE %in% c(5111, 5121, 
5131,5132,5133,5134,5135,5136,5137,5138,5139, 
5142, 5143, 5149, 9120, 
9131,9132,9133, 
9141, 
9151,9152,9153, 
9162, 5000,
5120, 5123, 5130, 9000,9130, 9140, 9150, 9160)] <- 26
d$occr[d$V14_CODE %in% c(6121,6122,6123,6124,6125,6126,6127,6128,6129,6130, 
6111, 6112, 6000, 6100, 6110, 6120)] <- 27
d$occr[d$V14_CODE %in% c(1100)] <- 11
d$occr[d$V14_CODE %in% c(1230)] <- 12
d$occr[d$V14_CODE %in% c(2420)] <- 9
d$occr[d$V14_CODE %in% c(3230)] <- 2
d$occr[d$V14_CODE %in% c(3433)] <- 14
d$occr[d$V14_CODE %in% c(3440)] <- 19
d$occr[d$V14_CODE %in% c(5100)] <- NA
'



for (i in 1:nrow(ld)){
  d <- str_replace_all(string = d, pattern = paste0("[\\s|,|\\(]",ld[i,"values"],"[\\s|,|\\)|\\]]"), 
                       replacement = paste0('"',ld[i,"labels"],'",'))
}
d <- str_replace_all(string = d, pattern = ",", 
                       replacement = ",\n")

writeLines(d, "./dd.R")
# tulos

# [1] "5211\"Street food salespersons\",5213,5214,5215,5216,5217,5218,5219,5220\"Shop keepers\"\"Shop supervisors\"\"Shop sales assistants\",5224,5225,5226,5227,5228,5229\"Cashiers and ticket clerks\",5231,5232,5233,5234,5235,5236,5237,5238,5239,5240\"Fashion and other models\"\"Sales demonstrators\"\"Door to door salespersons\"\"Contact centre salespersons\",5245"

# käännetään occr occupcoodit


d <- "d$occr[d$V14_CODE %in% c(2212)] <- 'Physicians and dentists'
d$occr[d$V14_CODE %in% c(2221)] <- 'Physicians and dentists'
d$occr[d$V14_CODE %in% c(2222)] <- 'Physicians and dentists'
d$occr[d$V14_CODE %in% c(2223)] <- 'Physicians and dentists'
d$occr[d$V14_CODE %in% c(2224)] <- 'Physicians and dentists'
d$occr[d$V14_CODE %in% c(2220)] <- 'Physicians and dentists'

d$occr[d$V14_CODE %in% c(2229)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(2230)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3220)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3223)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3224)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3226)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3229)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3231)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3232)] <- 'Other medical and paramedical'

d$occr[d$V14_CODE %in% c(2229)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(2230)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3220)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3223)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3224)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3226)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3229)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3231)] <- 'Other medical and paramedical'
d$occr[d$V14_CODE %in% c(3232)] <- 'Other medical and paramedical'

d$occr[d$V14_CODE %in% c(2411)] <- 'Accountants, auditors, actuaries'

d$occr[d$V14_CODE %in% c(2320)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2331)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2332)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2340)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2351)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2359)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(3310)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(3320)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(3330)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(3340)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2300)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2330)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(2350)] <- 'Teachers: elementary and secondary'
d$occr[d$V14_CODE %in% c(3300)] <- 'Teachers: elementary and secondary'

d$occr[d$V14_CODE %in% c(2310)] <- 'Teachers: university, social sc, librarians'
d$occr[d$V14_CODE %in% c(2400)] <- 'Teachers: university, social sc, librarians'
d$occr[d$V14_CODE %in% c(2410)] <- 'Teachers: university, social sc, librarians'
d$occr[d$V14_CODE %in% c(2431)] <- 'Teachers: university, social sc, librarians'
d$occr[d$V14_CODE %in% c(2432)] <- 'Teachers: university, social sc, librarians'
d$occr[d$V14_CODE %in% c(2430)] <- 'Teachers: university, social sc, librarians'

d$occr[d$V14_CODE %in% c(2211)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2212)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2213)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2412)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2419)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2441)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2442)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2443)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2444)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2445)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2000)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2210)] <- 'Other univ professionals'
d$occr[d$V14_CODE %in% c(2440)] <- 'Other univ professionals'

d$occr[d$V14_CODE %in% c(2111)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2112)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2113)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2121)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2122)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2131)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2139)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2141)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2142)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2143)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2144)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2145)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2146)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2147)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2148)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2149)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(3112)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(3141)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(3434)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2100)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2110)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2114)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2130)] <- 'Mathematicians, engineers etc'
d$occr[d$V14_CODE %in% c(2140)] <- 'Mathematicians, engineers etc'

d$occr[d$V14_CODE %in% c(3114)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3117)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3118)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3121)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3122)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3123)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3131)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3132)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3133)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3139)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3142)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3143)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3144)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3145)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3211)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3212)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3213)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3111)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3113)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3115)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3116)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3119)] <- 'Technicians etc'
d$occr[d$V14_CODE %in% c(3151)] <- 'Technicians etc'

d$occr[d$V14_CODE %in% c(2446)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(2460)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(2470)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(3423)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(3429)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(3432)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(3460)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(3480)] <- 'Public advisors'
d$occr[d$V14_CODE %in% c(5150)] <- 'Public advisors'

d$occr[d$V14_CODE %in% c(2421)] <- 'Lawyers and judges'
d$occr[d$V14_CODE %in% c(2422)] <- 'Lawyers and judges'
d$occr[d$V14_CODE %in% c(2429)] <- 'Lawyers and judges'

d$occr[d$V14_CODE %in% c(2451)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(2452)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(2453)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(2454)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(2455)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3471)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3472)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3473)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3474)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3475)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(5210)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(2450)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3470)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3000)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3100)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3110)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3120)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3130)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3140)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3150)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3200)] <- 'Arts and entertainment'
d$occr[d$V14_CODE %in% c(3210)] <- 'Arts and entertainment'

d$occr[d$V14_CODE %in% c(1110)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(1141)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(1142)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(1143)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(2352)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(3152)] <- 'Managers: public and quasi-public'
d$occr[d$V14_CODE %in% c(1140)] <- 'Managers: public and quasi-public'

d$occr[d$V14_CODE %in% c(1210)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1231)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1232)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1234)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1235)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1236)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1237)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1238)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1239)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1316)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1317)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1318)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(1319)] <- 'Managers: corporate'
d$occr[d$V14_CODE %in% c(3416)] <- 'Managers: corporate'

d$occr[d$V14_CODE %in% c(1225)] <- 'Managers: other'
d$occr[d$V14_CODE %in% c(1233)] <- 'Managers: other'
d$occr[d$V14_CODE %in% c(1314)] <- 'Managers: other'
d$occr[d$V14_CODE %in% c(1315)] <- 'Managers: other'

d$occr[d$V14_CODE %in% c(3431)] <- 'Secretaries'
d$occr[d$V14_CODE %in% c(3233)] <- 'Secretaries'
d$occr[d$V14_CODE %in% c(4115)] <- 'Secretaries'

d$occr[d$V14_CODE %in% c(4111)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4112)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4113)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4114)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4121)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4122)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4131)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4132)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4133)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4141)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4142)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4143)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4144)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4190)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4211)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4212)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4213)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4214)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4215)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4221)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4222)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4223)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(3430)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4000)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4100)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4110)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4120)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4130)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4140)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4200)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4210)] <- 'Other clerical'
d$occr[d$V14_CODE %in% c(4220)] <- 'Other clerical'

d$occr[d$V14_CODE %in% c(3411)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3412)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3413)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3414)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3415)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3417)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3419)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3421)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3422)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5220)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5221)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5222)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5223)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5230)] <- 'Sales'
d$occr[d$V14_CODE %in% c(9111)] <- 'Sales'
d$occr[d$V14_CODE %in% c(9113)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3400)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3410)] <- 'Sales'
d$occr[d$V14_CODE %in% c(3420)] <- 'Sales'
d$occr[d$V14_CODE %in% c(5200)] <- 'Sales'
d$occr[d$V14_CODE %in% c(9100)] <- 'Sales'
d$occr[d$V14_CODE %in% c(9110)] <- 'Sales'


d$occr[d$V14_CODE %in% c(1221)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1222)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1226)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1227)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1228)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1229)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1223)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1224)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1311)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1312)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1313)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1200)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1220)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1300)] <- 'Foremen'
d$occr[d$V14_CODE %in% c(1310)] <- 'Foremen'


d$occr[d$V14_CODE %in% c(7111)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7112)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7113)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7121)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7122)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7123)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7124)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7125)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7126)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7127)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7128)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7129)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7130)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7131)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7132)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7133)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7134)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7135)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7136)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7137)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7139)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7140)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7141)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7142)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7143)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7211)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7212)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7213)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7214)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7215)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7216)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7221)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7222)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7223)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7224)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7231)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7232)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7233)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7234)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7235)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7236)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7237)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7238)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7239)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7240)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7241)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7242)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7243)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7244)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7245)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7311)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7312)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7313)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7321)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7322)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7323)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7324)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7331)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7332)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7341)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7342)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7343)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7345)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7346)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7411)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7412)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7413)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7415)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7421)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7422)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7433)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7434)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7435)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7436)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7437)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7441)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7442)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(8124)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7000)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7100)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7110)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7120)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7200)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7210)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7220)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7230)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7300)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7310)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7320)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7330)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7340)] <- 'Crafts'
d$occr[d$V14_CODE %in% c(7400)] <- 'Crafts'

d$occr[d$V14_CODE %in% c(3441)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(3442)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(3443)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(3444)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(3449)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(3450)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(5161)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(5162)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(5163)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(5169)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(0100)] <- 'Government protective workers'
d$occr[d$V14_CODE %in% c(5160)] <- 'Government protective workers'

d$occr[d$V14_CODE %in% c(5112)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8311)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8312)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8321)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8322)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8323)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8324)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8331)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8332)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8333)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8334)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8340)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8300)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8310)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8320)] <- 'Transportation workers'
d$occr[d$V14_CODE %in% c(8330)] <- 'Transportation workers'

d$occr[d$V14_CODE %in% c(7344)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7414)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7416)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7423)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7424)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7431)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7432)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8111)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8112)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8113)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8121)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8122)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8123)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8131)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8139)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8140)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8141)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8142)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8143)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8151)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8152)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8153)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8154)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8155)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8159)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8160)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8161)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8162)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8163)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8170)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8211)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8212)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8221)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8222)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8223)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8224)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8229)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8230)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8231)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8232)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8240)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8251)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8252)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8253)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8261)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8262)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8263)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8264)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8265)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8266)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8269)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8270)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8271)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8272)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8273)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8274)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8275)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8276)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8277)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8278)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8279)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8280)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8281)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8282)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8283)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8284)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8285)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8286)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8287)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8290)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(9320)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7410)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7420)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(7430)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8000)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8100)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8110)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8120)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8130)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8150)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8200)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8210)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8220)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8250)] <- 'Operatives, except transportation'
d$occr[d$V14_CODE %in% c(8260)] <- 'Operatives, except transportation'

d$occr[d$V14_CODE %in% c(6121)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6122)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6123)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6124)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6125)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6126)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6127)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6128)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6129)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(6142)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9142)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9161)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9311)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9312)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9313)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9330)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9300)] <- 'Laborers, except farm laborers'
d$occr[d$V14_CODE %in% c(9310)] <- 'Laborers, except farm laborers'

d$occr[d$V14_CODE %in% c(6141)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6151)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6152)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6153)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6154)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(9211)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(9212)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(9213)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6140)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(6150)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(9200)] <- 'Farm Workers'
d$occr[d$V14_CODE %in% c(9210)] <- 'Farm Workers'

d$occr[d$V14_CODE %in% c(3221)] <- 'White collar services'
d$occr[d$V14_CODE %in% c(3222)] <- 'White collar services'
d$occr[d$V14_CODE %in% c(3225)] <- 'White collar services'
d$occr[d$V14_CODE %in% c(3227)] <- 'White collar services'
d$occr[d$V14_CODE %in% c(3228)] <- 'White collar services'

d$occr[d$V14_CODE %in% c(5113)] <- 'Skilled manual services'
d$occr[d$V14_CODE %in% c(5122)] <- 'Skilled manual services'
d$occr[d$V14_CODE %in% c(5141)] <- 'Skilled manual services'
d$occr[d$V14_CODE %in% c(5140)] <- 'Skilled manual services'

d$occr[d$V14_CODE %in% c(5111)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5121)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5131)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5132)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5133)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5134)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5135)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5136)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5137)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5138)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5139)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5142)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5143)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5149)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9120)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9131)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9132)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9133)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9141)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9151)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9152)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9153)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9162)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5000)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5120)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5123)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(5130)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9000)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9130)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9140)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9150)] <- 'Lowskilled services'
d$occr[d$V14_CODE %in% c(9160)] <- 'Lowskilled services'

d$occr[d$V14_CODE %in% c(6121)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6122)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6123)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6124)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6125)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6126)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6127)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6128)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6129)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6130)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6111)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6112)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6000)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6100)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6110)] <- 'Farmers and related profession'
d$occr[d$V14_CODE %in% c(6120)] <- 'Farmers and related profession'

d$occr[d$V14_CODE %in% c(1100)] <- 'Managers: public and quasi-public'

d$occr[d$V14_CODE %in% c(1230)] <- 'Managers: corporate'

d$occr[d$V14_CODE %in% c(2420)] <- 'Lawyers and judges'

d$occr[d$V14_CODE %in% c(3230)] <- 'Other medical and paramedical'

d$occr[d$V14_CODE %in% c(3433)] <- 'Secretaries'

d$occr[d$V14_CODE %in% c(3440)] <- 'Government protective workers'

d$occr[d$V14_CODE %in% c(5100)] <- NA
"

for (i in 1:nrow(ld)){
  d <- str_replace_all(string = d, pattern = paste0("[\\s|,|\\(]",ld[i,"values"],"[\\s|,|\\)|\\]]"), 
                       replacement = paste0('"',ld[i,"labels"],'",'))
}
d <- str_replace_all(string = d, pattern = ",", 
                     replacement = ",\n")

writeLines(d, "./doccr.R")


