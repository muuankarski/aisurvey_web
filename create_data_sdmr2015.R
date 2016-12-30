#' ---
#' title: Construct 2015 data 
#' output: 
#'   html_document: 
#'    toc: true
#'    toc_float: true
#'    number_sections: yes
#'    code_folding: show
#' ---

#+ knitr_setup, include=FALSE
library(knitr)
opts_chunk$set(list(echo=FALSE,eval=FALSE,cache=FALSE,warning=FALSE,message=FALSE))

#' This is a script to construct up-to-date version of SDMR wave 2015 data from 
#' `structure_2015_with_class.sav` -data
#' 
#' # Details of the raw data

#+ rawdetail
d <- haven::read_sav("~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_2015_with_class.sav")

dim(d)
writeLines(capture.output(str(d)), con = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_2015_with_class_str.txt")

saveRDS(d, file = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_2015_with_class.RDS")
#' 
#' [Check the output of ´str(d)`](./data/structure_2015_with_class_str.txt)
#' 

#' # Creating a metadata based on variable attributes

#+ meta_df, results="asis"

# 

var_label(d$age) <- "Respondents age"

library(tidyverse)
library(labelled)
# meta_df <- data_frame()
# for (i in 1:ncol(d)){
#   df <- data_frame()
#   code  <- names(d[i])
#   name <- attributes(d[[i]])$label
#   label <- names(attributes(d[[i]])$label)
#   if (is.null(label)){
#     value = NA
#     label=NA
#   } else {
#     value = attributes(d[[i]])$label
#     names(value) <- NULL
#   }
#   if (is.null(name)) name="not applicaple"
#   class <- ifelse(is.na(value), "numeric", "factor")
#   if (class == "numeric") class <- ifelse(class(d[[i]]) %in% "numeric", "numeric", "character") 
#   new_row <- data_frame(code=code,
#                         name=name,
#                         label=label,
#                         value=value,
#                         class=class)
#   meta_df <- rbind(meta_df,new_row)
# }

label_data <- data.frame()
for (i in 1:ncol(d)){
  df <- data.frame()
  code  <- names(d[i])
  name <- attributes(d[[i]])$label
  label <- names(attributes(d[[i]])$labels)
  if (is.null(label)){
    value = NA
    label=NA
  } else {
    value = as.integer(attributes(d[[i]])$labels)
    names(value) <- NULL
  }
  if (is.null(name)) name="not applicaple"
  class <- ifelse(is.na(value), "numeric", "factor")
  if (class == "numeric") class <- ifelse(class(d[[i]]) %in% "numeric", "numeric", "character") 
  df <- data.frame(code=code,
                   name=name,
                   label=label,
                   value=value,
                   class=class, stringsAsFactors=FALSE)
  label_data <- rbind(label_data,df)
}
meta_df <- label_data

dim(meta_df)
knitr::kable(meta_df[1:20,])

#' Now we have a data frame with separate wor for each value in each variable
#' 
#' # Strip variable attributes before recoding the variables in R
#' 
#' It is complicated to manipulate data with variable label in R, therefore we strip off the variable 
#' label and create a function to label the variables when that is needed.

#+ remove_attributes
for (i in 1:ncol(d)) {
  z<-class(d[[i]])
  if (z[[1]]=='labelled'){
    class(d[[i]]) <- z[-1]
    attr(d[[i]],'label')<-NULL
  }
  attr(d[[i]],'names')<-NULL
  attr(d[[i]],'label')<-NULL
  attr(d[[i]],'format.stata')<-NULL
  attr(d[[i]],'format.spss')<-NULL
}

d$V74c1[d$V74c1 == 9999998] <- NA
d$V74c1[d$V74c1 == 9999999] <- NA


d$V75c1[d$V75c1 == 9999998] <- NA
d$V75c1[d$V75c1 == 9999999] <- NA

dim(d)
writeLines(capture.output(str(d)), con = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_2015_with_class_stripped_str.txt")

saveRDS(d, file = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_2015_with_class_stripped.RDS")
#' 
#' [Check the output of ´str(d)`](./data/structure_2015_with_class_stripped_str.txt)
#' 
#' 
#' # Add new class variables
#' 
#' 
#' ## Occupational groups

#+ construct_classes_occup_groups, results="asis"

library(tidyverse)
library(ggplot2)

# source("~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/code/label_data.R")

label_data <- function(data=d, variable="tvtot", metadata=meta_df, into.factor=TRUE){
  if (!"factor" %in% unique(metadata[metadata$code %in% variable,]$class)) stop("Variable is either character or numeric and has no labels")
  vardata <- metadata[metadata$code %in% variable,]
  new_values <- with(vardata, label[match(data[[variable]], value)])
  if (into.factor) new_values <- factor(new_values, levels=vardata$label)
  return(new_values)
}

d$V14_CODE_labeled <- label_data(data = d, variable = "V14_CODE", metadata = meta_df, into.factor = FALSE)

# Occupational groups
d[["occup_groups_labeled"]] <- NA
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Legislators")]                                                                  = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Senior government officials")]                                                  = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional chiefs and heads of village")]                                      = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Senior officials of special-interest organizations")]                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Managing directors and chief executives")]                                      = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural and forestry production managers")]                                = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aquaculture and fisheries production managers")]                                = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Manufacturing managers")]                                                       = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining managers")]                                                              = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Construction managers")]                                                        = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Supply, distribution and related managers")]                                    = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Health services managers")]                                                     = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aged care services managers")]                                                  = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Social welfare managers")]                                                      = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Education managers")]                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Financial and insurance services branch managers")]                             = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Professional services managers not elsewhere classified")]                      = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Restaurant managers")]                                                          = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Retail and wholesale trade managers")]                                          = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sports, recreation and cultural centre managers")]                              = "13-Production and specialized services managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Meteorologists","Chemists")]                                                    = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Geologists and geophysicists")]                                                 = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mathematicians, actuaries and statisticians")]                                  = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Biologists, botanists, zoologists and related professionals")]                  = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Farming, forestry and fisheries advisers")]                                     = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental protection professionals")]                                       = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building architects")]                                                          = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Landscape architects")]                                                         = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Product and garment designers")]                                                = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Town and traffic planners")]                                                    = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cartographers and surveyors")]                                                  = "21-Science and engineering professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Industrial and production engineers")]                                          = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineers")]                                                              = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental engineers")]                                                      = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical engineers")]                                                         = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical engineers")]                                                           = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining engineers, metallurgists and related professionals")]                    = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Engineering professionals not elsewhere classified")]                           = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical engineers")]                                                         = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics engineers")]                                                        = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Telecommunications engineers")]                                                 = "3-Technicians and associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Specialist medical practitioners")]                                             = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Nursing professionals")]                                                        = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Midwifery professionals")]                                                      = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dentists")]                                                                     = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pharmacists")]                                                                  = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental and occupational health and hygiene professionals")]              = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physiotherapists")]                                                             = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dieticians and nutritionists")]                                                 = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Audiologists and speech therapists")]                                           = "22-Health professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("University and higher education teachers")]                                     = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Vocational education teachers")]                                                = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Secondary education teachers")]                                                 = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Primary school teachers")]                                                      = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Early childhood educators")]                                                    = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accountants")]                                                                  = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Financial and investment advisers")]                                            = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Financial analysts")]                                                           = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Management and organization analysts")]                                         = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Policy administration professionals")]                                          = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Personnel and careers professionals")]                                          = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Training and staff development professionals")]                                 = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Advertising and marketing professionals")]                                      = "24-Business and administration professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Software developers")]                                                          = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Web and multimedia developers")]                                                = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Applications programmers")]                                                     = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Software and applications developers and analysts not elsewhere classified")]   = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Database designers and administrators")]                                        = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Systems administrators")]                                                       = "25-Information and communications technology professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Lawyers")]                                                                      = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Judges")]                                                                       = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Legal professionals not elsewhere classified")]                                 = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Archivists and curators")]                                                      = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Librarians and related information professionals")]                             = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Economists")]                                                                   = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sociologists, anthropologists and related professionals")]                      = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Philosophers, historians and political scientists")]                            = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Psychologists")]                                                                = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Social work and counselling professionals")]                                    = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Religious professionals")]                                                      = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Authors and related writers")]                                                  = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Journalists")]                                                                  = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Translators, interpreters and other linguists")]                                = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Visual artists")]                                                               = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Musicians, singers and composers")]                                             = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dancers and choreographers")]                                                   = "26-Legal, social and cultural professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical and physical science technicians")]                                    = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineering technicians")]                                                = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical engineering technicians")]                                           = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics engineering technicians")]                                          = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical engineering technicians")]                                           = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical engineering technicians")]                                             = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining and metallurgical technicians")]                                         = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Draughtspersons")]                                                              = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physical and engineering science technicians not elsewhere classified")]        = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining supervisors")]                                                           = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Manufacturing supervisors")]                                                    = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Construction supervisors")]                                                     = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Power production plant operators")]                                             = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Incinerator and water treatment plant operators")]                              = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical processing plant controllers")]                                        = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Petroleum and natural gas refining plant operators")]                           = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal production process controllers")]                                         = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Process control technicians not elsewhere classified")]                         = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Life science technicians (excluding medical)")]                                 = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural technicians")]                                                     = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Forestry technicians")]                                                         = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ships engineers")]                                                              = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ships deck officers and pilots")]                                               = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aircraft pilots and related associate professionals")]                          = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Air traffic controllers")]                                                      = "31-Science and engineering associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical imaging and therapeutic equipment technicians")]                        = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical and pathology laboratory technicians")]                                 = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pharmaceutical technicians and assistants")]                                    = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical and dental prosthetic technicians")]                                    = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3215)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3216)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3217)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3218)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3219)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3220)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Nursing associate professionals")]                                              = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Midwifery associate professionals")]                                            = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3223)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3224)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3225)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3226)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3227)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3228)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3229)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional and complementary medicine associate professionals")]               = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3231)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3232)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3233)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3234)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3235)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3236)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3237)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3238)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3239)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Veterinary technicians and assistants")]                                        = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3241)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3242)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3243)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3244)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3245)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3246)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3247)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3248)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3249)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3250)]                                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dental assistants and therapists")]                                             = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical records and health information technicians")]                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Community health workers")]                                                     = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dispensing opticians")]                                                         = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physiotherapy technicians and assistants")]                                     = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical assistants")]                                                           = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental and occupational health inspectors and associates")]              = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ambulance workers")]                                                            = "32-Health associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Employment agents and contractors")]                                            = "12-Administrative and commercial managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Real estate agents and property managers")]                                     = "12-Administrative and commercial managers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Office supervisors")]                                                           = "4-Clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Legal secretaries")]                                                            = "4-Clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Administrative and executive secretaries")]                                     = "4-Clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical secretaries")]                                                          = "4-Clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Securities and finance dealers and brokers")]                                   = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Credit and loans officers")]                                                    = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accounting associate professionals")]                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Statistical, mathematical and related associate professionals")]                = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Valuers and loss assessors")]                                                   = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3316)]                                                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3317)]                                                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3318)]                                                                          = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3319)]                                                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3320)]                                                                          = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Insurance representatives")]                                                    = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Commercial sales representatives")]                                             = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Buyers")]                                                                       = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Customs and border inspectors")]                                                = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government tax and excise officials")]                                          = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government social benefits officials")]                                         = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government licensing officials")]                                               = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Police inspectors and detectives")]                                             = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3356)]                                                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3357)]                                                                           = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3358)]                                                                          = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Regulatory government associate professionals not elsewhere classified")]       = "33-Business and administration associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Police inspectors and detectives")]                                             = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Social work associate professionals")]                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Religious associate professionals")]                                            = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3414)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3415)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3416)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3417)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3418)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3419)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3420)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Athletes and sports players")]                                                  = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sports coaches, instructors and officials")]                                    = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fitness and recreation instructors and program leaders")]                       = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3424)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3425)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3426)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3427)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3428)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3429)]                                                                           = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3430)]                                                                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Photographers")]                                                                = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Interior designers and decorators")]                                            = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Gallery, museum and library technicians")]                                      = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chefs")]                                                                        = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other artistic and cultural associate professionals")]                          = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information and communications technology operations technicians")]             = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information and communications technology user support technicians")]           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Computer network and systems technicians")]                                     = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Web technicians")]                                                              = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3515)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3516)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3517)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3518)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3519)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(3520)]                                                                           = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Broadcasting and audio-visual technicians")]                                    = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("General office clerks")]                                                        = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4111)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4112)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4113)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4114)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4115)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4116)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4117)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4118)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4119)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Secretaries (general)")]                                                        = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4121)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4122)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4123)]                                                                          = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4124)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4125)]                                                                          = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4126)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4127)]                                                                          = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4128)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4129)]                                                                          = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4130)]                                                                           = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Typists and word processing operators")]                                        = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Data entry clerks")]                                                            = "41-General and keyboard clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4210)]                                                                           = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bank tellers and related clerks")]                                              = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bookmakers, croupiers and related gaming workers")]                             = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pawnbrokers and money-lenders")]                                                = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Debt-collectors and related workers")]                                          = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4215)]                                                                           = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4216)]                                                                           = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4217)]                                                                          = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4218)]                                                                           = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4219)]                                                                          = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4220)]                                                                           = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel consultants and clerks")]                                                = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Contact centre information clerks")]                                            = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Telephone switchboard operators")]                                              = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hotel receptionists")]                                                          = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Enquiry clerks")]                                                               = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Receptionists (general)")]                                                      = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Survey and market research interviewers")]                                      = "42-Customer services clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accounting and bookkeeping clerks")]                                            = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Statistical, finance and insurance clerks")]                                    = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Payroll clerks")]                                                               = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4314)]                                                                           = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4315)]                                                                           = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4316)]                                                                          = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4317)]                                                                           = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4318)]                                                                          = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4319)]                                                                           = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4320)]                                                                          = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stock clerks")]                                                                 = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Production clerks")]                                                            = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Transport clerks")]                                                             = "43-Numerical and material recording clerks"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Library clerks")]                                                               = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mail carriers and sorting clerks")]                                             = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Coding, proof-reading and related clerks")]                                     = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Scribes and related workers")]                                                  = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Filing and copying clerks")]                                                    = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Personnel clerks")]                                                             = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4417)]                                                                           = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(4418)]                                                                           = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Clerical support workers not elsewhere classified")]                            = "44-Other clerical support workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel attendants and travel stewards")]                                        = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Transport conductors")]                                                         = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel guides")]                                                                = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5114)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5115)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5116)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5117)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5118)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5119)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cooks")]                                                                        = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5121)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5122)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5123)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5124)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5125)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5126)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5127)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5128)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5129)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5130)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Waiters")]                                                                      = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bartenders")]                                                                   = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5133)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5134)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5135)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5136)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5137)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5138)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5139)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5140)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hairdressers")]                                                                 = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Beauticians and related workers")]                                              = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5143)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5144)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5145)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5146)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5147)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5148)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5149)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5150)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cleaning and housekeeping supervisors in offices, hotels")]                     = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Domestic housekeepers")]                                                        = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building caretakers")]                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5154)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5155)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5156)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5157)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5158)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5159)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5160)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Astrologers, fortune-tellers and related workers")]                             = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Companions and valets")]                                                        = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Undertakers and embalmers")]                                                    = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pet groomers and animal care workers")]                                         = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Driving instructors")]                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5166)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5167)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5168)]                                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Personal services workers not elsewhere classified")]                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stall and market salespersons")]                                                = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Street food salespersons")]                                                     = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5213)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5214)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5215)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5216)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5217)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5218)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5219)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5220)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop keepers")]                                                                 = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop supervisors")]                                                             = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop sales assistants")]                                                        = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5224)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5225)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5226)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5227)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5228)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5229)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cashiers and ticket clerks")]                                                   = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5231)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5232)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5233)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5234)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5235)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5236)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5237)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5238)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5239)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5240)]                                                                           = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fashion and other models")]                                                     = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sales demonstrators")]                                                          = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Door to door salespersons")]                                                    = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Contact centre salespersons")]                                                  = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Service station attendants")]                                                   = "52-Sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Child care workers")]                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Teachers aides")]                                                               = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5313)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5314)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5315)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5316)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5317)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5318)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5319)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5320)]                                                                           = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Health care assistants")]                                                       = "53-Personal care workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fire-fighters")]                                                                = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Police officers")]                                                              = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Prison guards")]                                                                = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Security guards")]                                                              = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5415)]                                                                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5416)]                                                                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5417)]                                                                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(5418)]                                                                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Protective services workers not elsewhere classified")]                         = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Forestry and related workers")]                                                 = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6211)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6212)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6213)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6214)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6215)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6216)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6217)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6218)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6219)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(6220)]                                                                           = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aquaculture workers")]                                                          = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Inland and coastal waters fishery workers")]                                    = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Deep-sea fishery workers")]                                                     = "6-Skilled agricultural, forestry and fishery workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("House builders")]                                                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bricklayers and related workers")]                                              = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stonemasons, stone cutters, splitters and carvers")]                            = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Concrete placers, concrete finishers and related workers")]                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Carpenters and joiners")]                                                       = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7116)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7117)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7118)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building frame and related trades workers not elsewhere classified")]           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7120)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Roofers")]                                                                      = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Floor layers and tile setters")]                                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plasterers")]                                                                   = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Insulation workers")]                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glaziers")]                                                                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plumbers and pipe fitters")]                                                    = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Air conditioning and refrigeration mechanics")]                                 = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7128)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7129)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7130)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Painters and related workers")]                                                 = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Spray painters and varnishers")]                                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building structure cleaners")]                                                  = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal moulders and coremakers")]                                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Welders and flamecutters")]                                                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sheet-metal workers")]                                                          = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Structural-metal preparers and erectors")]                                      = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Riggers and cable splicers")]                                                   = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7216)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7217)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7218)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7219)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7220)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Blacksmiths, hammersmiths and forging press workers")]                          = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Toolmakers and related workers")]                                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal working machine tool setters and operators")]                             = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal polishers, wheel grinders and tool sharpeners")]                          = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7225)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7226)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7227)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7228)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7229)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7230)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Motor vehicle mechanics and repairers")]                                        = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aircraft engine mechanics and repairers")]                                      = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural and industrial machinery mechanics and repairers")]                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bicycle and related repairers")]                                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Precision-instrument makers and repairers")]                                    = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Musical instrument makers and tuners")]                                         = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Jewellery and precious-metal workers")]                                         = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Potters and related workers")]                                                  = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glass makers, cutters, grinders and finishers")]                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sign writers, decorative painters, engravers and etchers")]                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Handicraft workers in wood, basketry and related materials")]                   = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Handicraft workers in textile, leather and related materials")]                 = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Handicraft workers not elsewhere classified")]                                  = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7320)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pre-press technicians")]                                                        = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Printers")]                                                                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Print finishing and binding workers")]                                          = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building and related electricians")]                                            = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical mechanics and fitters")]                                             = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical line installers and repairers")]                                     = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7414)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7415)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7416)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7417)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7418)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7419)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7420)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics mechanics and servicers")]                                          = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Butchers, fishmongers and related food preparers")]                             = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bakers, pastry-cooks and confectionery makers")]                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dairy-products makers")]                                                        = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fruit, vegetable and related preservers")]                                      = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Food and beverage tasters and graders")]                                        = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Tobacco preparers and tobacco products makers")]                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7517)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7518)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7519)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7520)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Wood treaters")]                                                                = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cabinet-makers and related workers")]                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Woodworking-machine tool setters and operators")]                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7524)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7525)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7526)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7527)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7528)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7529)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7530)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Tailors, dressmakers, furriers and hatters")]                                   = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Garment and related pattern-makers and cutters")]                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sewing, embroidery and related workers")]                                       = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Upholsterers and related workers")]                                             = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pelt dressers, tanners and fellmongers")]                                       = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shoemakers and related workers")]                                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7537)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7538)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7539)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7540)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Underwater divers")]                                                            = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shotfirers and blasters")]                                                      = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Product graders and testers (excluding foods and beverages)")]                  = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fumigators and other pest and weed controllers")]                               = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7545)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7546)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7547)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(7548)]                                                                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Craft and related workers not elsewhere classified")]                           = "7-Craft and related trades workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Miners and quarriers")]                                                         = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mineral and stone processing plant operators")]                                 = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Well drillers and borers and related workers")]                                 = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cement, stone and other mineral products machine operators")]                   = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8115)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8116)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8117)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8118)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8119)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8120)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal processing plant operators")]                                             = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal finishing, plating and coating machine operators")]                       = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8123)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8124)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8125)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8126)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8127)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8128)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8129)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8130)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical products plant and machine operators")]                                = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Photographic products machine operators")]                                      = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8133)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8134)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8135)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8136)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8137)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8138)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8139)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8140)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Rubber products machine operators")]                                            = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plastic products machine operators")]                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Paper products machine operators")]                                             = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8144)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8145)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8146)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8147)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8148)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8149)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8150)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fibre preparing, spinning and winding machine operators")]                      = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Weaving and knitting machine operators")]                                       = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sewing machine operators")]                                                     = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bleaching, dyeing and fabric cleaning machine operators")]                      = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fur and leather preparing machine operators")]                                  = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shoemaking and related machine operators")]                                     = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Laundry machine operators")]                                                    = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8158)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Textile, fur and leather products machine operators not elsewhere classified")] = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Food and related products machine operators")]                                  = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8161)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8162)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8163)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8164)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8165)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8166)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8167)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8168)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8169)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8170)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pulp and papermaking plant operators")]                                         = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Wood processing plant operators")]                                              = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8173)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8174)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8175)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8176)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8177)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8178)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8179)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8180)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glass and ceramics plant operators")]                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Steam engine and boiler operators")]                                            = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Packing, bottling and labelling machine operators")]                            = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8184)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8185)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8186)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8187)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8188)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stationary plant and machine operators not elsewhere classified")]              = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical machinery assemblers")]                                              = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical and electronic equipment assemblers")]                               = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8213)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8214)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8215)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8216)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8217)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8218)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Assemblers not elsewhere classified")]                                          = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Locomotive engine drivers")]                                                    = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Railway brake, signal and switch operators")]                                   = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8313)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8314)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8315)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8316)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8317)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8318)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8319)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8320)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Motorcycle drivers")]                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Car, taxi and van drivers")]                                                    = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8323)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8324)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8325)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8326)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8327)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8328)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8329)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8330)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bus and tram drivers")]                                                         = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Heavy truck and lorry drivers")]                                                = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8333)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8334)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8335)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8336)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8337)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8338)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8339)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(8340)]                                                                           = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mobile farm and forestry plant operators")]                                     = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Earthmoving and related plant operators")]                                      = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Crane, hoist and related plant operators")]                                     = "8-Plant and machine operators, and assemblers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cleaners and helpers in offices, hotels and other establishments")]             = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9113)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9114)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9115)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9116)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9117)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9118)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9119)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9120)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hand launderers and pressers")]                                                 = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Vehicle cleaners")]                                                             = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Window cleaners")]                                                              = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9124)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9125)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9126)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9127)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9128)]                                                                           = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other cleaning workers")]                                                       = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Kitchen helpers")]                                                              = "91-Cleaners and helpers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Crop farm labourers")]                                                          = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Livestock farm labourers")]                                                     = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineering labourers")]                                                  = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building construction labourers")]                                              = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9314)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9315)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9316)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9317)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9318)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9319)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9320)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hand packers")]                                                                 = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9322)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9323)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9324)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9325)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9326)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9327)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9328)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Manufacturing labourers not elsewhere classified")]                             = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9330)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hand and pedal vehicle drivers")]                                               = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Drivers of animal-drawn vehicles and machinery")]                               = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Freight handlers")]                                                             = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shelf fillers")]                                                                = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Kitchen helpers")]                                                              = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sweepers and related labourers")]                                               = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9614)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9615)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9616)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9617)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9618)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9619)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9620)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Messengers, package deliverers and luggage porters")]                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Odd job persons")]                                                              = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Meter readers and vending-machine collectors")]                                 = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Water and firewood collectors")]                                                = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9625)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9626)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9627)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9628)]                                                                           = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Elementary workers not elsewhere classified")]                                  = "92-Agricultural, forestry and fishery labourers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Not working")]                                                                  = NA
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(9998)]                                                                           = NA
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hard to say")]                                                                  = NA
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Commissioned armed forces officers")]                                           = "11-Chief executives, senior officials and legislators"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Non-commissioned armed forces officers")]                                       = "34-Legal, social, cultural and related associate professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Education methods specialists")]                                                = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Special needs teachers")]                                                       = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other language teachers")]                                                      = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other music teachers")]                                                         = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other arts teachers")]                                                          = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information technology trainers")]                                              = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(2357)]                                                                           = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c(2358)]                                                                           = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% c("Teaching professionals not elsewhere classified")]                              = "23-Teaching professionals"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% 3]                                                                                 = "35-Information and communications technicians"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% 52]                                                                                = "5-Service and sales workers"
d[["occup_groups_labeled"]][d[["V14_CODE_labeled"]] %in% 62]                                                                                = "6-Skilled agricultural, forestry and fishery workers"
d$occup_groups_labeled <- factor(d$occup_groups_labeled, levels=c("3-Technicians and associate professionals" ,
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

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "occup_groups_labeled"
unlabeled_varname <- "occup_groups"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d$occup_groups[i] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}
#% ------------------------------------

tbl <- as.data.frame(table(d$occup_groups_labeled, useNA="ifany"))
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

cat("\n\n")
cat("### Occupational codes of those missing")
cat("\n\n")
df_na <- d[is.na(d$occup_groups_labeled),c("V14_CODE_labeled")]
tbl <- as.data.frame(table(df_na))
tbl <- tbl[tbl$Freq > 0,]
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## Autonomia
#' 
#+ construct_classes_autonomia, results="asis"
# Some notes for next lines of code
## VV21c1 - Working in an organization or self-employed
### 1 In an organization
### 2 Self-employed

## V19c1 - Respondent’s current employment status
### 1 Working
### 2 Not working
d$autonomia_labeled <- NA
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% "12-Administrative and commercial managers"]             <- "1-managerial"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% "13-Production and specialized services managers"]       <- "1-managerial"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% "11-Chief executives, senior officials and legislators"] <- "1-managerial"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("91-Cleaners and helpers",
                                                    "6-Skilled agricultural, forestry and fishery workers")]                               <- "11-craftsman unqualified"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("21-Science and engineering professionals" ,
                                                    "22-Health professionals" ,
                                                    "23-Teaching professionals" ,
                                                    "24-Business and administration professionals" ,
                                                    "25-Information and communications technology professionals" ,
                                                    "26-Legal, social and cultural professionals")]      <- "5-professional"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("31-Science and engineering associate professionals" ,
                                                    "32-Health associate professionals" ,
                                                    "33-Business and administration associate professionals" ,
                                                    "34-Legal, social, cultural and related associate professionals" ,
                                                    "35-Information and communications technicians",
                                                    "3-Technicians and associate professionals")]         <- "6-scientific-technical"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% "53-Personal care workers"]                              <- "7-care work"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("4-Clerical support workers",
                                                    "41-General and keyboard clerks" ,
                                                    "42-Customer services clerks" ,
                                                    "43-Numerical and material recording clerks" ,
                                                    "44-Other clerical support workers")]                 <- "8-clerical"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("5-Service and sales workers","52-Sales workers")]                           <- "9-sales"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% c("7-Craft and related trades workers" ,
                                                    "8-Plant and machine operators, and assemblers" ,
                                                    "9-Elementary occupations")]                            <- "10-craftsman qualified"
d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$occup_groups_labeled %in% "92-Agricultural, forestry and fishery labourers"]                <- "10-craftsman qualified"

d$autonomia_labeled[d$V21c1 == 1 & # "In an organization"
                      d$V19c1==1 & # "Working"
                      d$V14_CODE_labeled %in% "Cooks"]                                                            <- "10-craftsman qualified"
d$autonomia_labeled[d$V21c1==2]                                                                             <- "12-entrepreneurs" # Self-employed
d$autonomia_labeled[d$V19c1==2]                                                                             <- "14-not working" # Not working

d$autonomia_labeled <- factor(d$autonomia_labeled, levels=c("1-managerial",
                                                            "5-professional",
                                                            "6-scientific-technical",
                                                            "7-care work",
                                                            "8-clerical",
                                                            "9-sales",
                                                            "10-craftsman qualified",
                                                            "11-craftsman unqualified",
                                                            "12-entrepreneurs",
                                                            "14-not working"))


#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "autonomia_labeled"
unlabeled_varname <- "autonomia"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d$occup_groups[i] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}
#% ------------------------------------



tbl <- as.data.frame(table(d$autonomia_labeled, useNA="ifany"))
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

cat("\n\n")
cat("### Occupational codes of those missing")
cat("\n\n")
df_na <- d[is.na(d$autonomia_labeled),c("V14_CODE_labeled")]
tbl <- as.data.frame(table(df_na))
tbl <- tbl[tbl$Freq > 0,]
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## Decision-making index (DMi)
#' 

#+ construct_classes_dmi, results="asis"
# # /Indexes of decision-making and autonomy
# # /There are a number of questions in the study that gauge the level of participation in the decision-making.
# #  Each question defines individual input into decision-making using a three-point scale:
# ## 1 decision taken by the respondent without prior consultations with his or her superiors,
# ## 2 decision taken in consort with other employees and no influence upon decisions in the designated area.
# # The questions serves as the basis for an index of decision-making (DM index) normalized to range from 1 to 100.
# # The autonomy index (AUT-index) is measured in the same way as the decision-making power.
d$v51_whole_for_13 <- rowSums(d[c("V51_1c1",  "V51_2c1", "V51_3c1", "V51_4c1", "V51_5c1",
                                  "V51_6c1", "V51_7c1", "V51_8c1", "V51_9c1", "V51_10c1",
                                  "V51_11c1", "V51_12c1", "V51_13c1")],na.rm = TRUE)
d$DMi_for13 <- d$v51_whole_for_13/39*100


ggplot(d, aes(DMi_for13)) + geom_density()

tbl <- as.data.frame(prop.table(table(d$DMi_for13))*100)
tbl$Freq <- round(tbl$Freq,1)

print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## The autonomy index (AUT-index)
#' 

#+ construct_classes_aut_index, results="asis"
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

ggplot(d, aes(AUT_index)) + geom_density()

tbl <- as.data.frame(prop.table(table(d$AUT_index))*100)
tbl$Freq <- round(tbl$Freq,1)

print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## OCCR
#' 

#+ construct_classes_occr, results="asis"
d[["occr_labeled"]] <- NA
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Specialist medical practitioners")]                                             = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Nursing professionals")]                                                        = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Midwifery professionals")]                                                      = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2223)]                                                                           = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2224)]                                                                           = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2220)]                                                                           = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2229)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional and complementary medicine professionals")]                         = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3220)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3223)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3224)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3226)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3229)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3231)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3232)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2229)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional and complementary medicine professionals")]                         = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3220)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3223)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3224)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3226)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3229)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3231)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3232)]                                                                           = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accountants")]                                                                  = 'Accountants, auditors, actuaries'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Primary school teachers")]                                                = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Child care workers")]                                                = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Teachers aides")]                                                = 'Teachers: elementary and secondary'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Early childhood educators")]                                                = 'Teachers: elementary and secondary'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Vocational education teachers")]                                                = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2331)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2332)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2340)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Education methods specialists")]                                                = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Teaching professionals not elsewhere classified")]                              = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3310)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3320)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3330)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3340)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2300)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Secondary education teachers")]                                                 = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2350)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3300)]                                                                           = 'Teachers: elementary and secondary'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("University and higher education teachers")]                                     = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2400)]                                                                           = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2410)]                                                                           = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Advertising and marketing professionals")]                                      = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Public relations professionals")]                                               = 'Teachers: university, social sc, librarians'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Library clerks")]                                                                           = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2430)]                                                                           = 'Teachers: university, social sc, librarians'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Generalist medical practitioners")]                                             = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Specialist medical practitioners")]                                             = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2213)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Financial and investment advisers")]                                            = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2419)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2441)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2442)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2443)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2444)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2445)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2000)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2210)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2440)]                                                                           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physicists and astronomers")]                                                   = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Meteorologists")]                                                               = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemists")]                                                                     = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2121)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2122)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Biologists, botanists, zoologists and related professionals")]                  = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2139)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Industrial and production engineers")]                                          = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineers")]                                                              = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental engineers")]                                                      = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical engineers")]                                                         = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical engineers")]                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining engineers, metallurgists and related professionals")]                    = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2147)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2148)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Engineering professionals not elsewhere classified")]                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineering technicians")]                                                = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Life science technicians (excluding medical)")]                                 = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chefs")]                                                                        = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2100)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2110)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Geologists and geophysicists")]                                                 = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Statistical, mathematical and related associate professionals")]                                                 = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2130)]                                                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2140)]                                                                           = 'Mathematicians, engineers etc'



d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics engineering technicians")]                                          = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Product graders and testers (excluding foods and beverages)")]                                          = 'Technicians etc'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining and metallurgical technicians")]                                         = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Draughtspersons")]                                                              = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining supervisors")]                                                           = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Manufacturing supervisors")]                                                    = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Construction supervisors")]                                                     = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Power production plant operators")]                                             = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Incinerator and water treatment plant operators")]                              = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical processing plant controllers")]                                        = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Process control technicians not elsewhere classified")]                         = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural technicians")]                                                     = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Forestry technicians")]                                                         = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3144)]                                                                           = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3145)]                                                                           = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical imaging and therapeutic equipment technicians")]                        = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical and pathology laboratory technicians")]                                 = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pharmaceutical technicians and assistants")]                                    = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical and physical science technicians")]                                    = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical engineering technicians")]                                           = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical engineering technicians")]                                           = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical engineering technicians")]                                             = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physical and engineering science technicians not elsewhere classified")]        = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ships engineers")]                                                              = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2446)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2460)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2470)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fitness and recreation instructors and program leaders")]                       = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3429)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Interior designers and decorators")]                                            = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3460)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3480)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5150)]                                                                           = 'Public advisors'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Management and organization analysts")]                                         = 'Lawyers and judges'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Policy administration professionals")]                                          = 'Lawyers and judges'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2429)]                                                                           = 'Lawyers and judges'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2451)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2452)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2453)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2454)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2455)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3471)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3472)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3473)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3474)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3475)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5210)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(2450)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3470)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3000)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3100)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3110)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3120)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3130)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3140)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3150)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3200)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3210)]                                                                           = 'Arts and entertainment'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1110)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1141)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1142)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1143)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Special needs teachers")]                                                       = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ships deck officers and pilots")]                                               = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1140)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1210)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1231)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1232)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1234)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1235)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1236)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1237)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1238)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1239)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1316)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1317)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1318)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1319)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3416)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1225)]                                                                           = 'Managers: other'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1233)]                                                                           = 'Managers: other'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1314)]                                                                           = 'Managers: other'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1315)]                                                                           = 'Managers: other'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Photographers")]                                                                = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3233)]                                                                           = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4115)]                                                                           = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4111)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4112)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4113)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4114)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4121)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4122)]                                                                           = 'Other clerical'



d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mail carriers and sorting clerks")]                                             = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Typists and word processing operators")]                                        = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Data entry clerks")]                                                            = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4133)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4141)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4142)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4143)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4144)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4190)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bank tellers and related clerks")]                                              = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bookmakers, croupiers and related gaming workers")]                             = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pawnbrokers and money-lenders")]                                                = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Debt-collectors and related workers")]                                          = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4215)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel consultants and clerks")]                                                = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Contact centre information clerks")]                                            = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Telephone switchboard operators")]                                              = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3430)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4000)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4100)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("General office clerks")]                                                        = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Secretaries (general)")]                                                        = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accounting and bookkeeping clerks")]                                            = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4130)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4140)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4200)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4210)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(4220)]                                                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Payroll clerks")]                                                               = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stock clerks")]                                                                 = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Regulatory government associate professionals not elsewhere classified")]                                                                 = 'Other clerical'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Police inspectors and detectives")]                                             = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Social work associate professionals")]                                          = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Religious associate professionals")]                                            = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3414)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3415)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3417)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3419)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Athletes and sports players")]                                                  = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sports coaches, instructors and officials")]                                    = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5220)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop keepers")]                                                                 = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop supervisors")]                                                             = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shop sales assistants")]                                                        = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cashiers and ticket clerks")]                                                   = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Domestic cleaners and helpers")]                                                = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Commercial sales representatives")]                                                = 'Sales'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9113)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3400)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3410)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3420)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5200)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9100)]                                                                           = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9110)]                                                                           = 'Sales'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sales and marketing managers")]                                                 = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Advertising and public relations managers")]                                    = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1226)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1227)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1228)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1229)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Research and development managers")]                                            = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1224)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural and forestry production managers")]                                = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aquaculture and fisheries production managers")]                                = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1313)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1200)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1220)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1300)]                                                                           = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1310)]                                                                           = 'Foremen'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sewing, embroidery and related workers")]                                                               = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bakers, pastry-cooks and confectionery makers")]                                                               = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Carpenters and joiners")]                                                               = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("House builders")]                                                               = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bricklayers and related workers")]                                              = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stonemasons, stone cutters, splitters and carvers")]                            = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Roofers")]                                                                      = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Floor layers and tile setters")]                                                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plasterers")]                                                                   = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Insulation workers")]                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glaziers")]                                                                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plumbers and pipe fitters")]                                                    = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Air conditioning and refrigeration mechanics")]                                 = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7128)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7129)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7130)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Painters and related workers")]                                                 = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Spray painters and varnishers")]                                                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building structure cleaners")]                                                  = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7134)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7135)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7136)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7137)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7139)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7140)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7141)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7142)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7143)]                                                                           = 'Crafts'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Crane, hoist and related plant operators")]                                                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal moulders and coremakers")]                                                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Welders and flamecutters")]                                                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sheet-metal workers")]                                                          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Structural-metal preparers and erectors")]                                      = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Riggers and cable splicers")]                                                   = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7216)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Blacksmiths, hammersmiths and forging press workers")]                          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Toolmakers and related workers")]                                               = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal working machine tool setters and operators")]                             = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal polishers, wheel grinders and tool sharpeners")]                          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Motor vehicle mechanics and repairers")]                                        = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aircraft engine mechanics and repairers")]                                      = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Agricultural and industrial machinery mechanics and repairers")]                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bicycle and related repairers")]                                                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7235)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7236)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7237)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7238)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7239)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7240)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7241)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7242)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7243)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7244)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7245)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Precision-instrument makers and repairers")]                                    = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Musical instrument makers and tuners")]                                         = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Jewellery and precious-metal workers")]                                         = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pre-press technicians")]                                                        = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Printers")]                                                                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Print finishing and binding workers")]                                          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7324)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7331)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7332)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7341)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7342)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7343)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7345)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7346)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building and related electricians")]                                            = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical mechanics and fitters")]                                             = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical line installers and repairers")]                                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7415)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics mechanics and servicers")]                                          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information and communications technology installers and servicers")]           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7433)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7434)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7435)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7436)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7437)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7441)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7442)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8124)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7000)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7100)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7110)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7120)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7200)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7210)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7220)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7230)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7300)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7310)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7320)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7330)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7340)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7400)]                                                                           = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3441)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3442)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3443)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3444)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3449)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3450)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Astrologers, fortune-tellers and related workers")]                             = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Companions and valets")]                                                        = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Undertakers and embalmers")]                                                    = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Personal services workers not elsewhere classified")]                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(0100)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5160)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Freight handlers")]                                                         = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Transport conductors")]                                                         = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Locomotive engine drivers")]                                                    = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Railway brake, signal and switch operators")]                                   = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Motorcycle drivers")]                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Car, taxi and van drivers")]                                                    = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8323)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8324)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bus and tram drivers")]                                                         = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Heavy truck and lorry drivers")]                                                = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8333)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8334)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8340)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8300)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8310)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8320)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8330)]                                                                           = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7344)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7414)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7416)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7423)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7424)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7431)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7432)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Miners and quarriers")]                                                         = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mineral and stone processing plant operators")]                                 = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Well drillers and borers and related workers")]                                 = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal processing plant operators")]                                             = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Metal finishing, plating and coating machine operators")]                       = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8123)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Chemical products plant and machine operators")]                                = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8139)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8140)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Rubber products machine operators")]                                            = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Plastic products machine operators")]                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Paper products machine operators")]                                             = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fibre preparing, spinning and winding machine operators")]                      = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Weaving and knitting machine operators")]                                       = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sewing machine operators")]                                                     = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bleaching, dyeing and fabric cleaning machine operators")]                      = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fur and leather preparing machine operators")]                                  = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Textile, fur and leather products machine operators not elsewhere classified")] = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Food and related products machine operators")]                                  = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8161)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8162)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8163)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8170)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mechanical machinery assemblers")]                                              = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical and electronic equipment assemblers")]                               = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8221)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8222)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8223)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8224)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8229)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8230)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8231)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8232)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8240)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8251)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8252)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8253)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8261)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8262)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8263)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8264)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8265)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8266)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8269)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8270)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8271)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8272)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8273)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8274)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8275)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8276)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8277)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8278)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8279)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8280)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8281)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8282)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8283)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8284)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8285)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8286)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8287)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8290)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9320)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7410)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7420)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(7430)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8000)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8100)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8110)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8120)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8130)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8150)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8200)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8210)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8220)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8250)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(8260)]                                                                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hand packers")]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Kitchen helpers")]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Livestock and dairy producers")]                                                = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Poultry producers")]                                                            = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Apiarists and sericulturists")]                                                 = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6124)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6125)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6126)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6127)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6128)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Animal producers not elsewhere classified")]                                    = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6142)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9142)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9161)]                                                                           = 'Laborers, except farm laborers'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Odd job persons")]                                               = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mining and quarrying labourers")]                                               = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Civil engineering labourers")]                                                  = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building construction labourers")]                                              = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9330)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9300)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9310)]                                                                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6141)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6151)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6152)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6153)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6154)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mobile farm and forestry plant operators")]                                    = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Crop farm labourers")]                                                          = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Livestock farm labourers")]                                                     = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mixed crop and livestock farm labourers")]                                      = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6140)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6150)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9200)]                                                                           = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9210)]                                                                           = 'Farm Workers'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Veterinary technicians and assistants")]                                                                 = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Nursing associate professionals")]                                              = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Midwifery associate professionals")]                                            = 'White collar services'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other artistic and cultural associate professionals")]                                                                           = 'White collar services'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3225)]                                                                           = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3227)]                                                                           = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3228)]                                                                           = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel guides")]                                                                = 'Skilled manual services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5122)]                                                                           = 'Skilled manual services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hairdressers")]                                                                 = 'Skilled manual services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5140)]                                                                           = 'Skilled manual services'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cleaners and helpers in offices, hotels and other establishments")]                                                                      = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Security guards")]                                                              = 'Lowskilled services'

# d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Security guards")]                                                              = 'Lowskilled services'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Travel attendants and travel stewards")]                                        = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sweepers and related labourers")]                                        = 'Lowskilled services'




d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5121)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Waiters")]                                                                      = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Bartenders")]                                                                   = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5133)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5134)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5135)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5136)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5137)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5138)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5139)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Beauticians and related workers")]                                              = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Messengers, package deliverers and luggage porters")]                                              = 'Lowskilled services'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building caretakers")]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5143)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5149)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9120)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9131)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9132)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9133)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9141)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9151)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9152)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9153)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9162)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5000)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cooks")]                                                                        = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5123)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5130)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9000)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9130)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9140)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9150)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(9160)]                                                                           = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Livestock and dairy producers")]                                                = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Poultry producers")]                                                            = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Apiarists and sericulturists")]                                                 = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6124)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6125)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6126)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6127)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6128)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Animal producers not elsewhere classified")]                                    = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mixed crop and animal producers")]                                              = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Field crop and vegetable growers")]                                             = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Tree and shrub crop growers")]                                                  = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6000)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6100)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6110)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(6120)]                                                                           = 'Farmers and related profession'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1100)]                                                                           = 'Managers: public and quasi-public'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(1230)]                                                                           = 'Managers: corporate'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Lawyers")]                                                                      = 'Lawyers and judges'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional and complementary medicine associate professionals")]               = 'Other medical and paramedical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Gallery, museum and library technicians")]                                      = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Administrative and executive secretaries")]                                      = 'Secretaries'


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(3440)]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Non-commissioned armed forces officers")]                                                                           = 'Government protective workers'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Police officers")]                                                                           = 'Government protective workers'

d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Commissioned armed forces officers")]                                                                           = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c(5100)]                                                                           = NA


d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Transport clerks")]                                                  = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Clerical support workers not elsewhere classified")]                 = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stall and market salespersons")]                                     = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Health care assistants")]                                            = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Stationary plant and machine operators not elsewhere classified")]   = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Assemblers not elsewhere classified")]                               = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Concrete placers, concrete finishers and related workers")]          = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Buyers")]                                                            = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information and communications technology operations technicians")]  = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hotel receptionists")]                                               = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electronics engineers")]                                             = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Psychologists")]                                                     = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical records and health information technicians")]                = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Employment agents and contractors")]                                 = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical assistants")]                                                = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Credit and loans officers")]                                         = 'Accountants, auditors, actuaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Service station attendants")]                                        = 'Lowskilled services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Food and beverage tasters and graders")]                             = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Garment and related pattern-makers and cutters")]                    = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Vehicle cleaners")]                                                  = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Farming, forestry and fisheries advisers")]                          = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Broadcasting and audio-visual technicians")]                         = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Forestry and related workers")]                                      = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Tailors, dressmakers, furriers and hatters")]                        = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shelf fillers")]                                                     = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Electrical engineers")]                                              = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dentists")]                                                          = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Software developers")]                                               = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Production clerks")]                                                 = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fire-fighters")]                                                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Steam engine and boiler operators")]                                 = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Elementary workers not elsewhere classified")]                       = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Managing directors and chief executives")]                           = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Journalists")]                                                       = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Legal secretaries")]                                                 = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Receptionists (general)")]                                           = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Personnel clerks")]                                                  = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cement, stone and other mineral products machine operators")]        = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glass and ceramics plant operators")]                                = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Retail and wholesale trade managers")]                               = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sports, recreation and cultural centre managers")]                   = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Real estate agents and property managers")]                          = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Statistical, finance and insurance clerks")]                         = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Craft and related workers not elsewhere classified")]                = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Wood processing plant operators")]                                   = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Telecommunications engineers")]                                      = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Archivists and curators")]                                           = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Translators, interpreters and other linguists")]                     = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Securities and finance dealers and brokers")]                        = 'Accountants, auditors, actuaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Filing and copying clerks")]                                         = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Deep-sea fishery workers")]                                          = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Butchers, fishmongers and related food preparers")]                  = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Earthmoving and related plant operators")]                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Construction managers")]                                             = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Cartographers and surveyors")]                                       = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Visual artists")]                                                    = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Musicians, singers and composers")]                                  = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Petroleum and natural gas refining plant operators")]                = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental and occupational health inspectors and associates")]   = 'Physicians and dentists'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Accounting associate professionals")]                                = 'Accountants, auditors, actuaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Insurance representatives")]                                         = 'Accountants, auditors, actuaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Office supervisors")]                                                = 'Foremen'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government tax and excise officials")]                               = 'Accountants, auditors, actuaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Coding, proof-reading and related clerks")]                          = 'Other clerical'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sign writers, decorative painters, engravers and etchers")]          = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Shoemakers and related workers")]                                    = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Senior officials of special-interest organizations")]                = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Manufacturing managers")]                                            = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Supply, distribution and related managers")]                         = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Mathematicians, actuaries and statisticians")]                       = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Environmental protection professionals")]                            = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Building architects")]                                               = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Pharmacists")]                                                       = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Audiologists and speech therapists")]                                = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Systems administrators")]                                            = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Sociologists, anthropologists and related professionals")]           = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Authors and related writers")]                                       = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Dancers and choreographers")]                                        = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aircraft pilots and related associate professionals")]               = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Air traffic controllers")]                                           = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Ambulance workers")]                                                 = 'Transportation workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Computer network and systems technicians")]                          = 'Mathematicians, engineers etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Street food salespersons")]                                          = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fashion and other models")]                                          = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Contact centre salespersons")]                                       = 'Sales'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Glass makers, cutters, grinders and finishers")]                     = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Upholsterers and related workers")]                                  = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Packing, bottling and labelling machine operators")]                 = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Hand launderers and pressers")]                                      = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Other cleaning workers")]                                            = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Water and firewood collectors")]                                     = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Traditional chiefs and heads of village")]                           = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Health services managers")]                                          = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Education managers")]                                                = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Financial and insurance services branch managers")]                  = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Professional services managers not elsewhere classified")]           = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Restaurant managers")]                                               = 'Managers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Philosophers, historians and political scientists")]                 = 'Other univ professionals'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical and dental prosthetic technicians")]                         = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Physiotherapy technicians and assistants")]                          = 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Medical secretaries")]                                               = 'Secretaries'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Customs and border inspectors")]                                     = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government social benefits officials")]                              = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Government licensing officials")]                                    = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Information and communications technology user support technicians")]= 'Technicians etc'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Survey and market research interviewers")]                           = 'White collar services'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Prison guards")]                                                     = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Protective services workers not elsewhere classified")]              = 'Government protective workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Aquaculture workers")]                                               = 'Farm Workers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Handicraft workers not elsewhere classified")]                       = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Fruit, vegetable and related preservers")]                           = 'Laborers, except farm laborers'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Underwater divers")]                                                 = 'Crafts'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Photographic products machine operators")]                           = 'Operatives, except transportation'
d[["occr_labeled"]][d[["V14_CODE_labeled"]] %in% c("Laundry machine operators")]                                         = 'Operatives, except transportation'

d[["occr_labeled"]] <- factor(d[["occr_labeled"]], levels=c("Managers",
                                                            "Lawyers and judges",
                                                            "Physicians and dentists",
                                                            "Mathematicians, engineers etc",
                                                            "Teachers: university, social sc, librarians",
                                                            "Other univ professionals",
                                                            "Teachers: elementary and secondary",
                                                            "Public advisors",
                                                            "Foremen",
                                                            "Accountants, auditors, actuaries",
                                                            "Technicians etc",
                                                            "Government protective workers",
                                                            "White collar services",
                                                            "Crafts",
                                                            "Skilled manual services",
                                                            "Secretaries",
                                                            "Other clerical",
                                                            "Sales",
                                                            "Operatives, except transportation",
                                                            "Laborers, except farm laborers",
                                                            "Transportation workers",
                                                            "Farm Workers",
                                                            "Lowskilled services"))

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "occr_labeled"
unlabeled_varname <- "occr"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d$occup_groups[i] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}
#% ------------------------------------

tbl <- as.data.frame(table(d$occr_labeled, useNA="ifany"))
print(knitr::kable(arrange(arrange(tbl, -Freq), -Freq), "html", table.attr='class="table table-striped table-hover"'))

cat("\n\n")
cat("### Occupational codes of those missing")
cat("\n\n")
df_na <- d[is.na(d$occr_labeled),c("V14_CODE_labeled")]
tbl <- as.data.frame(table(df_na))
tbl <- tbl[tbl$Freq > 0,]
print(knitr::kable(arrange(arrange(tbl, -Freq), -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## Skill2

#+ construct_classes_skills2, results="asis"
d$skill2_labeled <- NA
d$skill2_labeled[d$occr_labeled %in% c('Physicians and dentists')]                     <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Accountants, auditors, actuaries')]            <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Teachers: university, social sc, librarians')] <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Other univ professionals')]                    <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Mathematicians, engineers etc')]               <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Lawyers and judges')]                          <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Managers: public and quasi-public')]           <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Managers: corporate')]                         <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('Other medical and paramedical')]               <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Teachers: elementary and secondary')]          <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Technicians etc')]                             <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Public advisors')]                             <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Arts and entertainment')]                      <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Managers: other')]                             <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Foremen')]                                     <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Crafts')]                                      <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Government protective workers')]               <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Skilled manual services')]                     <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Farmers and related profession')]              <- 'Skilled'
d$skill2_labeled[d$occr_labeled %in% c('Lowskilled services')]                         <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Secretaries')]                                 <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Other clerical')]                              <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Sales')]                                       <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Transportation workers')]                      <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Operatives, except transportation')]           <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Laborers, except farm laborers')]              <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('Farm Workers')]                                <- 'Low/semi skilled'
d$skill2_labeled[d$occr_labeled %in% c('White collar services')]                       <- 'Low/semi skilled'


d$skill2_labeled[d$occr_labeled %in% c('13-Production and specialized services managers')]       <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('11-Chief executives, senior officials and legislators')] <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('12-Administrative and commercial managers')]             <- 'Expert'
d$skill2_labeled[d$occr_labeled %in% c('53-Personal care workers')]                              <- 'Skilled'

d$skill2_labeled <- factor(d$skill2_labeled, levels=c("Expert","Skilled","Low/semi skilled"))


#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "skill2_labeled"
unlabeled_varname <- "skill2"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d$occup_groups[i] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}
#% ------------------------------------



tbl <- as.data.frame(table(d$skill2_labeled, useNA="ifany"))
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

cat("\n\n")
cat("### Occupational CLASSES of those missing")
cat("\n\n")
df_na <- d[is.na(d$skill2_labeled),c("occup_groups_labeled")]
tbl <- as.data.frame(table(df_na))
tbl <- tbl[tbl$Freq > 0,]
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

#' ## Kivinen class 08

#+ construct_classes_kivinen, results="asis"
# Tästä alkaa Kivisen autonomiamuuttujan mun hapuilut. Ekaks tein noi tsunkin lopun csv:t ekseliin ja lähetin markulle/joukolle (23.9.2016)

# d$auto1[d$v34c1 %in% "Yes" &
#         d$v35c1 %in% "To a significant degree " &
#         d$v35c1 %in% "To a significant degree "] <- "Core of MC"
#

d$V34c1[d$V34c1 %in% c(9)] <- 2
d$V35c1[d$V35c1 %in% c(9)] <- 1
d$V37c1[d$V37c1 %in% c(9)] <- 1

# d$V34c1[is.na(d$V34c1)] <- 0
# d$V34c1[is.na(d$V35c1)] <- 0
# d$V34c1[is.na(d$V37c1)] <- 0


#
# dd <- d[!is.na(d$V34c1),]
#
#
# r <- expand.grid(unique(dd$V34c1),
#             unique(dd$V35c1),
#             unique(dd$V37c1))
# names(r) <- c("V34c1","V35c1","V37c1")
# write.csv(r, file="./data/autonomia_numeric.csv")
#
# r <- expand.grid(unique(label_sdmr(dd,"V34c1")),
#             unique(label_sdmr(dd,"V35c1")),
#             unique(label_sdmr(dd,"V37c1")))
# names(r) <- c("V34c1","V35c1","V37c1")
# write.csv(r, file="./data/autonomia_labeled.csv")

d2 <- d[!is.na(d$V34c1),]

r <- expand.grid(unique(d2$V35c1),
                 unique(d2$V35c1),
                 unique(d2$V34c1))
names(r) <- c("V34c1","V35c1","V37c1")


table(d$V34c1, useNA="ifany")
table(d$V35c1, useNA="ifany")
table(d$V37c1, useNA="ifany")

# xx <- d %>% filter(is.na(d$V34c1),is.na(d$V35c1),is.na(d$V37c1))

# V34c1 
# Yes 	1
# No 	2 

# V35c1 ja V37c1
# To an insignificant degree 	1
# Somewhat yes, somewhat no 	2
# To a significant degree 	3 

d$kivinen_class_08_labeled <- NA
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 3 & d$V34c1 %in% 1] <- "core of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 3 & d$V34c1 %in% 1] <- "core of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 2 & d$V34c1 %in% 1] <- "core of the middle class"

d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 3 & d$V34c1 %in% 1] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 2 & d$V34c1 %in% 1] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 2 & d$V34c1 %in% 1] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 1 & d$V34c1 %in% 1] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 1 & d$V34c1 %in% 1] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 3 & d$V34c1 %in% 2] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 3 & d$V34c1 %in% 2] <- "margin of the middle class"
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 2 & d$V34c1 %in% 2] <- "margin of the middle class"

d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 3 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 2 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 2 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 3 & d$V37c1 %in% 1 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 2 & d$V37c1 %in% 1 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 1 & d$V34c1 %in% 2] <- "working class"
d$kivinen_class_08_labeled[d$V35c1 %in% 1 & d$V37c1 %in% 1 & d$V34c1 %in% 1] <- "working class"




# d$kivinen_class_08_labeled <- NA
# d$kivinen_class_08_labeled[d$V34c1 %in%3 &	d$V34c1 %in% 3	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 2	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 1	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 1	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 0	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 0	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 2	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 3 &	d$V37c1 %in% 3	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 3	 & d$V34c1 %in% 1] <- "core of middle class"
# 
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 2	 & d$V34c1 %in% 1] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 1	 & d$V34c1 %in% 1] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 1	 & d$V34c1 %in% 2] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 0	 & d$V34c1 %in% 2] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 0	 & d$V34c1 %in% 1] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 2	 & d$V34c1 %in% 2] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 2 &	d$V37c1 %in% 3	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 3	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 2	 & d$V34c1 %in% 1] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 1	 & d$V34c1 %in% 1] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 1	 & d$V34c1 %in% 2] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 0	 & d$V34c1 %in% 2] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 0	 & d$V34c1 %in% 1] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 2	 & d$V34c1 %in% 2] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 1 &	d$V37c1 %in% 3	 & d$V34c1 %in% 2] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 3	 & d$V34c1 %in% 1] <- "core of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 2	 & d$V34c1 %in% 1] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 1	 & d$V34c1 %in% 1] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 1	 & d$V34c1 %in% 2] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 0	 & d$V34c1 %in% 2] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 0	 & d$V34c1 %in% 1] <- "working class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 2	 & d$V34c1 %in% 2] <- "margin of middle class"
# d$kivinen_class_08_labeled[d$V35c1 %in% 0 &	d$V37c1 %in% 3	 & d$V34c1 %in% 2] <- "core of middle class"
#
# core of the middle class
# margin of middle class
# working class

# Toinen vaihtoehto olisi uudelleenkoodata ammattien tasolla "V14_CODE_labeled" ammattiryhmien sijaan!

d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "7-Craft and related trades workers" & is.na(d$V35c1)]                             = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "8-Plant and machine operators, and assemblers" & is.na(d$V35c1)]                  = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "52-Sales workers" & is.na(d$V35c1)]                                               = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "92-Agricultural, forestry and fishery labourers" & is.na(d$V35c1)]                = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "32-Health associate professionals" & is.na(d$V35c1)]                              = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "53-Personal care workers" & is.na(d$V35c1)]                                       = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "43-Numerical and material recording clerks" & is.na(d$V35c1)]                     = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "23-Teaching professionals" & is.na(d$V35c1)]                                      = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "11-Chief executives, senior officials and legislators" & is.na(d$V35c1)]          = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "31-Science and engineering associate professionals" & is.na(d$V35c1)]             = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "3-Technicians and associate professionals" & is.na(d$V35c1)]                      = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "91-Cleaners and helpers" & is.na(d$V35c1)]                                        = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "33-Business and administration associate professionals" & is.na(d$V35c1)]         = "margin of middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "44-Other clerical support workers" & is.na(d$V35c1)]                              = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "41-General and keyboard clerks" & is.na(d$V35c1)]                                 = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "42-Customer services clerks" & is.na(d$V35c1)]                                    = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "34-Legal, social, cultural and related associate professionals" & is.na(d$V35c1)] = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "26-Legal, social and cultural professionals" & is.na(d$V35c1)]                    = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "21-Science and engineering professionals" & is.na(d$V35c1)]                       = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "4-Clerical support workers" & is.na(d$V35c1)]                                     = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "35-Information and communications technicians" & is.na(d$V35c1)]                  = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "13-Production and specialized services managers" & is.na(d$V35c1)]                = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "12-Administrative and commercial managers" & is.na(d$V35c1)]                      = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "24-Business and administration professionals" & is.na(d$V35c1)]                   = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "6-Skilled agricultural, forestry and fishery workers" & is.na(d$V35c1)]           = "working class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "25-Information and communications technology professionals" & is.na(d$V35c1)]     = "margin of the middle class"
d[["kivinen_class_08_labeled"]][d[["occup_groups_labeled"]] %in% "22-Health professionals" & is.na(d$V35c1)]                                        = "margin of the middle class"

d[["kivinen_class_08_labeled"]] <- factor(d[["kivinen_class_08_labeled"]], levels=c("core of the middle class","margin of the middle class","working class"))

prop.table(table(d$kivinen_class_08_labeled))*100
table(d$kivinen_class_08_labeled)

# d %>%  filter(is.na(kivinen_class_08_labeled)) %>% select(V34c1,V35c1,V37c1,kivinen_class_08_labeled)

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "kivinen_class_08_labeled"
unlabeled_varname <- "kivinen_class_08"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d[[unlabeled_varname]][[i]] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}
#% ------------------------------------

tbl <- as.data.frame(table(d$kivinen_class_08, useNA="ifany"))
tbl2 <- as.data.frame(prop.table(table(d$kivinen_class_08, useNA="ifany"))*100)
tbl <- merge(tbl,tbl2,by="Var1")
names(tbl) <- c("Class","Freq","Shares")
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))

cat("\n\n")
cat("### Occupational CLASSES of those missing")
cat("\n\n")
# df_na <- d[is.na(d$kivinen_class_08),c("occup_groups_labeled")]
df_na <- d[is.na(d$kivinen_class_08),c("V14_CODE_labeled")]

tbl <- as.data.frame(table(df_na))
tbl <- tbl[tbl$Freq > 0,]
print(knitr::kable(arrange(tbl, -Freq), "html", table.attr='class="table table-striped table-hover"'))


############ UUSIA MUUTTUJIA!!! ###################################################
# palkkaindeksi - working class mean == 100
# d %>% filter(kivinen_class_08 == 3) %>% summarise(kpalkka = mean(V74c1, na.rm=TRUE))
d$wage_index_mean_working_class <- d$V74c1 / 18515.76 *100
new_row_for_meta_df <- data_frame(code="wage_index_mean_working_class",
                                     name="wage_index_mean_working_class",
                                     label=NA,
                                     value=1,class="numeric")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)

# palkkaindeksi  - mean == 100
# d %>%  summarise(kpalkka = mean(V74c1, na.rm=TRUE))
d$wage_index_mean <- d$V74c1 / 22249.38 *100
new_row_for_meta_df <- data_frame(code="wage_index_mean",
                                     name="wage_index_mean",
                                     label=NA,
                                     value=1,class="numeric")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)


# palkkaindeksi  - median == 100
# d %>%  summarise(kpalkka = median(V74c1, na.rm=TRUE))
d$wage_index_median <- d$V74c1 / 17000 *100
new_row_for_meta_df <- data_frame(code="wage_index_median",
                                     name="wage_index_median",
                                     label=NA,
                                     value=1,class="numeric")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)


22249.38

17000




# poverty 60%
d %>% summarise(kraja = median(V74c1, na.rm=TRUE)*.6) %>% .$kraja-> raja60
d$poverty60_labeled <- ifelse(d$V74c1 <= raja60, "poor", "non-poor")
d$poverty60_labeled <- factor(d$poverty60_labeled)

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "poverty60_labeled"
unlabeled_varname <- "poverty60"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d[[unlabeled_varname]][[i]] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}

# poverty 50%
d %>% summarise(kraja = median(V74c1, na.rm=TRUE)*.5) %>% .$kraja-> raja50
d$poverty50_labeled <- ifelse(d$V74c1 <= raja50, "poor", "non-poor")
d$poverty50_labeled <- factor(d$poverty50_labeled)

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "poverty50_labeled"
unlabeled_varname <- "poverty50"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d[[unlabeled_varname]][[i]] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}

# poverty 40%
d %>% summarise(kraja = median(V74c1, na.rm=TRUE)*.4) %>% .$kraja-> raja40
d$poverty40_labeled <- ifelse(d$V74c1 <= raja40, "poor", "non-poor")
d$poverty40_labeled <- factor(d$poverty40_labeled)

#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
this_var_name <- "poverty40_labeled"
unlabeled_varname <- "poverty40"
label <- levels(d[[this_var_name]])
value <- 1:length(label)
code <- rep(unlabeled_varname, length(label))
name <- rep(unlabeled_varname, length(label))
new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)
# Recode the new variable into numeric!
for (i in 1:nrow(d)){
  label_value <- as.character(d[[this_var_name]][[i]])
  if (is.na(label_value)){
    d[[unlabeled_varname]][[i]] <- NA
  } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
}

####################################################################################
#% ------------------------------------

# in the end, lets remove all variables with variable name including "labeled"
d <- d[names(d)[!grepl("labeled", names(d))]]

saveRDS(d, file="./data/sdmr15.RDS")
saveRDS(meta_df, file="./data/meta_df.RDS")




#+ convert_to_spss
meta_df$numeric <- ifelse(is.na(meta_df$label), TRUE, FALSE)
library(labelled)
for (n in names(d)){
  var_label(d[[n]]) <- meta_df[meta_df$code %in% n, "name"][1]
}

for (n in names(d)){
  # if numeric, no need for value label
  if (is.na(meta_df[meta_df$code %in% n, "numeric"][1])) next()
  if (meta_df[meta_df$code %in% n, "numeric"][1]) next()
  vec <- as.integer(meta_df[meta_df$code %in% n, "value"])
  names(vec) <- meta_df[meta_df$code %in% n, "label"]
  d[[n]] <- labelled(d[[n]], label=vec)
}

foreign::write.foreign(d,  
              codefile="./data/sdmr2015.sps",
              datafile="./data/sdmr2015.sav", 
              package="SPSS") 

foreign::write.foreign(d,  
                       codefile="./data/sdmr2015.do",
                       datafile="./data/sdmr2015.dta", 
                       package="Stata") 

write.csv(d,  file = "./data/sdmr2015.csv")



#' # Summaries
#' 
#' 
#+ summaries


# Summaries

## V14_CODE, Occup_groups, occr, autonomia, skills2 and autonomia & DMi-index crosstabulated

# library(dplyr)
# tbl <- d %>% group_by(V14_CODE_labeled) %>%
#   summarise(occup_groups_labeled = occup_groups_labeled[1],
#             occr_labelled = occr_labeled[1],
#             skill2_labeled = skill2_labeled[1],
#             autonomia_labeled = autonomia_labeled[1],
#             mean_AUT_index = mean(AUT_index, na.rm=TRUE),
#             mean_DMi_for13 = mean(DMi_for13, na.rm=TRUE))
# 
# # print(knitr::kable(tbl, "html", table.attr='class="table table-striped table-hover"'))
# 
# library(DT)
# datatable(tbl, filter = 'top', options = list(
#   pageLength = 25, autoWidth = TRUE
# ))



## testitesti

# tbl <- d %>% select(V14_CODE_labeled,occup_groups_labeled,skill2_labeled,autonomia_labeled,V19c1,AUT_index,DMi_for13)
# 
# library(DT)
# datatable(tbl, filter = 'top', options = list(
#   pageLength = 15, autoWidth = TRUE
# ))


