#' ---
#' title: Construct merged data 
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

#' This is a script to construct up-to-date version of SDMR of all waves data from 
#' `merged_database_new_class_groups_and_autonomy_09032016.sav` -data
#' 
#' # Details of the raw data

#+ rawdetail
d <- haven::read_sav("~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/archive/merged_database_new_class_groups_and_autonomy_09032016.sav")


dim(d)
writeLines(capture.output(str(d)), con = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_merge_with_class_str.txt")

saveRDS(d, file = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_merge_with_class.RDS")

#' 
#' [Check the output of ´str(d)`](./data/structure_merge_with_class_str.txt)
#' 

#' # Creating a metadata based on variable attributes

#+ meta_df, results="asis"

# 

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

dim(d)
writeLines(capture.output(str(d)), con = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_merge_with_class_stripped_str.txt")

saveRDS(d, file = "~/btsync/mk/workspace/russia/huippari2016/aisurvey_web/data/structure_merge_with_class_stripped.RDS")

####################################################################################
#% ------------------------------------
# New variables

table(d$wave)

meta_df <- meta_df[!meta_df$code %in% "wave",]

code <- "wave"
name <- "wave"
value = c(1:4)
label = c(1991,1998,2007,2015)
class="factor"
new_row <- data_frame(code,name,label,value,class)
meta_df <- rbind(meta_df,new_row)


# palkkaindeksi - working class mean == 100
# d %>% group_by(wave) %>%  filter(kivinen_class_08 == 3) %>% summarise(kpalkka = mean(V74c1, na.rm=TRUE))
# d$wage_index_mean_working_class <- d$V74c1 / 18515.76 *100
# new_row_for_meta_df <- data_frame(code="wage_index_mean_working_class",
#                                   name="wage_index_mean_working_class",
#                                   label=NA,
#                                   value=1,class="numeric")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)

# palkkaindeksi  - mean == 100

# d$v74_1998_2007_2015[d$v74_1998_2007_2015 == 0] <- NA # lets get rid of the zeros here!
d %>%  group_by(wave) %>% 
  mutate(kpalkka = mean(v74_1998_2007_2015, na.rm=TRUE),
         wage_index_mean = v74_1998_2007_2015/kpalkka *100) %>% 
  select(-kpalkka)-> d
# d %>%  group_by(wave) %>% summarise(kpalkka = mean(wage_index_median, na.rm=TRUE))

new_row_for_meta_df <- data_frame(code="wage_index_mean",
                                  name="wage_index_mean",
                                  label=NA,
                                  value=NA,class="numeric")
meta_df <- rbind(meta_df,new_row_for_meta_df)


# palkkaindeksi  - median == 100
# d %>%  group_by(wave) %>% summarise(kpalkka = median(v74_1998_2007_2015, na.rm=TRUE))
d %>%  group_by(wave) %>% 
  mutate(kpalkka = median(v74_1998_2007_2015, na.rm=TRUE),
         wage_index_median = v74_1998_2007_2015/kpalkka *100) %>% 
  select(-kpalkka)-> d

new_row_for_meta_df <- data_frame(code="wage_index_median",
                                  name="wage_index_median",
                                  label=NA,
                                  value=NA,class="numeric")
# Write lines for meta_df
meta_df <- rbind(meta_df,new_row_for_meta_df)

# relative poverty rate .60
d %>% group_by(wave) %>% 
  mutate(kraja = median(v74_1998_2007_2015, na.rm=TRUE)*.6,
         poverty60 = ifelse(v74_1998_2007_2015 >= kraja, 0, 1)) %>% 
  select(-kraja)-> d

new_row_for_meta_df <- data_frame(code="poverty60",
                                  name="relative poverty rate at 60 % of median income threshold",
                                  label=c("non-poor","poor"),
                                  value=c(0,1),
                                  class="factor")  
meta_df <- rbind(meta_df,new_row_for_meta_df)

# relative poverty rate .70
d %>% group_by(wave) %>% 
  mutate(kraja = median(v74_1998_2007_2015, na.rm=TRUE)*.7,
         poverty70 = ifelse(v74_1998_2007_2015 >= kraja, 0, 1)) %>% 
  select(-kraja)-> d

new_row_for_meta_df <- data_frame(code="poverty70",
                                  name="relative poverty rate at 70 % of median income threshold",
                                  label=c("non-poor","poor"),
                                  value=c(0,1),
                                  class="factor")  
meta_df <- rbind(meta_df,new_row_for_meta_df)

# relative poverty rate .50
d %>% group_by(wave) %>% 
  mutate(kraja = median(v74_1998_2007_2015, na.rm=TRUE)*.5,
         poverty50 = ifelse(v74_1998_2007_2015 >= kraja, 0, 1)) %>% 
  select(-kraja)-> d

new_row_for_meta_df <- data_frame(code="poverty50",
                                  name="relative poverty rate at 50 % of median income threshold",
                                  label=c("non-poor","poor"),
                                  value=c(0,1),
                                  class="factor")  
meta_df <- rbind(meta_df,new_row_for_meta_df)



#% ------------------------------------
# This bloc of code is to recode the string variable into numeric and add equivalent rows into label data for analysis later on
# this_var_name <- "poverty40_labeled"
# unlabeled_varname <- "poverty40"
# label <- levels(d[[this_var_name]])
# value <- 1:length(label)
# code <- rep(unlabeled_varname, length(label))
# name <- rep(unlabeled_varname, length(label))
# new_row_for_meta_df <- data_frame(code,name,label,value,class="factor")
# # Write lines for meta_df
# meta_df <- rbind(meta_df,new_row_for_meta_df)
# # Recode the new variable into numeric!
# for (i in 1:nrow(d)){
#   label_value <- as.character(d[[this_var_name]][[i]])
#   if (is.na(label_value)){
#     d[[unlabeled_varname]][[i]] <- NA
#   } else  d[[unlabeled_varname]][[i]] <- new_row_for_meta_df[new_row_for_meta_df$label %in% label_value,]$value
# }

####################################################################################
#% ------------------------------------

saveRDS(d, file="./data/sdmr_merge.RDS")
saveRDS(meta_df, file="./data/meta_df_merge.RDS")


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
                       codefile="./data/sdmr_merge.sps",
                       datafile="./data/sdmr_merge.sav", 
                       package="SPSS") 

foreign::write.foreign(d,  
                       codefile="./data/sdmr_merge.do",
                       datafile="./data/sdmr_merge.dta", 
                       package="Stata") 

write.csv(d,  file = "./data/sdmr_merge.csv")



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


