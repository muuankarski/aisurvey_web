# # Manipulate the excel from Misha
# library(readxl)
# d <- read_excel("./data/sdmr_4_linear_distributions.xls", skip=1)
# names(d) <- c("name","label","share","n")
# # if % in share col, remove
# d <- d[!d$share %in% "%",]
# d <- d[!d$name %in% "Total",]
# d <- d[c("name","label")]
# d <- d[complete.cases(d[,2]),]
# library(zoo)
# d <- na.locf(d)
# d <- d[complete.cases(d),]
# library(dplyr)
# dd <- d %>% group_by(name) %>% mutate(value = 1:n())
# 
# # VAriable names from labelled SPSS data
# library(haven)
# d <- haven::read_sav("./data/sdmr_4_english_language_version.sav")
# 
# name_data <- data.frame()
# for (i in 1:ncol(d)){
#   code  <- names(d[i])
#   name <- attributes(d[[i]])$label
#   name_data <- rbind(name_data,data.frame(code,name, stringsAsFactors = FALSE))
# }
# 
# label_data <- merge(dd,name_data,by="name", all.x=TRUE)
# save(label_data, file="./data/label_data.RData")

# data=d
# var="V95_1c1"

label_sdmr <- function(data,var,factor=TRUE){
  
  load("./data/label_data.RData")
  labdat <- label_data[label_data$code %in% var,]
  
  for (vals in unique(labdat$value)){
    data[["newvar"]][data[[var]] %in% vals] <- labdat[labdat$value %in% vals,"label"]
  }
  if (factor) data[["newvar"]] <- factor(data[["newvar"]], levels=labdat[order(labdat$value),]$label)
  return(data[["newvar"]])
}


# var="V54_6c1"
