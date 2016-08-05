# d <- haven::read_sav("http://muuankarski.kapsi.fi/aisurvey/data/sdmr_4_english_language_version.sav")
# 
# label_data <- data.frame()
# for (i in 1:ncol(d)){
#   df <- data.frame()
#   code  <- names(d[i])
#   name <- attributes(d[[i]])$label
#   labels <- names(attributes(d[[i]])$labels)
#   if (is.null(labels)){
#     values = unique(d[[i]])
#     labels=NA
#   } else {
#     values = as.integer(attributes(d[[i]])$labels)
#   }
#     df <- data.frame(code=code,
#                      name=name,
#                      labels=labels,
#                      values=values, stringsAsFactors=FALSE)
#   label_data <- rbind(label_data,df)
# }
# label_data_orig <- label_data
# save(label_data_orig, file="./data/label_data_orig.RData")

# Add vars that do not exist yet
# load("./data/label_data_orig.RData")
# load("./data/d15.RData")
# newvars <- names(d15)[!names(d15) %in% unique(label_data_orig$code)]
# 
# dat <- data.frame()
# for (i in 1:length(newvars)){
#   df <- data.frame()
#   code  <- newvars[i]
#   name <- newvars[i]
#   labels <- unique(as.character(d15[[newvars[i]]]))
#   values <- unique(as.character(d15[[newvars[i]]]))
#   df <- data.frame(code=code,
#                      name=name,
#                      labels=labels,
#                      values=values, stringsAsFactors=FALSE)
#   dat <- rbind(dat,df)
# }
# # lets recode labels in NA for numeric indicators
# dat$labels <- ifelse(grepl("[0-9]$", dat$labels), NA, dat$labels)
# # lets remove rows with NA both in values & labels
# dat <- dat[!(is.na(dat$labels) & is.na(dat$values)),]
# label_data <- rbind(label_data_orig,dat)
# save(label_data, file="./data/label_data.RData")
# # data=d
# # var="V95_1c1"
# # var="V54_6c1"
 
label_sdmr <- function(data,var,into.factor=TRUE){
  load("./data/label_data.RData")
  labdat <- label_data[label_data$code %in% var,]
  if (NA %in% labdat$labels){
    data[["newvar"]] <- data[[var]]
  } else {
    for (vals in unique(labdat$value)){
      data[["newvar"]][data[[var]] %in% vals] <- labdat[labdat$value %in% vals,"labels"]
    }
  }
  if (!NA %in% labdat$labels & into.factor){
    # remove possible duplicated levels
    levs <- labdat[order(labdat$value),]$labels
    levs <- levs[!duplicated(levs)]
    data[["newvar"]] <- factor(data[["newvar"]], levels=levs)
  }
  return(data[["newvar"]])
}


## MERGED file

# d <- haven::read_sav("http://muuankarski.kapsi.fi/aisurvey/data/merged_4wave20151110.sav")
# 
# label_data <- data.frame()
# for (i in 1:ncol(d)){
#   df <- data.frame()
#   code  <- names(d[i])
#   name <- attributes(d[[i]])$label
#   labels <- names(attributes(d[[i]])$labels)
#   if (is.null(labels)){
#     values = unique(d[[i]])
#     labels=NA
#   } else {
#     values = as.integer(attributes(d[[i]])$labels)
#   }
#     df <- data.frame(code=code,
#                      name=name,
#                      labels=labels,
#                      values=values, stringsAsFactors=FALSE)
#   label_data <- rbind(label_data,df)
# }
# label_data_orig_merge <- label_data
# save(label_data_orig_merge, file="./data/llabel_data_orig_merge.RData")

# Add vars that do not exist yet
# load("./data/label_data_orig.RData")
# load("./data/d15.RData")
# newvars <- names(d15)[!names(d15) %in% unique(label_data_orig$code)]
# 
# dat <- data.frame()
# for (i in 1:length(newvars)){
#   df <- data.frame()
#   code  <- newvars[i]
#   name <- newvars[i]
#   labels <- unique(as.character(d15[[newvars[i]]]))
#   values <- unique(as.character(d15[[newvars[i]]]))
#   df <- data.frame(code=code,
#                      name=name,
#                      labels=labels,
#                      values=values, stringsAsFactors=FALSE)
#   dat <- rbind(dat,df)
# }
# # lets recode labels in NA for numeric indicators
# dat$labels <- ifelse(grepl("[0-9]$", dat$labels), NA, dat$labels)
# # lets remove rows with NA both in values & labels
# dat <- dat[!(is.na(dat$labels) & is.na(dat$values)),]
# label_data <- rbind(label_data_orig,dat)
# label_data_merge <- label_data_orig_merge
# save(label_data_merge, file="./data/label_data_merge.RData")

label_sdmr_merge <- function(data,var,into.factor=TRUE){
  load("./data/label_data_merge.RData")
  labdat <- label_data[label_data$code %in% var,]
  if (NA %in% labdat$labels){
    data[["newvar"]] <- data[[var]]
  } else {
    for (vals in unique(labdat$value)){
      data[["newvar"]][data[[var]] %in% vals] <- labdat[labdat$value %in% vals,"labels"]
    }
  }
  if (!NA %in% labdat$labels & into.factor){
    # remove possible duplicated levels
    levs <- labdat[order(labdat$value),]$labels
    levs <- levs[!duplicated(levs)]
    data[["newvar"]] <- factor(data[["newvar"]], levels=levs)
  }
  return(data[["newvar"]])
}

# load("./data/d_merge_N_class.RData")
# head(d_merge_N_class)
# rr <- d_merge_N_class
# rr$newvar <- label_sdmr_merge(rr, "v113_1998_2007_2015")
