#' ---
#' title: Analyysidemo
#' author: kuka?
#' date: "Last updated: **`r Sys.time()`**"
#' output: 
#'   html_document: 
#'     theme: yeti
#'     toc: true
#'     toc_float: true
#'     number_sections: yes
#'     code_folding: hide
#' ---
#' 
#' # Johdanto
#' 
#' Tähän tulee tekstiä

#+ setup
knitr::opts_chunk$set(list(echo=TRUE,eval=TRUE,cache=FALSE,warning=FALSE,message=FALSE,fig.width = 10, fig.height = 10))
if (!file.exists("./data/label_data.RData")){
  download.file(url = "http://muuankarski.kapsi.fi/aisurvey/demo/example.R",destfile = "./example.R")
  dir.create("./data/")
  download.file(url = "http://muuankarski.kapsi.fi/aisurvey/data/sdmr_4_english_language_versionN.RData",destfile = "./data/sdmr_4_english_language_versionN.RData")
  download.file(url = "http://muuankarski.kapsi.fi/aisurvey/data/merged_4wave20151110N.RData",destfile = "./data/merged_4wave20151110N.RData")
  download.file(url = "http://muuankarski.kapsi.fi/aisurvey/data/d_merge_N_class.RData",destfile = "./data/d_merge_N_class.RData")
  download.file(url = "http://muuankarski.kapsi.fi/aisurvey/data/label_data.RData",destfile = "./data/label_data.RData")
}
load("./data/sdmr_4_english_language_versionN.RData")
load("./data/label_data.RData")
source(url("http://muuankarski.kapsi.fi/aisurvey/code/label_data.R"))

#' 
#' # Tämä on otsikko
#' 
#' Tähän tule tekstiä

#+ kuvio
dd <- d15n
trust_vars <- label_data[grepl("Trust:", label_data$name),]
trust_vars <- trust_vars[!duplicated(trust_vars$code),]

aggdf <- data.frame()
for (i in trust_vars$code){
  
  dd$trust1 <- label_sdmr(data = dd, var= i)
  
  tbl <- as.data.frame(prop.table(table(dd$trust1))*100)
  tbl$Freq <- round(tbl$Freq,1)
  tbl$var <- label_data[label_data$code %in% i,"name"]
  aggdf <- rbind(aggdf,tbl)
  
}

tbl <- aggdf
library(ggplot2)
p <- ggplot(data=tbl,# data 
            aes(x=Var1, # x-akselin luokitteleva muuttuja
                y=Freq,
                label=Freq)) # y-akselin jatkuva muuttuja
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + geom_text(position=position_dodge(width=1), vjust=1.5,color="white",size=3)
p <- p + guides(fill = guide_legend(reverse=TRUE))
p <- p + theme(axis.text.x = element_text(angle=90))
p <- p + facet_wrap(~var)
print(p)



#+ taulukko
knitr::kable(tbl)
