dd <- haven::read_sav("./data/structure_2015_with_class.sav")

puuttuvat <- names(d)[!names(d) %in% names(dd)]

ee <- cbind(dd, d[puuttuvat])
table(ee$wright, useNA="ifany")
table(ee$kivinen_class_08, useNA="ifany")

round(prop.table(table(ee$wright, ee$kivinen_class_08, useNA="ifany"),1)*100,1)


round(prop.table(table(ee$wright, ee$V81c1, useNA="ifany"),1)*100,1)
round(prop.table(table(ee$V81c1, ee$kivinen_class_08, useNA="ifany"),2)*100)
ee$V81c1 <- label_sdmr(ee, "V81c1")

load("./data/d15.RData")
ee <- d15

library(ggplot2)

dat <- ee %>% filter(V74c1 < 250000)
ggplot(dat, aes(x=factor(wright),
                y=V74c1)) + geom_boxplot()

ggplot(dat, aes(x=factor(kivinen_class_08),
                y=V74c1)) + geom_boxplot()
  
ggplot(dat, aes(x=factor(skill2_labeled),
                y=V74c1)) + geom_boxplot()

dat$V113c1 <- label_sdmr(dat, "V113c1")

ggplot(dat, aes(x=factor(kivinen_class_08),
                y=V74c1)) + geom_boxplot() +
  facet_wrap(~V113c1)

