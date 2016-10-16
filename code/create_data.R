d <- haven::read_sav("./data/structure_2015_with_class.sav")
label_data <- data.frame()
for (i in 1:ncol(d)){
  df <- data.frame()
  code  <- names(d[i])
  name <- attributes(d[[i]])$label
  labels <- names(attributes(d[[i]])$labels)
  if (is.null(labels)){
    values = unique(d[[i]])
    labels=NA
  } else {
    values = as.integer(attributes(d[[i]])$labels)
  }
  if (is.null(name)) name="not applicaple"
  df <- data.frame(code=code,
                   name=name,
                   labels=labels,
                   values=values, stringsAsFactors=FALSE)
  label_data <- rbind(label_data,df)
}
label_data_orig <- label_data
save(label_data_orig, file="./data/label_data_orig.RData")
save(label_data, file="./data/label_data.RData")

# Remove the attributes!
for (i in 1:ncol(d)) {
  z<-class(d[[i]])
  if (z[[1]]=='labelled'){
    class(d[[i]])<-z[-1]
    attr(d[[i]],'label')<-NULL
    attr(d[[i]],'labels')<-NULL
    attr(d[[i]],'names')<-NULL
  } else {
    attr(d[[i]],'names')<-NULL
    attr(d[[i]],'label')<-NULL
  }
}
structure_2015_with_class <- d
save(structure_2015_with_class, file="./data/structure_2015_with_class.RData")
