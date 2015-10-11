setwd("Documents/ML/Classification/SpamClassifier/")
install.packages("tm")
library(tm)
spam.path <- 'data/spam'
easyham.path <- 'data/easyham'

get.msg <- function(path){
  con <- file(path,open="rt",encoding="latin1")
  text <- readLines(con)
  msg <- text [seq(which(text=="")[1]+1,length(text),1)]
  close(con)
  return(paste(msg,collapse="\n"))
  
}