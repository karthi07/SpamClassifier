setwd("Documents/ML/Classification/SpamClassifier/")
#install.packages("tm")
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


spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs,function(p)get.msg(paste(spam.path,p,sep="/")))
head(all.spam)

#Creating TDM

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))
  control <- list(stopwords=TRUE,removePunctuation=TRUE,removeNumbers=TRUE,minDocFreq=2)
  doc.tdm <- TermDocumentMatrix(doc.corpus,control)
  
  return(doc.tdm)
}

spam.tdm <- get.tdm(all.spam)


#prob of terms in msg

spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
names(spam.counts)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),stringsAsFactors=FALSE)
head(spam.df)
names(spam.df) <- c('term','frequency') 

str(spam.df)
spam.df$frequency <- as.numeric(spam.df$frequency)











