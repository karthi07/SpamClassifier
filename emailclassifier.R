setwd("Documents/ML/Classification/SpamClassifier/")
#install.packages("tm")
library(tm)
spam.path <- 'data/spam'
easyham.path <- 'data/easy_ham'
hardham.path <- 'data/hard_ham'
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

spam.occurence <- sapply(1:nrow(spam.matrix), function(i)
  {length(which(spam.matrix[i,]>0))/ ncol(spam.matrix)})
spam.density <- spam.df$frequency / sum(spam.df$frequency)

spam.df <- transform(spam.df,density = spam.density, occurence = spam.occurence)

head(spam.df[with(spam.df,order(-occurence)),])


# read ham mails


easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(paste(easyham.path,p,sep="/")))
head(all.easyham)

easyham.tdm <- get.tdm(all.easyham)


easyham.matrix <- as.matrix(easyham.tdm)
easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts),
                               as.numeric(easyham.counts)),stringsAsFactors=FALSE)

names(easyham.df) <- c('term','frequency')
easyham.df$frequency <- as.numeric(easyham.df$frequency) 
easyham.occurrence <- sapply(1:nrow(easyham.matrix),
                             function(i){length(which(easyham.matrix[i,]>0))/ncol(easyham.matrix)})
easyham.density <- easyham.df$frequency /sum(easyham.df$frequency)
easyham.df <- transform(easyham.df,occurrence =easyham.occurrence,density=easyham.density)
 

head(easyham.df[with(easyham.df,order(-occurrence)),])


classify.email <- function(path,training.df,prior=0.5,c=1e-6){
  msg <- get.msg(path)
  msg.tdm <- get.tdm(msg)
  msg.freq <- rowSums(as.matrix(msg.tdm))
  msg.match <- intersect(names(msg.freq), training.df$term)
  if(length(msg.match) < 1){
    return(prior*c^(length(msg.freq)))
  }
  
  else {
    match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
    return(prior * prod(match.probs)* c^(length(msg.freq)-length(msg.match)))
  }
}

#testing ti wtih hard ham

hardham.docs <- dir(hardham.path)
hardham.docs <- hardham.docs[which(hardham.docs != "cmds")]
hardham.spamtest <- sapply(hardham.docs,
                           function(p) classify.email(paste(hardham.path,p,sep="/"),
                                                      training.df = spam.df))
hardham.hamtest <- sapply(hardham.docs,
                          function(p) classify.email(paste(hardham.path,p,sep="/"),
                                                     training.df = easyham.df))
hardham.res <- ifelse(hardham.spamtest > hardham.hamtest,TRUE,FALSE)
summary(hardham.res)

  
spam.classifier <- function(path) {
  pr.spam <- classify.email(path, spam.df)
  pr.ham <- classify.email(path, easyham.df)
  return(c(pr.spam, pr.spam, ifelse(pr.spam > pr.ham,1,0)))
  
}  

  
  