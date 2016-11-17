#init
libs<-c("tm","plyr","class")
lapply(libs, require, character.only=TRUE)
options(stringsAsFactors = FALSE)
doc_class<-c("favourite","not_favourite")
pathname<-"/home/eugene/Desktop/books"

#clean text
cleanCorpus<-function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  corpus.tmp <- tm_map(corpus.tmp, PlainTextDocument)
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("russian"))
  return (corpus.tmp)
}

#build TDM
generateTDM<-function(candidate, path){
  s.dir <- sprintf("%s/%s", path, candidate)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding="UTF-8"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = candidate, tdm = s.tdm)
  return (result)
}
tdm <- lapply(doc_class, generateTDM, path = pathname)

#attach name
bindCandidateToTDM <- function(tdm) {
  s.mat <- t(data.matrix(tdm['tdm']))
  s.df  <- as.data.frame(s.mat, stringAsFactors = FALSE)
  s.df  <- cbind(s.df, rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <-"targetCandidate"
  return (s.df)
}
candTDM <- lapply(tdm, bindCandidateToTDM)

#stack
tdm.stack <- do.call(rbind.fill, candTDM)
tdm.stack[is.na(tdm.stack)] <- 0

#hold-out

#model KNN