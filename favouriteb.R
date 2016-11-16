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
  corpus.tmp <- tm_map(corpus.tmp, removeWords, stopwords("russian"))
  return (corpus.tmp)
}

#build TDM
generateTDM<-function(candidate, path){
  s.dir <- sprintf("%s/%s", path, candidate)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding="UTF8"))
  s.cor.cl <- clearCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = candidate, tdm = s.tdm)
  return (result)
}
tdm <- lapply(candidates, generateTDM, path = pathname)

#attach name

#stack

#hold-out

#model KNN