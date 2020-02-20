library(tm)
library(data.table)
library(DT)
library(wordcloud2)
library(RColorBrewer)
library(dplyr)
library(tidytext)
library(tidyverse)
library(rvest)
library(tibble)
library(syuzhet)
library(sentimentr)
library(gplots)
library(factoextra)
library(beeswarm)
library(scales)
library(RANN)
library(topicmodels)
library(stringr)
library(qdap)
library(tibble)
library(reshape2)
library(plotrix)
library(heatmaply)
library(ggpubr)

rap_full <- read.csv("C:/Users/ZZ2677/Desktop/LDA/rap.csv")
k <-6
rap_corpus <- Corpus(VectorSource(rap_full$stemmedwords))
rap_tdm <- TermDocumentMatrix(rap_corpus)
tdm <- tidy(rap_tdm) %>% cast_dtm(document = document, term = term, value = count)
ldaout <- LDA(tdm, k=6, control = list(seed = 1))


ldaOut.topics <- as.matrix(topics(ldaout))
table(c(1:k, ldaOut.topics))

write.csv(ldaOut.topics,file=paste("C:/Users/ZZ2677/Desktop/LDA/",k,"DocsToTopics.csv"))

ldaOut.terms <- as.matrix(terms(ldaout,20))
write.csv(ldaOut.terms,file=paste("C:/Users/ZZ2677/Desktop/LDA/",k,"TopicsToTerms.csv"))


topicProbabilities <- as.data.frame(ldaout@gamma)


terms.beta=ldaout@beta
terms.beta=scale(terms.beta)
topics.terms=NULL
for(i in 1:k){
  topics.terms=rbind(topics.terms, ldaout@terms[order(terms.beta[i,], decreasing = TRUE)[1:k]])
}
topics.terms

topicProbabilitiesp <- as.data.frame(ldaout@gamma)

rap_full$ldatopic=as.vector(ldaOut.topics)
topics.hash=c("Negative Emotion","Trust","Love","Life","Money","Party")
rap_full$ldahash=topics.hash[ldaOut.topics]
colnames(topicProbabilities)=topics.hash
rap=cbind(rap_full,topicProbabilities)

topic.summary=tbl_df(rap)%>%
  select(year, "Negative Emotion":"Party")%>%
  group_by(year)%>%
  summarise_at(vars("Negative Emotion":"Party"),mean)
topic.summary=as.data.frame(topic.summary)
rownames(topic.summary)=topic.summary[,1]
topic.plot=c(1, 2,3,4,5,6)
 g4<-heatmap.2(as.matrix(topic.summary[,topic.plot+1]), 
          scale = "column", key=F, 
          col = bluered(100),
         cexRow = 0.9, cexCol = 0.9, margins = c(8, 8),
         trace = "none", density.info = "none",main="Hip-Hop music heatmap")
g4
















