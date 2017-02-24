###Entra a Twiterr

###Entra a Twiterr
library(RCurl)
library(SnowballC)
library(tm)
library(twitteR)
library(ROAuth)
api_key <- "TEqtGTHhGpl0gaEj0YlacXSMP"
api_secret <- "C2Uyt9dN8REdTlTkRBY7gFtMhLgVeNtbpEjefKpiBKO8Ux7ns0"
access_token <- "831261005850120192-LQ39DWYq0KZOXqwldxMWHwB1AYUgdUv"
access_token_secret <- "z5PBlltSCwh5wuiXfgE0BHI5aR19QUmvPg0d5TzNeblBF"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##### Limpieza de Texto

#######
tweets <- userTimeline("marianaSalas", n = 3200) #Cargar en la variable tweets la informaciÃÂ³n de todos los twetter que  ella puso
retweets<-searchTwitter('@marianaSalas',n=100) #Cargar en la variable retweets la informaciÃÂ³n de todos los twetter en los que la mencionaron
tweets.df <- twListToDF(tweets)#hacer un data frame con la informaciÃÂ³n en tweets
retweets.df<-twListToDF(retweets)#hacer un data frame con la informaciÃÂ³n en retweets
#dump('tweets.df',file=xxx) Crea un archivo .txt en la ruta que ingreses en file

texttweets<-tweets.df$text
removegraph<-function(x) gsub("[^[:graph:]]", " ",x)
texttweets<-removegraph(texttweets)
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
texttweets<-removeURL(texttweets)
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
texttweets<-removeNumPunct(texttweets)
texttweets<-stemDocument(texttweets)
myStopwords <- c(stopwords("spanish"), "un","la","el","marianasalas")
texttweets<-removeWords(texttweets,myStopwords)
texttweets<-stripWhitespace(texttweets)
texttweets<-tolower(texttweets)
texttweets<-removeWords(texttweets,myStopwords)


textretweets<-retweets.df$text
textretweets<-removegraph(textretweets)
textretweets<-removeURL(textretweets)
textretweets<-removeNumPunct(textretweets)
textretweets<-stemDocument(textretweets)
textretweets<-removeWords(textretweets,myStopwords)
textretweets<-stripWhitespace(textretweets)
textretweets<-tolower(textretweets)
textretweets<-removeWords(textretweets,myStopwords)


texttweets<-Corpus(VectorSource(texttweets))
tdm <- TermDocumentMatrix(texttweets, control = list(wordLengths = c(1, Inf)))
(freq.terms <- findFreqTerms(tdm, lowfreq=3))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=7)
df <- data.frame(term = names(term.freq), freq = term.freq)


textretweets<-Corpus(VectorSource(textretweets))
tdmre <- TermDocumentMatrix(textretweets, control = list(wordLengths = c(1, Inf)))
(freq.termsre <- findFreqTerms(tdmre, lowfreq=3))
term.freqre <- rowSums(as.matrix(tdmre))
term.freqre <- subset(term.freqre, term.freqre >=3)
dfre <- data.frame(term = names(term.freqre), freq = term.freqre)



#########Grafico de Frecuencias (ggplot 2) para Tweets###############
library(ggplot2)

p<- ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Palabra") + ylab("Frecuencia") +coord_flip()
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p ##Se ejecuta la grafica

#########Grafico de Frecuencias (ggplot 2) para retweets###############

pr<- ggplot(dfre, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Palabra") + ylab("Frecuencia") +coord_flip()
pr <- pr + theme(axis.text.x=element_text(angle=45, hjust=1))   
pr ##Se ejecuta la grafica

################## Nube de Palabras de los Tweets##############################
library(wordcloud)
set.seed(1234) #### Numero aleatorio
m <- as.matrix(tdm)
##### Calcula la nube de palabras con un maximo de 200
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

################## Nube de Palabras de las mensiones##############################

set.seed(1234) #### Numero aleatorio
mr <- as.matrix(tdmre)
##### Calcula la nube de palabras con un maximo de 200
word.freqr <- sort(rowSums(mr), decreasing = T)
wordcloud(words = names(word.freqr), freq = word.freqr, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

########## 1. Agrupamiento jerárquico##################################
############################################################################
# En minería de datos, el agrupamiento jerárquico es un método de análisis de grupos puntuales, el cual busca construir una jerarquía de grupos. Estrategias para agrupamiento jerárquico generalmente caen en dos tipos:
#Aglomerativas: Este es un acercamiento ascendente: cada observación comienza en su propio grupo, y los pares de grupos son mezclados mientras uno sube en la jerarquía.
#Divisivas: Este es un acercamiento descendente: todas las observaciones comienzan en un grupo, y se realizan divisiones mientras uno baja en la jerarquía.
set.seed(12354)
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# distancia del cluster
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6,border="red") # dibuja en rojo los grupoos 

m_hclust<-hclust(distMatrix, method= "average")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "ward.D2")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "single")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "complete")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "mcquitty")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "median")
plot(m_hclust)

m_hclust<-hclust(distMatrix, method= "centroid")
plot(m_hclust)


############# Para retwitters ############################

tdmre2 <- removeSparseTerms(tdmre, sparse = 0.95)
me2 <- as.matrix(tdmre2)
# distancia del cluster
distMatrixe <- dist(scale(me2))
fitre <- hclust(distMatrixe, method = "ward.D")
plot(fitre)
rect.hclust(fitre, k = 5,border="red") # dibuja en rojo los grupos

m_hclust<-hclust(distMatrixe, method= "average")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "ward.D2")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "single")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "complete")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "mcquitty")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "median")
plot(m_hclust)

m_hclust<-hclust(distMatrixe, method= "centroid")
plot(m_hclust)




###############2. K-Means Algoritmos de Analisis Grupa;#############################


m3 <- t(m2) # Transpone la matriz m2
set.seed(122) # una semilla random nueva
k <- 6 # elegimos el numero de cluster k-means
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # centros del Grupoide

#################Grafica de K-means######################
library(fpc)
library(cluster)
d <- dist(t(tdm2), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(tdm2), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
##########################################################################




##################3.      ##################################################
library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
k <- pamResult$nc # number of clusters identified

pamResult <- pamResult$pamobject
# print cluster medoids
for (i in 1:k) {
  cat("cluster", i, ": ",
      colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
}
# plot clustering result
layout(matrix(c(1, 2), 1, 2)) # set to two graphs per page
plot(pamResult, col.p = pamResult$clustering)

for (i in 1:k){
  cat(paste("cluster",i,": ", sep =""))
  s <- sort(kmeansResult$centers[i, ],decreasing = T)
  cat(names(s)[1:5], "\n")
}

(n.tweet <- length(tweets))






################################# 4. Analisis de Topicos ################


dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)
lda <- LDA(dtm, k = 6)
term <- terms(lda, 6) 
(term <- apply(term, MARGIN = 2, paste, collapse = ", "))
library("lda")

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
library(data.table) #fore IDate
topic <- topics(lda, 1)
topics <- data.frame(date=as.IDate(tweets.df$created), topic)
qplot(date, ..count.., data=topics, geom="density",
      fill=term[topic], position="stack")
#############################################################

######### Analisis de Sentimientos
require(devtools)
install_github("sentiment140", "okugami79")
######### sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
##
t
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data = sentiments, sum)
plot(result, type = "l")
############################


 #### Analisis de Grafos#######

library("igraph")
m2[m2>=1] <- 1
m4<- m2 %*% t(m2)

g <- graph.adjacency(m4, weighted=T, mode="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
V(g)$label.color <- rgb(0, 0, .2, .8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
E(g)$width <- egam
plot(g, layout=layout1)
#############################


