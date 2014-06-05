library(network)
library(igraph)

setwd("/Users/liuxihan/Dropbox/14spring/SocialMedia/final")
senti <- read.csv("Sentiment scores.csv")

## Generating edges
edgesGen <- function(senti){

  weight <- matrix(NA, ncol(senti)*(ncol(senti)-1), 3)
  rowcounter <- 1
  for(i in 1:10){
    for(j in 1:10){
      sum_itoj <- 0
      count_itoj <- 0

      if(i!=j) {

        for(n in 1:nrow(senti)){
                  if(!is.na(senti[n,i]) & !is.na(senti[n,j])) {
                    if(senti[n,i] > senti[n,j]){
                      sum_itoj <- sum_itoj + senti[n,i] - senti[n,j]
                      count_itoj <- count_itoj +1
                    }
                  }
        }
       weight[rowcounter, 1] <- colnames(senti)[i]
       weight[rowcounter, 2] <- colnames(senti)[j]
       weight[rowcounter, 3] <- sum_itoj / count_itoj
       rowcounter <- rowcounter +1
       }
    }
  }
  return(weight)
}

edges <- edgesGen(senti)

new_edges <- cbind(data.frame(edges[,1:2]), data.frame(as.numeric(edges[,3])))
com_edges <- new_edges[complete.cases(new_edges),]

## Creating network
brandNet <- network.initialize(10, multiple = TRUE, loops=TRUE)
elData<-data.frame(
  from_id=c(com_edges[,1]),
  to_id=c(com_edges[,2]),
  myEdgeWeight=c(com_edges[,3]),
  stringsAsFactors=FALSE
)

add.edges(brandNet,elData[,1],elData[,2],
          names.eval=rep(list(list("myEdgeWeight")),nrow(elData)))

## Setting the size of the vertices
vetexSizes <- c(mean(senti[,5], na.rm=TRUE),
                mean(senti[,4], na.rm=TRUE),
                mean(senti[,1], na.rm=TRUE),
                mean(senti[,2], na.rm=TRUE),
                mean(senti[,3], na.rm=TRUE),
                mean(senti[,10], na.rm=TRUE),
                mean(senti[,6], na.rm=TRUE),
                mean(senti[,7], na.rm=TRUE),
                mean(senti[,8], na.rm=TRUE),
                mean(senti[,9], na.rm=TRUE))

## Dichotomous network
plot(brandNet, label = sort(colnames(senti)), vertex.cex = 3*vetexSizes)

brandgraph <- graph.data.frame(elData)
# Another plot 
# plot(brandgraph)

## PageRank
PageRank <- page.rank(brandgraph, weights = elData$myEdgeWeight)
as.matrix(sort(PageRank$vector))

## The sales of the products
sales <- c(135, 30, 120, 12, 20, 220, 60, 14, 6.6, 25)
data <- data.frame(cbind(PageRank$vector, sales))
reg <- lm(log(sales) ~ V1, data)
summary(reg)
