
## ----dataIn--------------------------------------------------------------
compData <- read.csv("train.csv")
questions <- colnames(compData)[9:109]
library(caTools)
set.seed(101)
split <- sample.split( compData$Happy, SplitRatio = 0.7)
compTrain <- subset(compData, split==TRUE)
compTest <- subset(compData, split==FALSE)
library(cluster)
tabacc <- function(tb) sum(diag(tb))/sum(tb)


## ----qlevels-------------------------------------------------------------

qlevels <- t(sapply(questions, function(q) levels(compData[,q])))
rownames(qlevels) <- questions
hlevels <- levels(compData$HouseholdStatus)



## ----natureOfTheQuestions------------------------------------------------
qidx <- read.csv("questions.csv", stringsAsFactors=FALSE)$Index
qs <- read.csv("questions.csv", stringsAsFactors=FALSE)$Question
qs <- gsub(pattern="/", replacement=" ", qs)
qs <- gsub(pattern="-", replacement=" ", qs)
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(qs))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english") ) 
capFirstLetter <- function(x) paste(toupper(substring(x, 1,1)), substring(x, 2), sep="")

qlists <- lapply(corpus, function(x) Filter(function(s) s != "", unlist(strsplit(x, " ", fixed=TRUE))))
qsAbrv <- unlist(lapply(qlists, function(xs) paste(Map(capFirstLetter, xs), collapse = "") ))

choices <- read.csv("questions.csv", stringsAsFactors=FALSE)$Choices
qsans <- as.matrix(cbind( qsAbrv, choices))


## ----groupStatsByLevel---------------------------------------------------
happyMean <- function(comp) mean(comp$Happy)
computeGroupMeanByLevel <- function(q, comp){
  m <- sapply(levels( comp[ , q]), 
           function(l) {
             s <- sum( comp$Happy[ comp[, q] == l])
             n <- sum(comp[,q]==l)
             if (s==0) 0 else (s/n)
          }
      )
  m
}
computeGroupSdByLevel <- function(q, comp){
  s <-  sapply(levels( comp[ , q]), 
           function(l) {
             s <- sum( comp$Happy[ comp[, q] == l])
             if (s==0) 0 else sd( comp$Happy[comp[,q] == l])
          }
        )
  s
}
computeGroupSizesByLevel<- function(q, comp){
  s <- sapply(levels( comp[ , q]), function(l) sum( comp[,q] == l))
  s
}
computeGroupMeanSdsByLevel <- function(q, comp) {
  sqrt(2*computeGroupMeanByLevel(q, comp)/computeGroupSizesByLevel(q, comp))
}


energyFromMean <- function(m) -log(m/(1-m))

probFromEnergies <- function(es, hmean) {
  e0 <- -log(hmean/(1-hmean))
  totE <- sum( es - e0) + e0
  1/(1 + exp(totE))
}

computeGroupEnergyByLevel <- function(q, comp){
  energyFromMean( computeGroupMeanByLevel(q, comp))
}


## ----statDefs------------------------------------------------------------
hmean <- happyMean(compTrain)
groupMeans.1 <- lapply(c(questions, "HouseholdStatus"), function(q) computeGroupMeanByLevel(q, compTrain))
names(groupMeans.1) <- c(questions, "HouseholdStatus")
groupSizes.1 <- lapply(c(questions, "HouseholdStatus"), function(q) computeGroupSizesByLevel(q, compTrain))
names(groupSizes.1) <- c(questions, "HouseholdStatus")
groupMeanSds.1 <- lapply(c(questions, "HouseholdStatus"),  function(q) computeGroupMeanSdsByLevel(q, compTrain))
names(groupMeanSds.1) <- c(questions, "HouseholdStatus")
groupEnergies.1 <- lapply(groupMeans.1, function(m) energyFromMean(m))
groupZs.1 <- lapply(1:length(groupMeans.1), function(i) (groupMeans.1[[i]] - hmean)/groupMeanSds.1[[i]])
names(groupZs.1) <- c(questions, "HouseholdStatus")
vs.1 <- c(questions, "HouseholdStatus")


## ----svord---------------------------------------------------------------
singleVariables <- c(questions, "HouseholdStatus")
ge <- groupEnergies.1
spread <- function(es) max(es) - min(es)
svord <- singleVariables[order(sapply(ge, spread), decreasing=TRUE)]
dscrmMean <- function(m, s) {
  imax <- which.max(m)
  imin <- which.min(m)
  m[imax] - s[imax] - (m[imin] + s[imin])
}



## ----totalEnergyAndProbHappy---------------------------------------------

totalEnergy <- function(anss, ge, hmean){ #anss is a vector of factor variables
  qns <- names(anss)
  levs <- as.numeric(anss)
  sum(sapply(names(anss), function(q) ge[[q]][anss[[q]]])) - log(hmean/(1-hmean))
}

probHappy <- function(anss, ge, hmean) 1/(1+exp(totalEnergy(anss, ge, hmean)))


## ----energyLinearModel---------------------------------------------------
elm <- function(response, predictors, data, beta = 1, beta0 = 1, ym = 0.5){ #beta parameterizes Poisson conjugate Gamma
  model <- list()
  model$response <- response
  model$predictors <- predictors
  model$data <- data
  R <- data[, response]
  Qs <- as.data.frame(data[, predictors])
  colnames(Qs) <- predictors
  statistics <- list()
  statistics$N <- nrow(Qs)
  statistics$M <- ncol(Qs)
  statistics$ymean <- (sum(R) + beta0*ym)/(length(R) + beta0)
  statistics$groupMeans <- lapply(Qs, function(x){
    sapply(levels(x), function(l){
       s <- sum( R[ x == l])
       n <- sum(x==l)
       (s + beta*statistics$ymean)/(n + beta)
    })
  })  
  statistics$groupSizes <- lapply(Qs, function(x){
    sapply(levels(x), function(l){
      sum(x==l)
    })
  })
  statistics$groupMeanSds <- lapply(1:statistics$M, function(i){
    sqrt(2*statistics$groupMeans[[i]]/statistics$groupSizes[[i]])
  })
  names(statistics$groupMeanSds) <- predictors
  
  statistics$groupZs <- lapply(1:statistics$M, function(i){
    (statistics$groupMeans[[i]] - statistics$ymean)/statistics$groupMeanSds[[i]]
  })
  names(statistics$groupZs) <- predictors
  
  statistics$groupEnergies <- lapply(statistics$groupMeans, function(m) -log(m/(1-m)) )
  
  statistics$groupEnergySds <- lapply(1:statistics$M, function(i){
    statistics$groupMeanSds[[i]]/(statistics$groupMeans[[i]]*(1-statistics$groupMeans[[i]]))
  })
  names(statistics$groupEnergySds) <- predictors
  model$statistics <- statistics
  model
}

predict.elm <- function(model, newdata){
  tes <- do.call("cbind", lapply( model$predictors, function(q){
    model$statistics$groupEnergies[[q]][newdata[, q]]
  }))
  rownames(tes) <- rownames(newdata)
  apply(tes, 1, function(es)probFromEnergies(es, model$statistics$ymean))
}


## ----interaction---------------------------------------------------------

makeInteraction <- function(cd){ # assuming two columns of factors
  uv <- list(sapply(1:nrow(cd), function(i) paste(as.character(cd[i,1]), as.character(cd[i,2]), sep=".")))
  names(uv) <- paste(colnames(cd)[1], colnames(cd)[2], sep=".")
  as.data.frame(uv)
}


## ----interactionExamples-------------------------------------------------
compareIndpInter <- function(ctr, cts, vs){
  X12.train.indp <- ctr[ vs]
  X12.train.indp$Happy <- ctr$Happy
  X12.test.indp <- cts[, vs]
  X12.test.indp$Happy <- cts$Happy
  X12.indp.elm <- elm("Happy", vs, data=X12.train.indp)
  acc.indp.train <- tabacc( table(X12.train.indp$Happy, predict.elm(X12.indp.elm, newdata=X12.train.indp) > 0.5))
  acc.indp.test <- tabacc( table(X12.test.indp$Happy, predict.elm(X12.indp.elm, newdata=X12.test.indp) > 0.5))
  
  X12.train.int <- makeInteraction(ctr[, vs])
  X12.train.int$Happy <- ctr$Happy
  X12.test.int <- makeInteraction(cts[, vs])
  X12.test.int$Happy <- cts$Happy
  X12.int.elm <- elm( "Happy", colnames(X12.train.int)[1], data=X12.train.int )
  acc.int.train <- tabacc(table( X12.train.int$Happy, predict.elm( X12.int.elm, newdata = X12.train.int) > 0.5))
  acc.int.test <- tabacc(table( X12.test.int$Happy, predict.elm( X12.int.elm, newdata = X12.test.int) > 0.5))
  c( indpTrain=acc.indp.train, indpTest=acc.indp.test, intTrain=acc.int.train, intTest=acc.int.test)
}


  


## ------------------------------------------------------------------------
probLevels.singleVariable <- function(q, cd){
  sapply(levels(cd[,q]), function(l) sum(cd[, q] == l))/nrow(cd)
}
entropy <- function(m, base=2) -sum(m*log(m)/log(base))

computeMutualInformation <- function(varsToInteract, data){
  n <- length(varsToInteract)
  h1s <- sapply(varsToInteract, function(v) entropy(probLevels.singleVariable(v, data),3))
  mis <- sapply(1:(n-1), function(i) {
    v1 <- varsToInteract[i]
    sapply((i+1):n, function(j){
      v2 <- varsToInteract[j]
      #v12 <- paste(v1, v2, sep=".")
      #H12 <- entropy(probLevels.singleVariable( v12, data),3)
      iv <- makeInteraction( data[, varsToInteract])
      H12 <- entropy(probLevels.singleVariable(colnames(iv)[1], iv  ), 3)
      H1 <- entropy(probLevels.singleVariable(v1, data),3)
      H2 <- entropy(probLevels.singleVariable(v2, data), 3)
      H1 + H2 - H12
    })
  })
  mismat <- matrix(0, nrow=n, ncol=n)
  for(i in 1:(n-1)){
    mismat[i,i] <- h1s[i]
    for(j in (i+1):n){
      mismat[i,j] <- mis[[i]][j-i]
      mismat[j,i] <- mismat[i,j]
    }
  }
  mismat[n,n] <- h1s[n]
  mismat
}


## ----entropyExamples-----------------------------------------------------
computeMutualInformation(svord[c(1,2,4,5,6,7,8,9)], data=compData)
computeMutualInformation(svord[c(1,2,4,5,6,7,8,9)], data=compData[compData$Happy==1,])
computeMutualInformation(svord[c(1,2,4,5,6,7,8,9)], data=compData[compData$Happy==0,])


## ------------------------------------------------------------------------
disBwnFactors <- function(x,y) sum(x!=y) #Hamming distance
#cd.train.dist <- as.dist(do.call("cbind", lapply(1:n, function(i){
  #ds <- rep(0, n)
  #if (i < n) {ds[(i+1):n] <- sapply((i+1):nrow(cd), function(j) sum( cd[i, varsToCluster] != cd[j, varsToCluster]))}
  #ds
#})))
#cd.train.dist[n,] <- rep(0,n)

varsToCluster <- svord[1:10]
varsToTrain <- svord[1:4]
cd.dist <- as.matrix(daisy(compData[, varsToCluster]))
cd.test.dist.totrain <- cd.dist[rownames(compTest), rownames(compTrain)]
cd.train.dist <- daisy(compTrain[, varsToCluster])

cd.train.hclu <- hclust( cd.train.dist, method="ward")
k = 16
cd.train.hcluCut <- cutree(cd.train.hclu, k = k)
hapsize <- t(sapply(1:k, function(i) c(happiness=mean(compTrain$Happy[cd.train.hcluCut == i]), size=sum(cd.train.hcluCut==i))))

acc.train.c <- sapply(1:k, function(i){
  cluster.elm <- elm("Happy", varsToTrain, compTrain[cd.train.hcluCut==i, c(varsToTrain, "Happy")], beta=1., beta0=1, ym=hmean)
  pred.clu <- predict.elm(cluster.elm, newdata=compTrain[cd.train.hcluCut==i, c(varsToTrain, "Happy")])
  tabacc(table(compTrain$Happy[cd.train.hcluCut==i], pred.clu > 0.5))
})

compTrain.elm <- elm("Happy", varsToTrain, compTrain[, c(varsToTrain, "Happy")], beta=1, beta0=1, ym=0.5)
acc.train.o <- sapply(1:k, function(i){
  pred.clu <- predict.elm(compTrain.elm, newdata=compTrain[cd.train.hcluCut==i, c(varsToTrain, "Happy")])
  cutoff <- mean(compTrain$Happy[cd.train.hcluCut == i]) 
  #cutoff <- mean(compTrain$Happy)
  #cutoff <- 0.5
  tabacc(table(compTrain$Happy[cd.train.hcluCut==i], pred.clu > cutoff))
})
hapsizeacc <- cbind(hapsize, acc.train.c, acc.train.o)
hapsizeacc
sum( hapsizeacc[,1]*hapsizeacc[,2])/sum(hapsizeacc[,2])
sum( hapsizeacc[,3]*hapsizeacc[,2])/sum(hapsizeacc[,2])
sum( hapsizeacc[,4]*hapsizeacc[,2])/sum(hapsizeacc[,2])


## ------------------------------------------------------------------------
knnClusterPredict <- function(clusters, distances, k){
  if( class(distances) == "list"){
    lapply(distances, function(d) names(which.max(table(clusters[names(sort(d))[1:k]]))))
  }
  else{ #assuming matrix
    apply(distances, 1, function(d) names(which.max(table(clusters[names(sort(d))[1:k]]))) )  
  }
}
cd.train.dist.mat <- as.matrix(cd.train.dist)
cd.train.dist.list <- lapply(1:nrow(compTrain), function(i) cd.train.dist.mat[i, -i])
cd.train.clusters.predicted <- as.numeric(do.call("c", knnClusterPredict( cd.train.hcluCut, cd.train.dist.list, 5)))
acc.train.c.pred <- sapply(1:k, function(i){
  cluster.elm <- elm("Happy", varsToTrain, compTrain[cd.train.hcluCut==i, c(varsToTrain, "Happy")], beta=0.01, beta0=1, ym=hmean)
  pred.clu <- predict.elm(cluster.elm, newdata=compTrain[cd.train.clusters.predicted==i, c(varsToTrain, "Happy")])
  tabacc(table(compTrain$Happy[cd.train.clusters.predicted==i], pred.clu > 0.5))
})
hapsizeacc.pred <- cbind(hapsizeacc, acc.train.c.pred)
sum( hapsizeacc.pred[,5]*hapsizeacc[,2])/sum(hapsizeacc[,2])



## ------------------------------------------------------------------------

kn <- 5
cd.test.clusters.predicted <- as.numeric( knnClusterPredict( cd.train.hcluCut, cd.test.dist.totrain, kn))
cd.train.clusters.predicted <- as.numeric(do.call("c", knnClusterPredict( cd.train.hcluCut, cd.train.dist.list, kn)))

#models
cd.train.hcluCut.elms <- lapply(1:k, function(i) elm("Happy", varsToTrain, compTrain[cd.train.hcluCut==i, c(varsToTrain, "Happy")], beta=2., beta0=1, ym=hmean))
cd.test.preds <- sapply(1:nrow(compTest), function(i) predict.elm( cd.train.hcluCut.elms[[cd.test.clusters.predicted[i]]], newdata=compTest[i, c(varsToTrain, "Happy")]))
cd.train.preds <- sapply(1:nrow(compTrain), function(i) predict.elm( cd.train.hcluCut.elms[[cd.train.clusters.predicted[i]]], newdata=compTrain[i, c(varsToTrain, "Happy")]))
tabacc( table( compTrain$Happy, cd.train.preds > 0.5))
tabacc(table(compTest$Happy, cd.test.preds > 0.5))

accuraciesTestTrainClustered <- function(i, vs){
  X12.train.indp <- compTrain[cd.train.hcluCut==i, vs]
  X12.train.indp$Happy <- compTrain$Happy[cd.train.hcluCut==i]
  X12.test.indp <- compTest[cd.test.clusters.predicted==i, vs]
  X12.test.indp$Happy <- compTest$Happy[cd.test.clusters.predicted==i]
  X12.indp.elm <- elm("Happy", vs, data=X12.train.indp)
  acc.indp.train <- tabacc( table(X12.train.indp$Happy, predict.elm(X12.indp.elm, newdata=X12.train.indp) > 0.5))
  acc.indp.test <- tabacc( table(X12.test.indp$Happy, predict.elm(X12.indp.elm, newdata=X12.test.indp) > 0.5))
  c(size.train=sum(cd.train.hcluCut==i), acc.train=acc.indp.train, size.test=sum(cd.test.clusters.predicted==i), acc.test=acc.indp.test)
}


## ------------------------------------------------------------------------
varsToCluster <- svord[1:102]
cd.dist <- daisy(compData[, c("Income", "EducationLevel", "Gender", varsToCluster)])
cd.hclu <- hclust( cd.dist, method="ward")
k = 16
cd.hcluCut <- cutree(cd.hclu, k = k)
names(cd.hcluCut) <- rownames(compData)
varsToTrain <- svord[1:6]
clusters.train <- cd.hcluCut[rownames(compTrain)]
clusters.test <- cd.hcluCut[rownames(compTest)]
cd.hcluCut.elms <- lapply(1:k, function(i) elm("Happy", varsToTrain, compTrain[clusters.train==i, c(varsToTrain, "Happy")], beta=2., beta0=1, ym=hmean))
cd.test.preds <- sapply(1:nrow(compTest), function(i) predict.elm( cd.hcluCut.elms[[clusters.test[i]]], newdata=compTest[i, c(varsToTrain, "Happy")]))
cd.train.preds <- sapply(1:nrow(compTrain), function(i) predict.elm( cd.hcluCut.elms[[clusters.train[i]]], newdata=compTrain[i, c(varsToTrain, "Happy")]))
tabacc( table( compTrain$Happy, cd.train.preds > 0.5))
tabacc(table(compTest$Happy, cd.test.preds > 0.5))
