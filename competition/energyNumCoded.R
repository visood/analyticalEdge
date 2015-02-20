
## ------------------------------------------------------------------------

n <- 0.25
m <- 0.25
xfunc <- function(size=1) {
  sapply(1:size, function(i){
    u <- runif(n=1)
    if (u < n)  -1
    else {
      if (u < (1-m)) 0
      else 1
    }
  })
}
p <- c("-1"=0.,"0"=0.5, "1"=1.)
yfunc <- function(xs) sapply(xs, function(x)if (runif(n=1) < p[x]) 1 else 0)
numCode <- c("-1"=-1, "0"=0, "1"=1)
N <- 10000
X <- as.factor(xfunc(N))
Y <- yfunc(as.character(X))
XY <- data.frame(X=X, Y=Y)
XY$X <- as.character(XY$X)
freqs <- sapply(unique(XY$X), function(x) sum( XY$X == x))
XY$X <- as.factor(XY$X)
XY$X <-relevel(XY$X, names(which.max(freqs)))
#XY$X <- relevel(XY$X, "0")
XY$num <- numCode[ as.character(XY$X)]
XY.log <- glm( Y ~ X, data=XY, family="binomial")
XY.num.log <- glm(Y ~ num, data=XY, family="binomial")
groupMeans <- sapply(c("-1", "0","1"), function(x) mean(XY$Y[ as.character(XY$X) == x]))
groupSds <- sapply(c("-1", "0","1"), function(x) sd(XY$Y[ as.character(XY$X) == x]))
nd <- data.frame( X=as.factor(c(-1, 0, 1)), num=c(-1, 0, 1), Y=c(0, 1, 0.5))
predFac <- predict(XY.log, newdata=nd, type="response")
predNum <- predict(XY.num.log, newdata=nd, type="response")
cbind(nd, groupMeans, predFac, predNum, groupSds)
summary(XY.log)
summary(XY.num.log)


## ------------------------------------------------------------------------
allXY <- as.data.frame(do.call("rbind", lapply(c(-1,0,1), function(x) do.call("rbind", lapply(c(-1,0,1), function(y) c(x,y))) )))
colnames(allXY) <- c("X", "Y")
allXY <- allXY[order( 3*allXY$X + allXY$Y),]
predFun <- function(size=1, p) {
  sapply(1:size, function(i){
    u <- runif(n=1)
    if (u < p[1])  -1
    else {
      if (u < (1-p[2])) 0
      else 1
    }
  })
}
#respFun <- function(xys, p) sapply(xs, function(x)if (runif(n=1) < p[p[,1]==x,2]) 1 else 0)
respFun <- function(xys, p) apply( xys, 1, function(xy) if( runif(n=1) < p[ p$X==xy[1] & p$Y==xy[2], 3]) 1 else 0 )
numCode <- c("-1"=-1, "0"=0, "1"=1)
px <- c(0.25, 0.25)
py <- c(0.25, 0.25)
pz <- data.frame(allXY, Z=apply(allXY, 1,  function(r) (sum(r) + 2)/4))
N <- 10000
X <- predFun(N, px)
Y <- predFun(N, py)
Z <- respFun(data.frame(X=X, Y=Y), pz)
XYZ <- data.frame(X=as.factor(X), Y=as.factor(Y), Z=Z)
XYZ$X <- relevel(XYZ$X, names(which.max(table(XYZ$X))))
XYZ$Y <- relevel(XYZ$Y, names(which.max(table(XYZ$Y))))
XYZ$nX <- numCode[as.character(XYZ$X)]
XYZ$nY <- numCode[as.character(XYZ$Y)]
xyz.log <- glm( Z ~ X + Y, data=XYZ)
xyz.num.log <- glm(Z ~ nX + nY, data=XYZ)
summary(xyz.log)
summary(xyz.num.log)
nd <- allXY
nd$X <- as.factor(nd$X)
nd$Y <- as.factor(nd$Y)
nd$nX <- numCode[as.character(nd$X)]
nd$nY <- numCode[as.character(nd$Y)]

predFac <- predict(xyz.log, newdata=nd, type="response")
predNum <- predict(xyz.num.log, newdata=nd, type="response")

groupMeans <- apply(allXY, 1, function(r) mean( XYZ$Z[ as.character(XYZ$X) == r[1] & as.character(XYZ$Y) == r[2]]))
groupSds <- apply(allXY, 1, function(r) sd( XYZ$Z[ as.character(XYZ$X) == r[1] & as.character(XYZ$Y) == r[2]]))
#cbind(ndXY, predFac, predNum, groupMeans, groupSds)


## ------------------------------------------------------------------------
comp <- read.csv("train.csv")
questions <- colnames(comp)[9:109]
library(caTools)
split <- sample.split( comp$Happy, SplitRatio = 0.7)
compTrain <- subset(comp, split==TRUE)
compTest <- subset(comp, split==FALSE)


## ------------------------------------------------------------------------
computeGroupMeanByLevel <- function(q, compdf){
  sapply(levels( compdf[ , q]), function(l) mean( compdf[ compdf[, q] == l, "Happy"]) )
}
        
groupMeans <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupMeanByLevel(q, compTrain))))
rownames(groupMeans) <- questions
colnames(groupMeans) <- c("blank", "No", "Yes")
hb <- hist(groupMeans$blank, plot=FALSE)
hn <- hist(groupMeans$No, plot=FALSE)
hp <- hist(groupMeans$Yes, plot=FALSE)
plot(hb, freq=FALSE, border="green", xlim=c(0.4, 0.7), main="Histogram of the three responses", xlab="group means")
lines(hn, freq=FALSE, border="blue")
lines(hp, freq=FALSE, border="red")


## ----natureOfTheQuestions------------------------------------------------
qidx <- read.csv("questions.csv", stringsAsFactors=FALSE)$Index
#names(questions) <- qidx
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



## ----questionAnswers-----------------------------------------------------
choices <- read.csv("questions.csv", stringsAsFactors=FALSE)$Choices
choices <- data.frame(do.call("rbind", lapply(strsplit(x=choices, split="/"), function(xs)sapply(xs, function(x)  gsub(pattern=" ", replacement="", x)))))
choices <- t(apply(choices, 1, function(r) c("blank", r)))
colnames(choices) <- c("A0", "A1", "A2")
qs <- as.matrix(cbind( qsAbrv, choices))
rownames(qs) <- qidx
colnames(qs) <- c("Q", "A0", "A1", "A2")

#reorder the rows of qs to confirm to the order in the columns of comp dfs
qs <- qs[questions, ]

toset <- apply(qs, 1, function(r) (r[2] != "Yes") | (r[3] != "No"))
torev <- c(7, 8, 9, 10, 11, 13, 14, 16, 18, 20, 22, 25, 26, 27, 29, 33, 35, 36, 37, 41, 42, 43, 45, 48, 49, 50, 54, 56, 57, 59, 60, 61, 64, 66, 68, 69, 70, 74, 77, 78, 79, 80, 84, 86, 88, 91, 92, 94, 97, 98, 100, 101  )

numCode <- cbind(rep(0, 101), rep(1, 101), rep(-1, 101))
for(i in torev){
  numCode[i,2] <- -1
  numCode[i,3] <- 1
}
rownames(numCode) <- qidx
colnames(numCode) <- c("A0n", "A1n", "A2n")
qsans <- data.frame(qs, numCode)


## ----numericAnswers------------------------------------------------------
makeNum <- function(q, xs) {
  sapply(xs, function(x){
    if (x=="") 0
    else {
      if( gsub(pattern=" ", replacement="", x) == qsans[q, "A1"]) qsans[q, "A1n"]
      else qsans[q, "A2n"]
    }
  })
}

compTrainNum <- compTrain
compTestNum <- compTest
for( q in questions){
  compTrainNum[,q] <- makeNum(q, compTrainNum[,q])
  compTestNum[,q] <- makeNum(q, compTestNum[,q])
}


## ----numhistos-----------------------------------------------------------
computeGroupMeanNum <- function(q, compNum){
  sapply( c(-1, 0, 1), function(x) mean( compNum[ compNum[, q] == x, "Happy"]) )
}
computeGroupSdNum <- function(q, compNum){
  sapply( c(-1, 0, 1), function(x) sd( compNum[ compNum[, q] == x, "Happy"]) )
}
groupMeansNum <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupMeanNum(q, compTrainNum))))
rownames(groupMeansNum) <- questions
colnames(groupMeansNum) <- c("Down", "Flat", "Up")
groupSdsNum <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupSdNum(q, compTrainNum))))
rownames(groupSdsNum) <- questions
colnames(groupSdsNum) <- c("Down", "Flat", "Up")
hd <- hist(groupMeansNum$Down, plot=FALSE)
hf <- hist(groupMeansNum$Flat, plot=FALSE)
hu <- hist(groupMeansNum$Up, plot=FALSE)

plot(hd, freq=FALSE, border="blue", xlim=c(0.4, 0.7), ylim=c(0,50), main="Histogram of group means", xlab="group means")
lines(hf, freq=FALSE, border="green")
lines(hu, freq=FALSE, border="red")
legend("topleft", legend=c("down", "flat", "up"), col=c("blue", "green", "red"), lty=1, cex=2)

hdl <- hist(log(groupMeansNum$Down), plot=FALSE)
hfl <- hist(log(groupMeansNum$Flat), plot=FALSE)
hul <- hist(log(groupMeansNum$Up), plot=FALSE)
plot(hdl, freq=FALSE, border="blue", xlim=c(-1,-0.3), ylim=c(0,25), main="Histogram of log group means", xlab = "log of group means")
lines(hfl, freq=FALSE, border="green")
lines(hul, freq=FALSE, border="red")
legend("topleft", legend=c("down", "flat", "up"), col=c("blue", "green", "red"), lty=1, cex=2)


## ------------------------------------------------------------------------
N <- nrow(compTrainNum)
groupTs <- sqrt(N)*(groupMeansNum - colMeans(groupMeansNum, na.rm=TRUE))/groupSdsNum
groupStats <- cbind(groupMeans, groupSdsNum, groupTs)
hdt <- hist(groupTs$Down, plot=FALSE)
hft <- hist(groupTs$Flat, plot=FALSE)
hut <- hist(groupTs$Up, plot=FALSE)
plot(hdt, border="blue", ylim=c(0, 0.12), xlim=c(-25, 15), freq=FALSE, main="Histogram of t-statistics of group means", xlab="t-statistics of group means")
lines(hft, border="green", freq=FALSE)
lines(hut, border="red", freq=FALSE)
legend("topleft", legend=c("down", "flat", "up"), col=c("blue", "green", "red"), lty=1, cex=2)

sigQsStats.num <- groupStats[ groupTs$Up > 10 | groupTs$Down < -10,]
sigQidx.num <- rownames(sigQsStats.num)


## ------------------------------------------------------------------------
qlevels <- t(sapply(questions, function(q) levels(compTrain[,q])))
qlevels[,1] <- "blank"
rownames(qlevels) <- questions
qlevels.list <- lapply(questions, function(q) {ls <- levels(compTrain[,q]);  ls[1] <- "blank"; ls})
qlevels.list <- lapply(qlevels.list, function(ls) { ll <- 1:3; names(ll) <- ls; ll})
names(qlevels.list) <- questions
computeGroupMeanByLevel <- function(q, comp){
  sapply(levels( comp[ , q]), 
         function(l) {
           s <- sum( comp$Happy[ comp[, q] == l])
           n <- sum(comp[,q]==l)
           if (s==0) 0 else (s/n)
        }
  )
}
computeGroupSdByLevel <- function(q, comp){
  sapply(levels( comp[ , q]), 
         function(l) {
           s <- sum( comp$Happy[ comp[, q] == l])
           if (s==0) 0 else sd( comp$Happy[comp[,q] == l])
        }
  )
}
computeGroupSizesByLevel<- function(q, comp){
  sapply(levels( comp[ , q]), function(l) sum( comp[,q] == l))
}
        
groupMeans <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupMeanByLevel(q, compTrain))))
groupSizes <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupSizesByLevel(q, compTrain))))
groupSds <- data.frame(do.call("rbind", lapply(questions, function(q) computeGroupSdByLevel(q, compTrain))))
rownames(groupMeans) <- questions
rownames(groupSizes) <- questions


## ------------------------------------------------------------------------
makeGroupMeanSDPlot <- function(i, gm, gs, fromLevels=FALSE){
  m <- as.numeric(gm[i,])
  s <- as.numeric(gs[i,])
  ylim <- c(min(m - s) - 0.10, max(m + s) + 0.1)
  xlim <- c(min(m) - 0.05, max(m) + 0.05)
  plot(m, m , col=c("red", "green", "blue"), ylim=ylim, xlim=xlim, cex=2, pch=19, main= qs[i, 1])
  points(m, m - s, col=c("red", "green", "blue"))
  points(m, m + s, col=c("red", "green", "blue"))
  abline(h=(m-s), col=c("red", "green", "blue"), lty=1)
  abline(h=m, col=c("red", "green", "blue"), lty=1, lwd=2)
  abline(h=(m+s), col=c("red", "green", "blue", lty=1))
  if (fromLevels){
    text(x = m,  y = m - 0.01, labels=qlevels[i,], col=c("red", "green", "blue"))
  }
  else text(x=m, y=m-0.01, labels=c("Down", "Flat", "Up"), col=c("red", "green", "blue") )
  print("energies")
  #print(paste(questionEnergyLevels(i, gm)))
  questionEnergyLevels(i, gm)
}
discrimination <- function(i, gm, gs){
  m <- as.numeric(gm[i,])
  s <- as.numeric(gs[i,])
  ord <- order(m)
  m <- m[ord]
  s <- s[ord]
  d <- c( m[2] - s[2] - m[1] - s[1], m[3] - s[3] - m[2] -s[2])
  names(d) <- as.character(qlevels[i, ord][c(1,3)])
  d
}
happyMean <- mean(compTrain$Happy)
questionEnergyLevels <- function(q, gm){
  m <- as.numeric(gm[q,])
  -log(m/(1-m)) + log(happyMean/(1-happyMean))
}
  
  
groupMeanSds <- sqrt(2*groupMeans/groupSizes)


energyFromMean <- function(m, hmean=happyMean) -log(m/(1-m)) + log(hmean/(1-hmean))
computeGroupEnergyByLevel <- function(q, comp){
  sapply(levels( comp[, q]), function(l) meanEnergy( mean(comp[comp[,q]])))
}
groupEnergies <- t(apply(groupMeans, 1, energyFromMean))
qlevel.list <- lapply( qlevels, function(ls) 1:3)
totalEnergy0 <- function(anss){ # anss should bea  named list with questions as names
  n <- length(anss)
  qns <- names(anss)
  qls <- qlevels.list[qns]
  js <- as.numeric( sapply(1:n, function(i) qls[[i]][anss[i]]))
  #sapply(1:n, function(i) groupEnergies[qns[i], js[i]])
  mean(diag(groupEnergies[qns, js]))
}

#i did not realize that invoking as.numeric on factor data will return the factor levels! this makes our life simpler
totalEnergy <- function(anss, ge=groupEnergies, hhe = NULL, hmean = happyMean){ #anss is a vector of factor variables
  qns <- intersect(names(anss), questions)
  levs <- as.numeric(anss[qns])
  if(is.null(hhe))  sum(diag(ge[qns, levs])) - log(hmean/(1-hmean))
  else   sum(diag(ge[qns, levs]))  +  hhe[as.numeric(anss$HouseholdStatus)] - log(hmean/(1-hmean)) 

}
meanEnergy <- function(anss, ge=groupEnergies){ #anss is a vector of factor variables
  totalEnergy(anss, ge=ge)/length(anss)
}
  


## ----numByLevel----------------------------------------------------------
numCodeByLevelsList<- lapply(questions, function(q){
  m <- as.numeric(groupMeans[q,])
  ord <- order(m)
  code0 <- c(-1, 0, 1)
  code <- code0[ord]
  names(code) <- c("blank", levels(compTrain[,q])[c(2,3)])
  code
})
names(numCodeByLevelsList) <- questions
numCodeByLevels <- as.matrix(do.call("rbind", numCodeByLevelsList))
qsansByLevel <- data.frame(qs, numCodeByLevels)
colnames(qsansByLevel) <- c("Q", "L0", "L1", "L2", "L0n", "L1n", "L2n")

numOrd <- t(apply(groupMeans,1,  order))
groupMeansLevNum <- data.frame(t(sapply(1:101, function(i) as.numeric(groupMeans[i, numOrd[i,]]))))
groupSizesLevNum <-  data.frame(t(sapply(1:101, function(i) as.numeric(groupSizes[i, numOrd[i,]]))))
colnames(groupMeansLevNum) <- c("Down", "Flat", "Up")
colnames(groupSizesLevNum) <- c("Down", "Flat", "Up")
rownames(groupMeansLevNum) <- rownames(groupMeans)
rownames(groupSizesLevNum) <- rownames(groupSizes)
groupMeanSdsLevNum <- sqrt(2*groupMeansLevNum/groupSizesLevNum)
groupMeansLevNum.withOrdering <- data.frame(groupMeansLevNum, qsansByLevel[, -1])

makeNum <- function(anss){
  qanss <- names(anss)
  as.numeric(
    sapply(1:length(anss), function(i){ 
       q <- qanss[i]
        a <- as.character(anss[[i]])
        if(a=="") a <- "blank"
        numCodeByLevelsList[[q]][a]
      }
      )
 )
}

compTrainNumQs <- data.frame(t(apply(compTrain[, 9:109], 1, makeNum)))
colnames(compTrainNumQs) <- questions
compTrainNum <- data.frame( compTrain[, 1:8], compTrainNumQs, compTrain[,110])
colnames(compTrainNum) <- colnames(compTrain[, 1:110])
rownames(compTrainNum) <- rownames(compTrain)

compTestNumQs <- data.frame(t(apply(compTest[, 9:109], 1, makeNum)))
colnames(compTestNumQs) <- questions
compTestNum <- data.frame( compTest[, 1:8], compTestNumQs, compTest[,110])
colnames(compTestNum) <- colnames(compTest[, 1:110])
rownames(compTestNum) <- rownames(compTest)

compDataNumQs <- data.frame(t(apply(compTrain[, 9:109], 1, makeNum)))
colnames(compDataNumQs) <- questions
compDataNum <- data.frame( compData[, 1:8], compDataNumQs, compData[,110])
colnames(compDataNum) <- colnames(compData[, 1:110])
rownames(compDataNum) <- rownames(compData)


