# Dataset loading

loadTable <- function(filename) { 
  d <- read.csv(filename, check.names=F)
  d <- as.matrix(d)
  d <- ifelse(d==0,NA,d)
  d <- d[,order(colnames(d))]
  d
}


# Getting normalisation values from differences

getNormValues <- function(diffs) apply(diffs, 2, median)


# Normalising a table

normTable <- function(table, normValues) t(apply(table, 1, function(x) x-normValues))


# Comparing allele pairs

compareAlleles <- function(a,b) {
    if(NA%in%a||NA%in%b) c(0,0) 
    else if(sum(abs(b-a)) <= sum(abs(rev(b)-a)))  b-a  
    else  rev(b)-a
}


# Comparing two genotypes

compareGenotypes <- function(dat,a,b) {
  allelePairs <-  cbind(seq(1,ncol(dat),by=2),seq(2,ncol(dat),by=2))
  apply(allelePairs, 1, function(x) as.vector(compareAlleles(dat[a,x],dat[b,x])))
}
