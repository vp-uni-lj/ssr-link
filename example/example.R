# Load datasets.
# The data should be formatted as shown in data-example.csv

bal <- loadTable('Balkan-genotipi.csv')

fra <- loadTable('FR-genotipi.csv')

bal.ref <- loadTable('Balkan-ref.csv')

fra.ref <- loadTable('FR-ref.csv')


# Reference differences

ref.diff <- bal.ref - fra.ref


# Manually check / fix the reference differences

ref.diff
fix(ref.diff)


# Get normalisation values from reference differences

normValues <- getNormValues(ref.diff)


# Normalise table

bal.norm <- normTable(bal, normValues)


# Join tables

dat <- rbind(fra, bal.norm)


# Pair combinations

genotypePairs <- t(combn(rownames(dat),2))


# Comparing differences pairwise 

genotypeDiffs <- t(apply(genotypePairs, 1, 
  function(x) compareGenotypes(dat,x[1],x[2])))


# genotypeSynonyms <- genotypePairs[which(rowSums(abs(genotypeDiffs)) <= 1),]
genotypeSynonyms <- 
  genotypePairs[which(rowSums(ifelse(abs(genotypeDiffs)>1,1,0)) == 0),]

genotypeSynonyms.1miss <- 
  genotypePairs[which(rowSums(ifelse(abs(genotypeDiffs)>1,1,0)) == 1),]


union(which(genotypeSynonyms[,1] %in% rownames(bal.norm)), which(genotypeSynonyms[,2] %in% rownames(bal.norm)))
intersect(which(genotypeSynonyms[,1] %in% rownames(bal.norm)), which(genotypeSynonyms[,2] %in% rownames(bal.norm)))  

dat[genotypeSynonyms[1,],]


hist(rowSums(ifelse(abs(genotypeDiffs)>1,1,0)))
