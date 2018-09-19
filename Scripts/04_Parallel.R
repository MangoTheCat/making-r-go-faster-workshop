# Parallel computing example

# PIN Recovery ------------------------------------------------------------

# A common approach to storing passwords is to use a one-way hashing function
# e.g. using the SHA1 algorithm

library(digest)
sha1("mypassword")

hashedPIN <- "600272d88d7961888d606497e8380280c2e95314"  # Which 5-digit PIN creates this hash?

# Serial version to recover PIN:
findPIN <- function(hashedPIN) {
  potentialPINs <- formatC(seq.int(0, 99999),
                           width = 5, format = "d", flag = "0")
  allHashes <- lapply(potentialPINs, FUN = sha1)
  allHashes <- unlist(allHashes)
  names(allHashes) <- potentialPINs
  
  return(names(allHashes[which(allHashes == hashedPIN)]))
}
findPIN(hashedPIN)


# Parallel version to recover PIN:
library(parallel)
nCores <- parallel::detectCores()
cl <- makeCluster(nCores)

parFindPIN <- function(cl, hashedPIN) {
  potentialPINs <- formatC(seq.int(0, 99999), 
                           width = 5, format = "d", flag = "0")
  allHashes <- parLapply(cl = cl, X = potentialPINs, fun = sha1)
  allHashes <- unlist(allHashes)
  names(allHashes) <- potentialPINs
  return(names(allHashes[which( allHashes == hashedPIN)]))
}
parFindPIN(cl, hashedPIN)

library(microbenchmark)
res <- microbenchmark(times = 3, 
               findPIN(hashedPIN), 
               parFindPIN(cl, hashedPIN))
print(res, signif = 2)


clusterExport(cl, varlist = c("hashedPIN", "sha1"))

parFindPIN <- function(cl) {
  potentialPINs <- formatC(seq.int(0, 99999), 
                           width = 5, format = "d", flag = "0")
  allHashes <- parLapply(cl = cl, X = potentialPINs, fun = function(x) sha1(x) == hashedPIN)
  allHashes <- unlist(allHashes)
  names(allHashes) <- potentialPINs
  return(names(allHashes[allHashes]))
}
parFindPIN(cl)

parLapply(cl, 1:10, function(y) y - x)


stopCluster(cl)


# Not included in the materials
library(foreach)
library(doParallel)

cl <- makeCluster(nCores)
registerDoParallel(cl)

foreachFindPIN <- function(hashedPIN){
  potentialPINs <- formatC(seq.int(0, 99999), 
                           width = 5, format = "d", flag = "0")
  allHashes <- foreach(i = seq_along(potentialPINs), .combine = "c", .export = "sha1") %dopar% {
    sha1(potentialPINs[i])
  }
  names(allHashes) <- potentialPINs
  return(names(allHashes[which( allHashes == hashedPIN)]))
}

foreachFindPIN(hashedPIN)

res <- microbenchmark(times = 3, 
                      findPIN(hashedPIN), 
                      parFindPIN(cl, hashedPIN), 
                      foreachFindPIN(hashedPIN)
                      )
