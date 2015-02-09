#' Parse the stdout of the greedy algorithm C program 
#'
#' This function parses the text that is printed to standard output
#' after the C program to implement the greedy algorithm is run. 
#' Eventually we'll replace this, but for now it's the simplest
#' way to write an R wrapper around the C function originally
#' written by H. Karloff. 
#' 
#' @param stdout character vector containing, in each element, one line of 
#' standard output from the C implementation of the greedy algorithm.
#'
#' @return A length-K list of matrices, where each matrix represents the 
#' summary tree with K nodes. The kth matrix contains k rows and 4 columns:
#' (1) the node number, (2) the node weight, and (3) the type of node in the
#' summary tree, which is one of 3 types: (a) singleton, (b) subtree, or
#' (c) 'other' node.
#'
#' @references \url{http://www2.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf}
#'
#' @export
#'
parse.greedy.stdout <- function(stdout = character()){
  # stdout is the standard output from the C function that Howard wrote

  # remove blank lines:
  stdout <- stdout[stdout != ""]

  # Find the total number of trees to build
  K <- length(grep("Now printing", stdout))
  x <- as.list(rep(NA, K - 1))
  grep.seq <- grep("Now printing", stdout)
  for (k in 2:K){
    # return a matrix with four columns: node, weight, and category, 
    # where category is 1 = singleton, 2 = subtree, and 3 = cluster:
  	x[[k]] <- matrix(NA, k, 3)
  	colnames(x[[k]]) <- c("node", "weight", "cluster")
    rg <- seq(grep.seq[k] + 1, length = k)
    # first, singletons:
    w.singles <- grep("};", stdout[rg], fixed=TRUE)
    n.singles <- length(w.singles)
    if (n.singles > 0){
      tmp <- unlist(strsplit(stdout[rg][w.singles], "{" , fixed = TRUE))[seq(2, by = 2, length = length(w.singles))]
      singles <- as.numeric(unlist(strsplit(tmp, "}" ,fixed = TRUE))[seq(1, by = 2, length = length(w.singles))])
      x[[k]][1:n.singles, 1] <- singles
      wt <- unlist(strsplit(tmp, "weight=", fixed=TRUE))[seq(2, by = 2, length = n.singles)]
      wt <- as.numeric(substr(wt, 1, nchar(wt) - 1))
      x[[k]][1:n.singles, 2] <- wt
      x[[k]][1:n.singles, 3] <- 1
    } else {singles <- NULL}
    w.trees <- grep("^T_", stdout[rg])
    n.trees <- length(w.trees)
    if (n.trees > 0){
      tmp <- unlist(strsplit(stdout[rg][w.trees], "T_", fixed=TRUE))[seq(2, by = 2, length = length(w.trees))]
      trees <- as.numeric(unlist(strsplit(tmp, ";", fixed=TRUE))[seq(1, by = 2, length = length(w.trees))])
      x[[k]][1:n.trees + n.singles, 1] <- trees
      wt <- unlist(strsplit(tmp, "cluster=", fixed = TRUE))[seq(2, by = 2, length = n.trees)]
      wt <- as.numeric(substr(wt, 1, nchar(wt) - 1))
      x[[k]][1:n.trees + n.singles, 2] <- wt
      x[[k]][1:n.trees + n.singles, 3] <- 2
    } else {trees <- NULL}
    w.clusters <- grep("^Other", stdout[rg])
    n.clusters <- length(w.clusters)
    if (length(w.clusters) > 0){
      tmp <- unlist(strsplit(stdout[rg][w.clusters], "Other="))[seq(2, by = 2, length = length(w.clusters))]
      tmp <- gsub("T_", "", tmp)      
      tmp <- strsplit(tmp, " ")
      clusters <- as.numeric(unlist(tmp)[c(1, cumsum(unlist(lapply(tmp, length))[-n.clusters]) + 1)])
      x[[k]][1:n.clusters + n.trees + n.singles, 1] <- clusters
      x[[k]][1:n.clusters + n.trees + n.singles, 3] <- 3
      tmp <- unlist(strsplit(stdout[rg][w.clusters], "Other="))[seq(2, by = 2, length = length(w.clusters))]
      tmp <- gsub("T_", "", tmp)
      wt <- unlist(strsplit(tmp, "cluster="))[seq(2, by = 2, length = n.clusters)]
      wt <- as.numeric(substr(wt, 1, nchar(wt)-1))
      x[[k]][1:n.clusters + n.trees + n.singles, 2] <- wt      
    } else {clusters <- NULL}
  }
  return(list(x = x))
}
