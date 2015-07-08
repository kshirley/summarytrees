#' Parse the stdout of the optimal algorithm C program
#'
#' This function parses the text that is printed to standard output
#' after the C program to implement the optimal algorithm is run.
#' Eventually we'll replace this, but for now it's the simplest
#' way to write an R wrapper around the C function originally
#' written by H. Karloff.
#'
#' @param stdout character vector containing, in each element, one line of
#' standard output from the C implementation of the optimal algorithm.
#'
#' @return A length-K list of matrices, where each matrix represents the
#' summary tree with K nodes. The kth matrix contains k rows and 3 columns:
#' (1) the node number, (2) the node weight, and (3) the type of node in the
#' summary tree, which is one of 3 types: 1 = singleton, 2 = subtree, or
#' 3 = 'other' node. If node type = 3, the node number is the parent
#' node of this 'other' cluster.
#'
#' @references \url{http://www2.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf}
#'
#' @export
#'
parse.optimal.stdout <- function(stdout = character()){
  # stdout is the standard output from the C function
  # that Howard wrote (final.c)

  # remove blank lines:
  stdout <- stdout[stdout != ""]

  # Find the total number of trees to build:
  K <- length(grep("k=", stdout))
  x <- as.list(rep(NA, K))
  for (k in 1:K){
  	x[[k]] <- matrix(NA, k, 3)
    rg <- seq(grep("k=", stdout)[k] + 1, length = k)
    w.singles <- grep("{", stdout[rg], fixed = TRUE)
    n.singles <- length(w.singles)
    if (n.singles > 0){
      evens <- seq(2, by = 2, length = n.singles)
      odds <- seq(1, by = 2, length = n.singles)
      tmp <- unlist(strsplit(stdout[rg][w.singles], "{", fixed = TRUE))[evens]
      singles <- as.numeric(unlist(strsplit(tmp, "}", fixed = TRUE))[odds])
      single.weights.char <- unlist(strsplit(tmp, "weight ", fixed = TRUE))[evens]
      single.weights <- as.numeric(substr(single.weights.char, 1, nchar(single.weights.char) - 1))
    } else {
      singles <- NULL
      single.weights <- NULL
    }
    w.trees <- grep("_", stdout[rg], fixed = TRUE)
    n.trees <- length(w.trees)
    if (n.trees > 0){
      tmp <- unlist(strsplit(stdout[rg][w.trees], "T_", fixed = TRUE))[seq(2, by = 2, length = n.trees)]
      trees <- as.numeric(unlist(strsplit(tmp, " ", fixed = TRUE))[seq(1, by = 4, length = n.trees)])
      tree.weights <- sapply(strsplit(tmp, " "), function(x) as.numeric(substr(x[[4]], 1, nchar(x[[4]]) - 1)))
    } else {
      trees <- NULL
      tree.weights <- NULL
    }
    w.clusters <- grep("other", stdout[rg], fixed = TRUE)
    n.clusters <-  length(w.clusters)
    if (n.clusters > 0){
      clusters <- as.numeric(unlist(strsplit(stdout[rg][w.clusters], " "))[seq(4, by = 7, length = length(w.clusters))])
      f <- function(x) as.numeric(substr(x[[7]], 1, nchar(x[[7]]) - 1))
      cluster.weights <- sapply(strsplit(stdout[rg][w.clusters], " "), f)
    } else {
      clusters <- NULL
      cluster.weights <- NULL
    }
    x[[k]][, 1] <- c(singles, trees, clusters)
    x[[k]][, 2] <- c(single.weights, tree.weights, cluster.weights)
    x[[k]][, 3] <- rep(1:3, c(n.singles, n.trees, n.clusters))
    colnames(x[[k]]) <- c("node", "weight", "type")
  }
  return(list(x = x))
}
