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
#' summary tree with K nodes. The kth matrix contains k rows and 3 columns:
#' (1) the node number, (2) the node weight, and (3) the type of node in the
#' summary tree, which is one of 3 types: 1 = singleton, 2 = subtree, or
#' 3 = 'other' node.
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
  grep.seq <- grep("Now printing", stdout)
  K <- length(grep.seq)
  x <- vector("list", K)

  # Loop through the summary trees from k = 2, ..., K:
  for (k in 2:K){
    # return a matrix with three columns: node, weight, and type,
    # where type is 1 = singleton, 2 = subtree, and 3 = cluster.
  	x[[k]] <- matrix(NA, k, 3)
  	colnames(x[[k]]) <- c("node", "weight", "type")

    # gather the lines of standard out for the k-node summary tree:
    rg <- seq(grep.seq[k] + 1, length = k)

    # first, singletons (type = 1):
    w.singles <- grep("};", stdout[rg], fixed=TRUE)
    n.singles <- length(w.singles)
    if (n.singles > 0){
      evens <- seq(2, by = 2, length = n.singles)
      odds <- seq(1, by = 2, length = n.singles)
      tmp <- unlist(strsplit(stdout[rg][w.singles], "{" , fixed = TRUE))[evens]
      singles <- as.numeric(unlist(strsplit(tmp, "}" ,fixed = TRUE))[odds])
      x[[k]][1:n.singles, 1] <- singles
      wt <- unlist(strsplit(tmp, "weight=", fixed=TRUE))[evens]
      wt <- as.numeric(substr(wt, 1, nchar(wt) - 1))
      x[[k]][1:n.singles, 2] <- wt
      x[[k]][1:n.singles, 3] <- 1
    } else {
      singles <- NULL
    }

    # Next, subtrees (type = 2):
    w.trees <- grep("^T_", stdout[rg])
    n.trees <- length(w.trees)
    if (n.trees > 0){
      evens <- seq(2, by = 2, length = n.trees)
      odds <- seq(1, by = 2, length = n.trees)
      tmp <- unlist(strsplit(stdout[rg][w.trees], "T_", fixed=TRUE))[evens]
      trees <- as.numeric(unlist(strsplit(tmp, ";", fixed=TRUE))[odds])
      x[[k]][1:n.trees + n.singles, 1] <- trees
      wt <- unlist(strsplit(tmp, "cluster=", fixed = TRUE))[evens]
      wt <- as.numeric(substr(wt, 1, nchar(wt) - 1))
      x[[k]][1:n.trees + n.singles, 2] <- wt
      x[[k]][1:n.trees + n.singles, 3] <- 2
    } else {
      trees <- NULL
    }

    # third, 'other' clusters (type = 3):
    w.clusters <- grep("^Other", stdout[rg])
    n.clusters <- length(w.clusters)
    if (n.clusters > 0){
      evens <- seq(2, by = 2, length = n.clusters)
      odds <- seq(1, by = 2, length = n.clusters)
      tmp <- unlist(strsplit(stdout[rg][w.clusters], "Other="))[evens]
      tmp <- gsub("T_", "", tmp)
      tmp <- strsplit(tmp, " ")
      clusters <- as.numeric(unlist(tmp)[c(1, cumsum(unlist(lapply(tmp, length))[-n.clusters]) + 1)])
      x[[k]][1:n.clusters + n.trees + n.singles, 1] <- clusters
      x[[k]][1:n.clusters + n.trees + n.singles, 3] <- 3
      tmp <- unlist(strsplit(stdout[rg][w.clusters], "Other="))[evens]
      tmp <- gsub("T_", "", tmp)
      wt <- unlist(strsplit(tmp, "cluster="))[evens]
      wt <- as.numeric(substr(wt, 1, nchar(wt)-1))
      x[[k]][1:n.clusters + n.trees + n.singles, 2] <- wt
    } else {
      clusters <- NULL
    }
  }

  # manually create the 1-node summary tree:
  x[[1]] <- matrix(c(1, sum(x[[2]][, "weight"]), 2), nrow = 1)
  colnames(x[[1]]) <- c("node", "weight", "type")

  # return the length-K list:
  return(list(x = x))
}
