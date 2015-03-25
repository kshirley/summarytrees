#' Compute summary trees using greedy algorithm
#'
#' Compute a series of summary trees for a given
#' input tree using a greedy algorithm. This algorithm returns summary
#' trees whose entropies are typically within 5\% of the maximum 
#' possible entropy (based on a series of experiments described by
#' the paper cited in the references). The
#' algorithm runs substantially faster than the optimal algorithm, which
#' is also described in the paper in the references.
#' 
#' @param tree an integer matrix with three columns and as many rows
#' as there are parent nodes in the tree.
#' The first column contains the indices of the parent nodes in 
#' the tree. The second column contains the index of the first child
#' node of a given parent node, and the third column contains the index
#' of the last child of a given parent node.
#'
#' @param nodes integer vector containing a set of positive, unique, integers
#' denoting the identities of the nodes of the tree
#'
#' @param parents integer vector containing the (positive integer) id of the 
#' parent of each node in the tree. One  and only one element of 
#' \code{parents} must be set to zero, indicating which node is the root of
#' the tree.
#'
#' @param weights numeric vector containing non-negative weight of each node
#' in the tree.
#' @param labels character vector containing labels for each node in the tree
#' @param K integer. The number of nodes in the largest summary tree.
#'
#' @return A length-K list of matrices, where each matrix represents the 
#' summary tree with K nodes. The kth matrix contains k rows and 4 columns.
#' (describe the columns later...)
#'
#' @references \url{http://www2.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf}
#'
#' @useDynLib summarytrees
#'
#' @export
#'
greedy <- function(nodes = integer(), parents = integer(), weights = numeric(), 
                   labels = character(), K = integer()) {

  # convert the node-parent input data to the parent-children data required by
  # the program:
  tree <- get.tree(nodes = nodes, parents = parents, weights = weights, labels = labels)$tree

  # check the input tree:
  if (any(duplicated(tree[, 1]))) {
  	stop("Error: The elements of the first column of 'tree' must be unique.")
  }
  if (any(duplicated(tree[, 2]))) {
  	stop("Error: The elements of the second column of 'tree' must be unique.")
  }
  if (any(duplicated(tree[, 3]))) {
  	stop("Error: The elements of the third column of 'tree' must be unique.")
  }
  if (any(tree[, 2] <= tree[, 1])) {
  	stop("Error: At least one of the children nodes has an equal or lower index 
  	      than its parent")
  }
  if (any(tree[, 3] < tree[, 2])) {
  	stop("Error: At least one of the 'last child' nodes has a smaller index
  	      than the 'first child' nodes. Child nodes of each parent must 
  	      be indexed in increasing order.")
  }
  
  # read in total number of nodes:
  n <- as.integer(length(weights))

  # Check that length of weights is equal to length of labels:
  if (n != length(labels)) {
  	stop("Error: The length of the 'weights' vector must be equal to the length
  	      of the 'labels' vector.")
  }

  # check that K <= n:
  if (K > n) {
    stop("Error: K must be less than or equal to n, the number of nodes in the 
          input tree.")
  }  
  if (K < 1) stop("Error: K must be greater than or equal to 1.")
  
  # check that the weights are non-negative:
  if (any(weights < 0)) stop("Error: All weights must be non-negative.")
  
  # collect some variables and force types:
  numparents <- as.integer(dim(tree)[1])
  childindex <- as.integer(tree[, 1])
  childstart <- as.integer(tree[, 2])
  childend <- as.integer(tree[, 3])

  # Run the C function:
  tmp <- capture.output(.C("Rgreedy", 
          R_K = as.integer(K), 
          R_n = n, 
          R_numparents = numparents, 
          R_weight = weights, 
          R_childindex = childindex, 
          R_childstart = childstart, 
          R_childend = childend, 
          PACKAGE = "summarytrees"))

  output <- tmp[1:(which(tmp == "$R_K") - 1)]
  out <- parse.greedy.stdout(output)$x
  out[[1]] <- matrix(c(1, sum(weights), 2), nrow = 1)
  colnames(out[[1]]) <- c("node", "weight", "cluster")  
  return(out)
}

