#' Convert node-parent data to parent-children data
#'
#' Convert two integer vectors containint node identifiers and
#' their respective parent identifiers to a data structure that
#' contains a list of all parents, and for each parent, the first
#' and last child identifier, after re-naming all nodes to be
#' in decreasing order of level, and such that all children of
#' a given parent node are given consecutive integer identifiers.
#' This is a utility function that takes the user-friendly 
#' node-parent data format and converts it to the parent-children
#' format that is currently required by the function that does the 
#' computation of the maximum entropy summary trees.
#'
#' @param nodes integer vector containing a set of positive, unique, integers
#' denoting the identities of the nodes of the tree
#'
#' @param parents integer vector containing the (positive integer) id of the 
#' parent of each node in the tree. One  and only one element of 
#' \code{parents} must be set to zero, indicating which node is the root of
#' the tree.
#'
#' @param weights numeric vector containing the nonnegative weight of
#' each node in the tree. Used in this function only for breaking ties
#' in ordering of children.
#'
#' @param labels character vector containing the labels of
#' each node in the tree. Used in this function only for breaking ties
#' in ordering of children.
#'
#' @return tree an integer matrix with three columns and as many rows
#' as there are parent nodes in the tree.
#' The first column contains the indices of the parent nodes in 
#' the tree. The second column contains the index of the first child
#' node of a given parent node, and the third column contains the index
#' of the last child of a given parent node.
#'
#' @export
#'
get.tree <- function(nodes = integer(), parents = integer(), 
                     weights = numeric(), labels = character()) {

  # Check the inputs:
  if (any(duplicated(nodes))) {
  	stop("Error: Each element of 'nodes' must be unique")
  }
  if (any(nodes < 1)) {
  	stop("Error: All node ID numbers must be integers greater than zero.")
  }
  if (sum(parents == 0) != 1) {
  	stop("Error: Exactly one element of 'parents' must be equal to zero, and
  	     this element must correspond to the root node.")
  }
  n <- length(nodes)
  if (length(parents) != n) {
    stop("Error: The length of 'parents' is not equal to the length of 'nodes'")
  }
  if (length(weights) != n) {
    stop("Error: The length of 'weights' is not equal to the length of 'nodes'")
  }
  if (length(labels) != n) {
    stop("Error: The length of 'labels' is not equal to the length of 'nodes'")
  }
  if (any(parents %in% c(0, nodes) == FALSE)) {
  	stop("Error: Every element of 'parents' must also be an element of 'nodes'")
  }
  if (any(weights < 0)) {
    stop("Error: Weights must be nonnegative")
  }

  # compute the level of each node:
  level <- numeric(n)
  current.level <- 1
  level[which(parents == 0)] <- current.level
  n.zero <- sum(level == 0)
  while (n.zero > 0){
    current.level <- current.level + 1
    level[parents %in% nodes[level == current.level - 1]] <- current.level
    n.zero <- sum(level == 0)
  }
  D <- max(level)

  # create a data.frame to hold the tree information:
  data <- data.frame(nodes, parents, weights, labels, level, stringsAsFactors = FALSE)

  # order by level, then parent, then weight, then label:
  # here we re-order the whole data each time, for each level
  # probably only need to do this for the nodes at a given level
  # come back to improve this (to save time)
  for (i in 1:D) {
    # re-order rows of data
    data <- data[order(data[, "level"], data[, "parents"], data[, "weights"], data[, "labels"]), ]
    # re-number the nodes
    data[, "parents"] <- match(data[, "parents"], data[, "nodes"])
    data[, "nodes"] <- 1:n
    data[1, "parents"] <- 0
  }
  rownames(data) <- 1:n

  # compute children:
  first.child <- numeric(n)
  last.child <- numeric(n)
  for (i in sort(unique(data[, "parents"])[-1])){
    first.child[i] <- min(which(data[, "parents"] == i))
    last.child[i] <- max(which(data[, "parents"] == i))
  }

  # gather into a data.frame called "child.index":
  tree <- cbind(sort(unique(data[, "parents"]))[-1], 
                first.child[first.child != 0], 
                last.child[last.child != 0])
  return(list(tree = tree, data = data))
}
