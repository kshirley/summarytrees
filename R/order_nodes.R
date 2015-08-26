#' Order the nodes of the input tree
#'
#' Convert two integer vectors containing node identifiers and
#' their respective parent identifiers to a data structure that
#' contains a list of all parents, and for each parent, the first
#' and last child identifier, after re-ordering all nodes to be
#' in decreasing order of level, and such that all children of
#' a given parent node have consecutive integer identifiers.
#' This is a utility function that takes the user-friendly
#' node-parent data format and converts it to the parent-children
#' format that is currently required by the function that does the
#' computation of the maximum entropy summary trees.
#'
#' @param node integer vector containing a set of positive, unique, integers
#' denoting the identities of the nodes of the tree
#'
#' @param parent integer vector containing the (positive integer) id of the
#' parent of each node in the tree. One  and only one element of
#' \code{parents} must be set to zero, indicating which node is the root of
#' the tree.
#'
#' @param weight numeric vector containing the nonnegative weight of
#' each node in the tree. Used in this function only for breaking ties
#' in ordering of children.
#'
#' @param label character vector containing the labels of
#' each node in the tree. Used in this function only for breaking ties
#' in ordering of children.
#'
#' @return list containing three elements: \code{tree}, an integer matrix
#' with three columns and as many rows
#' as there are parent nodes in the tree.
#' The first column contains the indices of the parent nodes in
#' the tree. The second column contains the indices of the first child
#' node of each parent node, and the third column contains the indices
#' of the last child node of each parent node. Second, \code{data}, a
#' data.frame containing five columns: node, parent, weights, labels,
#' and level, which is equal to 1 for the root of the tree. This
#' data.frame is returned because the ordering of the nodes may be
#' changed by running this function. Third, \code{order}, which is the
#' new ordering of the nodes.
#'
#' @export
#'
order.nodes <- function(node = integer(), parent = integer(),
                        weight = numeric(), label = character()) {

  # Check the inputs:
  n <- length(node)
  if (length(node) < 2) {
    stop("Error: Tree must contain more than one node.")
  }
  if (any(duplicated(node))) {
  	stop("Error: Each element of 'node' must be unique")
  }
  if (any(node < 1)) {
  	stop("Error: All node ID numbers must be integers greater than zero.")
  }
  if (sum(parent == 0) != 1) {
  	stop("Error: Exactly one element of 'parent' must be equal to zero, and
  	     this element must correspond to the root node.")
  }
  if (length(parent) != n) {
    stop("Error: The length of 'parent' is not equal to the length of 'node'")
  }
  if (length(weight) != n) {
    stop("Error: The length of 'weight' is not equal to the length of 'node'")
  }
  if (length(label) != n) {
    stop("Error: The length of 'label' is not equal to the length of 'node'")
  }
  if (any(parent %in% c(0, node) == FALSE)) {
  	stop("Error: Every element of 'parent' must also be an element of 'node'")
  }
  if (any(weight < 0)) {
    stop("Error: Weights must be nonnegative")
  }
  # Note: How can we check that there are no cycles in the graph?

  # compute the level of each node:
  level <- numeric(n)
  current.level <- 1
  level[which(parent == 0)] <- current.level
  n.zero <- sum(level == 0)
  print("Computing node levels")
  while (n.zero > 0) {
    current.level <- current.level + 1
    in.this.level <- parent %in% node[level == current.level - 1]
    level[in.this.level] <- current.level
    n.zero <- sum(level == 0)
  }
  D <- max(level)

  # create a data.frame to hold the tree information:
  data <- data.frame(node, parent, weight, label, level,
                     stringsAsFactors = FALSE)

  # order by level, then parent, then weight, then label:
  o <- do.call(order, data[, c("level", "parent", "weight", "label")])
  data <- data[o, ]
  data[, "parent"] <- match(data[, "parent"], data[, "node"])
  data[, "node"] <- 1:n
  data[1, "parent"] <- 0
  rownames(data) <- 1:n

  #for (i in 1:D) {
    #print(paste0("Sorting level ", i))
    ## re-order rows of data
    #data <- data[order(data[, "level"],
    #                   data[, "parent"],
    #                   data[, "weight"],
    #                   data[, "label"]), ]
    ## re-number the nodes
    #data[, "parent"] <- match(data[, "parent"], data[, "node"])
    #data[, "node"] <- 1:n
    #data[1, "parent"] <- 0
  #}
  #rownames(data) <- 1:n

  # compute children:
  print("Computing child indices for each parent")
  #first.child <- numeric(n)
  #last.child <- numeric(n)
  #a <- aggregate(data[, "node"], by = list(data[, "parent"]), FUN = min)
  #b <- aggregate(data[, "node"], by = list(data[, "parent"]), FUN = max)
  #first.child[a[-1, 1]] <- a[-1, 2]
  #last.child[b[-1, 1]] <- b[-1, 2]
  #for (i in sort(unique(data[, "parent"])[-1])){
  #  first.child[i] <- min(which(data[, "parent"] == i))
  #  last.child[i] <- max(which(data[, "parent"] == i))
  #}
  # gather into a data.frame called "child.index":
  #tree <- cbind(sort(unique(data[, "parent"]))[-1],
  #              first.child[first.child != 0],
  #              last.child[last.child != 0])

  # Faster way to compute vector of child indices for each parent
  d <- diff(data[, "parent"])
  w <- c(which(d != 0), n)
  parent.vector <- unique(data[, "parent"])[-1]
  first.child <- (2:n)[w[-length(w)]]
  last.child <- (2:n)[w[-1] - 1]
  tree <- cbind(parent.vector, first.child, last.child)
  tree <- tree[order(tree[, 1]), , drop = FALSE]
  if (sum(tree[, 3] - tree[, 2] + 1) != n - 1) {
    stop("Error occurred while indexing children of each parent")
  }
  colnames(tree)[1] <- "parent"

  return(list(tree = tree, data = data, order = o))
}
