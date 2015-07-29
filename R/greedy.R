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
#' @param node integer vector containing a set of positive, unique, integers
#' denoting the identities of the nodes of the tree
#'
#' @param parent integer vector containing the (positive integer) id of the
#' parent of each node in the tree. One  and only one element of
#' \code{parents} must be set to zero, indicating which node is the root of
#' the tree.
#'
#' @param weight numeric vector containing non-negative weight of each node
#' in the tree.
#'
#' @param label character vector containing labels for each node in the tree
#'
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
greedy <- function(node = integer(), parent = integer(), weight = numeric(),
                   label = character(), K = integer()) {

  # convert the node-parent input data to the parent-children data required by
  # the program:
  new <- order.nodes(node = node, parent = parent,
                     weight = weight, label = label)
  tree <- new$tree
  data <- new$data

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
  n <- as.integer(length(weight))

  # Check that length of weights is equal to length of labels:
  if (n != length(label)) {
  	stop("Error: The length of the 'weight' vector must be equal to the length
  	      of the 'label' vector.")
  }

  # check that K <= n:
  if (K > n) {
    stop("Error: K must be less than or equal to n, the number of nodes in the
          input tree.")
  }
  if (K < 1) stop("Error: K must be greater than or equal to 1.")

  # check that the weights are non-negative:
  if (any(weight < 0)) stop("Error: All weights must be non-negative.")

  # collect some variables and force types:
  numparents <- as.integer(dim(tree)[1])
  childindex <- as.integer(tree[, 1])
  childstart <- as.integer(tree[, 2])
  childend <- as.integer(tree[, 3])

  # Run the C function:
  tmp <- capture.output(.C("Rgreedy",
          R_K = as.integer(K),
          R_n = as.integer(n),
          R_numparents = as.integer(numparents),
          R_weight = as.double(data[, "weight"]),
          R_childindex = as.integer(childindex),
          R_childstart = as.integer(childstart),
          R_childend = as.integer(childend),
          PACKAGE = "summarytrees"))

  # gather the output
  output <- tmp[1:(which(tmp == "$R_K") - 1)]

  # parse the output to gather it into a list of length K:
  out <- parse.greedy.stdout(output)$x

  # convert each summary tree into the node-parent-weight-label format:
  final <- vector("list", K)
  for (k in 1:K) {
    # get original labels for these nodes:
    lab.vec <- data[out[[k]][, "node"], "label"]

    # set up the labels for the 'other' clusters in the summary tree:
    sel <- out[[k]][, "type"] == 3  # indicates that this node is an "other" cluster
    if (sum(sel) > 0) {
      parent.vec <- data[out[[k]][sel, "node"], "parent"]
      #out[[k]][sel, "node"] <- parent.vec
      for (i in 1:sum(sel)){
        total.children <- tree[tree[, 1] == parent.vec[i], 3] - tree[tree[, 1] == parent.vec[i], 2] + 1
        not.in.cluster <- sum(data[out[[k]][, "node"], "parent"] == parent.vec[i]) - 1
        num.other.cluster <- total.children - not.in.cluster
        lab.vec[sel][i] <- paste(num.other.cluster, "others", sep=" ")
      }
    }
    # code to reset node numbers in summary tree to 1:n
    #new.parent <- match(data[out[[k]][, "node"], "parent"], data[out[[k]][, "node"], "node"])
    #new.node <- 1:k
    #new.parent[1] <- 0
    #final[[k]] <- data.frame(node = new.node,
    #                         parent = new.parent,
    #                         weight = out[[k]][, "weight"],
    #                         label = lab.vec,
    #                         stringsAsFactors = FALSE)
    final[[k]] <- data.frame(node = out[[k]][, "node"],
                             parent = data[out[[k]][, "node"], "parent"],
                             weight = out[[k]][, "weight"],
                             type = out[[k]][, "type"],
                             label = lab.vec,
                             stringsAsFactors = FALSE)
    final[[k]] <- final[[k]][order(final[[k]][, "type"],
                                   final[[k]][, "node"]), ]
    rownames(final[[k]]) <- 1:k
    final[[k]][final[[k]][, "type"] == 3, 1] <- NA
  }
  return(list(data = data, tree = tree, summary.trees = final))
}



