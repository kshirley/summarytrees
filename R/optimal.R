#' Compute maximum entropy summary trees using exact algorithm
#'
#' Compute a series of summary trees for a given
#' input tree using an exact algorithm.
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
#' @param epsilon the nonnegative upper bound of the additive error when
#' using the approximate algorithm. Each k-node summary tree returned by
#' the approximate algorithm will have entropy within \code{epsilon} of the
#' maximal entropy of a k-node summary tree.
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
optimal <- function(node = integer(), parent = integer(), weight = numeric(),
                    label = character(), K = integer(), epsilon = 0) {

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

  # check that the weights are non-negative:
  if (epsilon < 0) stop("Error: epsilon must be greater than or equal to zero.")

  # collect some variables and force types:
  numparents <- as.integer(dim(tree)[1])
  childindex <- as.integer(tree[, 1])
  childstart <- as.integer(tree[, 2])
  childend <- as.integer(tree[, 3])

  #install("~/Git/summarytrees/")

  # Run the C function:
  tmp <- capture.output(.C("Roptimal",
            R_K = as.integer(K),
            R_n = as.integer(n),
            R_numparents = as.integer(numparents),
            R_epsilon = as.double(epsilon),
            R_weight = as.double(data[, "weight"]),
            R_childindex = as.integer(childindex),
            R_childstart = as.integer(childstart),
            R_childend = as.integer(childend),
            PACKAGE = "summarytrees"))

  output <- tmp[1:(which(tmp == "$R_K") - 1)]

  # parse the output to gather it into a list of length K:
  out <- parse.optimal.stdout(output)$x

  # convert each summary tree into the node-parent-weight-label format:
  final <- vector("list", K)
  for (k in 1:K) {
    # get original labels for these nodes:
    lab.vec <- data[out[[k]][, "node"], "label"]
    p.vec <- data[out[[k]][, "node"], "parent"]

    # set up the labels for the 'other' clusters in the summary tree:
    sel <- out[[k]][, "type"] == 3  # indicates that this node is an "other" cluster
    p.vec[sel] <- out[[k]][sel, "node"]

    if (sum(sel) > 0) {
      parent.vec <- out[[k]][sel, "node"]
      for (i in 1:sum(sel)){
        total.children <- tree[tree[, 1] == parent.vec[i], 3] - tree[tree[, 1] == parent.vec[i], 2] + 1
        not.in.cluster <- sum(data[out[[k]][!sel, "node"], "parent"] == parent.vec[i])
        num.other.cluster <- total.children - not.in.cluster
        lab.vec[sel][i] <- paste(num.other.cluster, "others", sep=" ")
      }
    }
    final[[k]] <- data.frame(node = out[[k]][, "node"],
                             parent = p.vec,
                             weight = out[[k]][, "weight"],
                             type = out[[k]][, "type"],
                             label = lab.vec,
                             stringsAsFactors = FALSE)
    final[[k]] <- final[[k]][order(final[[k]][, "type"],
                                   final[[k]][, "node"]), ]
    rownames(final[[k]]) <- 1:k
    final[[k]][final[[k]][, "type"] == 3, 1] <- NA
  }
  return(final)
}
