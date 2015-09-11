#' Compute maximum entropy summary trees using exact or approximate algorithm
#'
#' Compute the series of maximum entropy summary trees for a given
#' input tree using an exact algorithm or approximate algorithm.
#'
#' @param node integer vector containing a set of positive, unique integers
#' denoting the identities of the nodes of the tree
#'
#' @param parent integer vector containing the (positive integer) id of the
#' parent of each node in the tree. One and only one element of
#' \code{parent} must be set to zero, indicating which node is the root of
#' the tree.
#'
#' @param weight numeric vector containing non-negative weight of each node
#' in the tree.
#'
#' @param label character vector containing the label for each node in the tree
#'
#' @param K integer. The number of nodes in the largest summary tree.
#'
#' @param epsilon the nonnegative upper bound of the additive error when
#' using the approximate algorithm. Each k-node summary tree returned by
#' the approximate algorithm will have entropy within \code{epsilon} of the
#' maximal entropy of a k-node summary tree.
#'
#' @return A list of five objects:
#' \enumerate{
#'   \item \code{data} a data frame containing the re-ordered input data,
#'   as returned by the \code{\link{order.nodes}} function, including the level
#'   of each node.
#'   \item \code{tree} a matrix containing the structure of the tree as
#'   returned by the \code{\link{order.nodes}} function.
#'   \item \code{summary.trees} a length-K list of matrices, where each matrix
#'   represents the
#'   summary tree with K nodes. The kth matrix contains k rows and 5 columns:
#'   \enumerate{
#'     \item the first column contains the node ID, which is NA if the node in
#'     the summary tree is an 'other' cluster
#'     \item the second column contains the ID of the node's parent
#'     \item the third column contains the weight of the node in the summary
#'     tree
#'     \item the fourth column contains the 'type' of node in the summary tree,
#'     where 1 indicates a singleton (whose weight in the summary tree is equal
#'     to its weight in the input tree), 2 indicates a subtree (whose weight in
#'     the summary tree is equal to the sum of the weights of the nodes in its
#'     subtree in the input tree), and 3 indicates an 'other cluster'.
#'     \item the fifth column contains the label, which is the same as the input
#'     label for summary tree nodes of type = 1 or type = 2, but for summary
#'     tree nodes of type 3, the label is 'x others' where 'x' indicates how
#'     many sibling nodes comprise the 'other' cluster.
#'   }
#'   \item \code{entropy} a (K x 2) matrix containing the entropy of each
#'   summary tree
#'   \item \code{order} the ordering applied to the original rows of data to
#'   produce the 'data' object returned by this function
#' }
#'
#' @references \url{http://www.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf}
#'
#' @useDynLib summarytrees
#'
#' @export
#'
#' @examples
#' data(Gauss)  # load the sample from the Math Genealogy tree
#'
#' # abbreviate the original full-name labels by first initial + last name:
#' last <- sapply(strsplit(Gauss[, "label"], " "), function(x) x[length(x)])
#' first.initial <- substr(Gauss[, "label"], 1, 1)
#' new.label <- paste(first.initial, last, sep = " ")
#'
#' \dontrun{
#' x <- optimal(node = Gauss[, "node"],
#'              parent = Gauss[, "parent"],
#'              weight = Gauss[, "weight"],
#'              label = new.label,
#'              K = 50,
#'              epsilon = 0)
#'
#' # look at a few pieces of the output:
#'
#' # The first 10 rows of the re-ordered input data
#' x$data[1:10, ]
#'
#' # The first 10 rows of the 'tree' representation of the data:
#' x$tree[1:10, ]
#'
#' # The 5-node summary tree
#' x$summary.trees[[5]]
#'
#' # The 20-node summary tree:
#' x$summary.trees[[20]]
#'
#' # The entropy sequence, a (K x 2) matrix with entropies in the second column
#' x$entropy
#'
#' # If you want to reconcile your original copy of the data with the newly
#' # ordered version, check it:
#' s <- sample(dim(Gauss)[1], 10)  # randomly select a few rows
#' Gauss[x$order, ][s, ]
#' x$data[s, ]
#' # the node IDs and parent IDs will be different, but the weights and labels
#' # will match.
#'
#' # Use the approximate algorithm:
#' x <- optimal(node = Gauss[, "node"],
#'              parent = Gauss[, "parent"],
#'              weight = Gauss[, "weight"],
#'              label = new.label,
#'              K = 50,
#'              epsilon = 0.5)
#' }
#'
#'
optimal <- function(node = integer(), parent = integer(), weight = numeric(),
                    label = character(), K = integer(), epsilon = 0) {

  # convert the node-parent input data to the parent-children data required by
  # the program:
  print("Running order.nodes() function to prepare data")
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

  # check that epsilon is non-negative:
  if (epsilon < 0) stop("Error: epsilon must be greater than or equal to zero.")

  # check that if epsilon=0, the weights are integers:
  if (epsilon == 0 & !all(weight == floor(weight))) {
    stop("Error: When epsilon = 0, all weights must be integers.")
  }

  # collect some variables and force types:
  numparents <- as.integer(dim(tree)[1])
  childindex <- as.integer(tree[, 1])
  childstart <- as.integer(tree[, 2])
  childend <- as.integer(tree[, 3])

  # Run the C function:
  print("Running C function to compute summary trees")
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

  print("Computation finished; now formatting output")

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

  # Compute entropy of each of the K trees:
  norm <- function(x) if (sum(x) == 0) numeric(length(x)) else x/sum(x)
  xlogx <- function(x) ifelse(x == 0, 0, x*log(x, 2))
  ent <- function(x) -sum(xlogx(norm(x)))
  ent.vec <- sapply(final, function(x) ent(x[, "weight"]))
  entropy <- cbind(k = 1:K, entropy = ent.vec)

  return(list(data = data,
              tree = tree,
              summary.trees = final,
              entropy = entropy,
              order = new$order))
}
