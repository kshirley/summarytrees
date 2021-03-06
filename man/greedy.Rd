% Generated by roxygen2 (4.0.1): do not edit by hand
\name{greedy}
\alias{greedy}
\title{Compute summary trees using greedy algorithm}
\usage{
greedy(node = integer(), parent = integer(), weight = numeric(),
  label = character(), K = integer())
}
\arguments{
\item{node}{integer vector containing a set of positive, unique integers
denoting the identities of the nodes of the tree}

\item{parent}{integer vector containing the (positive integer) id of the
parent of each node in the tree. One and only one element of
\code{parent} must be set to zero, indicating which node is the root of
the tree.}

\item{weight}{numeric vector containing non-negative weight of each node
in the tree.}

\item{label}{character vector containing the label for each node in the tree}

\item{K}{integer. The number of nodes in the largest summary tree.}
}
\value{
A list of five objects:
\enumerate{
  \item \code{data} a data frame containing the re-ordered input data,
  as returned by the \code{\link{order.nodes}} function, including the level
  of each node.
  \item \code{tree} a matrix containing the structure of the tree as
  returned by the \code{\link{order.nodes}} function.
  \item \code{summary.trees} a length-K list of matrices, where each matrix
  represents the
  summary tree with K nodes. The kth matrix contains k rows and 5 columns:
  \enumerate{
    \item the first column contains the node ID, which is NA if the node in
    the summary tree is an 'other' cluster
    \item the second column contains the ID of the node's parent
    \item the third column contains the weight of the node in the summary
    tree
    \item the fourth column contains the 'type' of node in the summary tree,
    where 1 indicates a singleton (whose weight in the summary tree is equal
    to its weight in the input tree), 2 indicates a subtree (whose weight in
    the summary tree is equal to the sum of the weights of the nodes in its
    subtree in the input tree), and 3 indicates an 'other cluster'.
    \item the fifth column contains the label, which is the same as the input
    label for summary tree nodes of type = 1 or type = 2, but for summary
    tree nodes of type 3, the label is 'x others' where 'x' indicates how
    many sibling nodes comprise the 'other' cluster.
  }
  \item \code{entropy} a (K x 2) matrix containing the entropy of each
  summary tree
  \item \code{order} the ordering applied to the original rows of data to
  produce the 'data' object returned by this function
}
}
\description{
Compute a series of summary trees for a given
input tree using a greedy algorithm. This algorithm returns summary
trees whose entropies are typically within 5\% of the maximum
possible entropy (based on a series of experiments described by
the paper cited in the references), but there is no performace guarantee
such as that provided by the approximation algorithm. The greedy
algorithm runs substantially faster than the optimal algorithm, which
is also described in the paper in the references.
}
\examples{
data(Gauss)  # load the sample from the Math Genealogy tree

# abbreviate the original full-name labels by first initial + last name:
last <- sapply(strsplit(Gauss[, "label"], " "), function(x) x[length(x)])
first.initial <- substr(Gauss[, "label"], 1, 1)
new.label <- paste(first.initial, last, sep = " ")

x <- greedy(node = Gauss[, "node"],
            parent = Gauss[, "parent"],
            weight = Gauss[, "weight"],
            label = new.label,
            K = 50)

# look at a few pieces of the output:

# The first 10 rows of the re-ordered input data
x$data[1:10, ]

# The first 10 rows of the 'tree' representation of the data:
x$tree[1:10, ]

# The 5-node summary tree
x$summary.trees[[5]]

# The 20-node summary tree:
x$summary.trees[[20]]

# The entropy sequence, a (K x 2) matrix with entropies in the second column
x$entropy

# If you want to reconcile your original copy of the data with the newly
# ordered version, check it:
s <- sample(dim(Gauss)[1], 10)  # randomly select a few rows
Gauss[x$order, ][s, ]
x$data[s, ]
# the node IDs and parent IDs will be different, but the weights and labels
# will match.
}
\references{
\url{http://www.research.att.com/~kshirley/papers/KarloffShirleyWebsite.pdf}
}

