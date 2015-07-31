#' Prepare data to supply the interactive visualization
#'
#' @description Prepare data from summary trees and create one json object
#' to supply necessary information for the browser to create interactive
#' visualization.
#'
#' @details This function takes the output from one of the functions that
#' computes summary trees -- \code{greedy()} or \code{optimal()} -- and
#' processes the output
#' to create the necessary file to feed the interactive visualiation.
#'
#' @param tree.list the list of summary trees that is output by either the
#' \code{greedy()} function or the \code{optimal()} function.
#' @param labels a character vector of labels for all the nodes in the
#' original tree (to be depracated).
#' @param tree a 3-column matrix representing the tree as output by either
#' the \code{greedy()} function or the \code{optimal()} function (to be
#' depracated).
#'
#' @return json.object a JSON object that will be passed to the browser to
#' feed the interactive visualization
#' @export
#'

prepare.vis <- function(tree.list, labels = NULL, tree = NULL,
                        legend.width = 100,
                        node.width = 150,
                        node.height = 12) {

  # Check that the legend.width is not wider than the level.width
  if (node.width < legend.width) {
    stop("node.width must be greater than or equal to legend.width")
  }

  # Set K and gather the edges from tree.list:
  # This bit of code can be improved - for now it just extracts info from
  # tree.list and mutates it to match a previous script (which resulted in
  # the object 'mat' below)
  K <- length(tree.list)
  gather.edges <- function(x) {
    tmp <- x[-1, 2:1]
    tmp[is.na(tmp[, 2]), 2] <- 0
    names(tmp) <- c("parent.vec", "child.vec")
    tmp
  }
  y <- c(NA, lapply(tree.list[-1], gather.edges))
  x <- lapply(tree.list, function(x) x[, "weight"])
  mat <- as.matrix(do.call(rbind, y[-1]), rownames.force = FALSE)

  # Now only keep the unique edges:
  u <- unique(mat)
  u <- u[order(u[, 1], u[, 2]), ]

  # gather a character vector identifying all nodes:
  all.nodes <- u[, 2]
  all.nodes[u[, 2] == 0] <- paste(u[u[, 2] == 0, 1], "other", sep = "")
  all.nodes <- c("1", all.nodes)

  # now compute the children for all these nodes:
  l <- length(all.nodes)
  children <- as.list(rep(NA, l))
  for (i in 1:l){
    if (i %in% grep("other", all.nodes) == FALSE){
      if (as.numeric(all.nodes[i]) %in% u[, 1]){
        children[[i]] <- sort(unique(u[as.numeric(all.nodes[i]) == u[,1], 2]))
        children[[i]][children[[i]] == 0] <- paste0(as.numeric(all.nodes[i]),
                                                    "other")
      }
    }
  }

  # Look at the edges and the associated node labels:
  du <- data.frame(u, node = all.nodes[-1])
  # this excludes the root
  # child.vec = 0 indicates 'other' node here

  # gather the labels, indicators of existence, and weights for each
  # of the K summary trees:
  node.labels <- matrix("", K, l)
  node.exists <- matrix(0, K, l)
  node.weights <- matrix(0, K, l)
  for (k in 2:K) {
    # compute the size:
    o <- match(paste(y[[k]][, 1], y[[k]][, 2], sep = "-"),
               paste(u[, 1], u[, 2], sep = "-"))
    node.weights[k, -1][o] <- x[[k]][-1]
    node.weights[k, 1] <- x[[k]][1]
    # write down whether the node exists in this tree:
    node.exists[k, -1][o] <- 1
    node.exists[k, 1] <- 1
    # compute node labels for this summary tree:
    # this can be greatly simplified by just reading from tree.list!
    label.vec <- c(labels[1], rep("", k - 1))
    label.vec[-1][y[[k]][, 2] != 0] <- labels[y[[k]][y[[k]][, 2] != 0, 2]]
    num.others <- numeric(sum(y[[k]][, 2] == 0))
    for (i in 1:length(num.others)) {
      r <- which(tree[, 1] == y[[k]][y[[k]][, 2] == 0, 1][i])
      num.others[i] <- tree[r, 3] - tree[r, 2] + 1 -
        sum(y[[k]][, 1] == y[[k]][y[[k]][, 2] == 0, 1][i]) + 1
    }
    label.vec[-1][y[[k]][, 2] == 0] <- paste(num.others, "others", sep = " ")
    node.labels[k, -1][o] <- label.vec[-1]
    node.labels[k ,1] <- label.vec[1]
  }

  # recursive function to write the tree object to a json formatted
  # character vector:
  paste.json <- function(name, ch, children, all.nodes){
    cat("{\n")
    cat(paste("\"name\": \"", name, "\", \n", sep = ""))
    cat("\"size\": \"0\"")
    if (!is.na(ch[1])){
      cat(", \n")
      cat("\"children\": [\n")
      for (i in 1:length(ch)){
        paste.json(name = ch[i],
                   ch = unlist(children[match(ch[i], all.nodes)]),
                   children = children,
                   all.nodes = all.nodes)
      }
      cat("]\n")
      cat("},\n")
    } else {
      cat("\n")
      cat("},\n")
    }
  }

  # use the function:
  basetree <- capture.output(paste.json(name = "1",
                                        ch = children[[1]],
                                        children = children,
                                        all.nodes = all.nodes))

  # Correct a couple things that I couldn't do properly using the
  # recursive function (try to fix this later...)
  basetree[which(basetree == "]") - 1] <- "}"
  basetree[length(basetree)] <- "}"
  # Now it's legit json.
  # collapse to one string:
  basetree <- paste(basetree, collapse = " ")

  # Set up stuff for the (trivial) 1-node summary tree:
  node.exists[1, 1] <- 1
  node.weights[1, 1] <- sum(node.weights[2, ])
  node.labels[1, 1] <- labels[1]

  # Create the flat file to send to the browser containing info about
  # node existence, node weights, and node labels for each of the K
  # summary trees:
  out <- rbind(node.exists, node.weights, node.labels)
  colnames(out) <- all.nodes

  ### Now compute, for each k = 1, 2, ..., K, the size range at each level:
  size.range <- as.list(rep(NA, K))
  num.at.level <- as.list(rep(NA, K))
  for (k in 2:K){
    level <- numeric(k - 1)
    for (j in 1:(k - 1)){
      node <- y[[k]][j, 2]
      root <- y[[k]][j, 1]
      current.level <- 1
      while(root != 1){
        current.level <- current.level + 1
        node <- root
        root <- y[[k]][y[[k]][, 2] == node, 1][1]
      }
      level[j] <- current.level
    }
    size.range[[k]] <- matrix(NA, length(unique(level)), 2)
    num.at.level[[k]] <- numeric(length(unique(level)))
    for (j in 1:length(unique(level))){
      size.range[[k]][j, ] <- range(x[[k]][-1][level == sort(unique(level))[j]])
      num.at.level[[k]][j] <- sum(level == sort(unique(level))[j])
    }
  }

  # pre-compute divisor to keep all node widths less than 100 pixels:
  divisor <- numeric(K)
  for (k in 2:K) divisor[k] <- max(size.range[[k]])/100
  divisor[1] <- divisor[2]

  # gather all the data to be sent to the browser in a list:
  all <- list(basetree = basetree,
              divisor = toJSON(divisor),
              out = out,
              legend.width = legend.width,
              node.width = node.width,
              node.height = node.height)

  # convert the list to a json object:
  json.object <- RJSONIO::toJSON(all)
  return(json.object)
}
