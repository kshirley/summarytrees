#' Interactively visualize a summary tree in a browser
#'
#' @description Interactively visualize a summary tree in a browser
#'
#' @details This function will place the necessary html/js/css files (located in
#' \code{system.file("vis", package = "summarytrees")}) in a directory specified
#' by \code{out.dir}, start a local file server in that directory (if necessary),
#' and (optionally) open the default browser in this directory.
#'
#' @param json character string output from \link{createJSON}.
#' @param out.dir directory to store html/js/json files.
#' @param open.browser Should R open a browser? If yes, this function will
#' attempt to create a local file server via the servr package.
#' This is necessary since the javascript needs to access local files and most
#' browsers will not allow this.
#'
#' @return An invisible object.
#' @export
#'

d3tree <- function(tree.list, out.dir = tempfile(),
                   open.browser = interactive(),
                   labels = NULL, tree = NULL) {

  ## Copy html/js/css files to out.dir
  dir.create(out.dir)
  src.dir <- system.file("vis", package = "summarytrees")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  file.copy(to.copy, out.dir, overwrite = TRUE, recursive = TRUE)

  # for now call tree.list (the length-K list of summary trees) 'g'
  g <- tree.list
  K <- length(tree.list)
  gather.edges <- function(x) {
    tmp <- x[-1, 2:1]
    tmp[is.na(tmp[, 2]), 2] <- 0
    names(tmp) <- c("parent.vec", "child.vec")
    tmp
  }
  y <- c(NA, lapply(g[-1], gather.edges))
  x <- lapply(g, function(x) x[, "weight"])
  mat <- as.matrix(do.call(rbind, y[-1]), rownames.force = FALSE)

  # Now just the unique edges:
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
  # child.vec = 0 indicates other node here

  # gather the labels, indicators of existence, and weights for each
  # of the K summary trees:
  node.labels <- matrix("", K, l)
  node.exists <- matrix(0, K, l)
  node.weights <- matrix(0, K, l)
  for (k in 2:K){
    # compute the size:
    o <- match(paste(y[[k]][, 1], y[[k]][, 2], sep = "-"),
               paste(u[, 1], u[, 2], sep = "-"))
    node.weights[k, -1][o] <- x[[k]][-1]
    node.weights[k, 1] <- x[[k]][1]
    # write down whether the node exists in this tree:
    node.exists[k, -1][o] <- 1
    node.exists[k, 1] <- 1
    # compute node labels for this summary tree:
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
  # Good, this worked.
  # We have 3 dataframes now, and 1 tree object

  # function to write the tree object to a json formatted file:
  json.out <- function(name, ch, file.name, children, all.nodes){
    cat("{\n", file = file.name, append = TRUE)
    cat(paste("\"name\": \"", name, "\", \n", sep = ""),
        file = file.name, append = TRUE)
    cat("\"size\": \"0\"", file = file.name, append = TRUE)
    if (!is.na(ch[1])){
      cat(", \n", file = file.name, append = TRUE)
      cat("\"children\": [\n", file = file.name, append = TRUE)
      for (i in 1:length(ch)){
        json.out(name = ch[i], ch = unlist(children[match(ch[i], all.nodes)]),
                 file.name, children, all.nodes)
      }
      cat("]\n", file = file.name, append = TRUE)
      cat("},\n", file = file.name, append = TRUE)
    } else {
      cat("\n", file = file.name, append = TRUE)
      cat("},\n", file = file.name, append = TRUE)
    }
  }

  # use the function:
  name <- "1"
  ch <- children[[1]]
  file.name <- file.path(out.dir, "basetree.json")

  # create the file:
  cat("", file = file.name)
  json.out("1", ch = children[[1]], file.name = file.name, children = children,
           all.nodes = all.nodes)

  # read in the file and correct it by removing the extra commas:
  tmp <- readLines(file.name)
  tmp[which(tmp == "]") - 1] <- "}"
  tmp[length(tmp)] <- "}"
  writeLines(tmp, con = file.name)

  # Now set up the other main data file:
  node.exists[1, 1] <- 1
  node.weights[1, 1] <- sum(node.weights[2, ])
  node.labels[1, 1] <- labels[1]

  out <- rbind(node.exists, node.weights, node.labels)
  colnames(out) <- all.nodes

  # Look at differences in nodes in consecutive summary trees:
  #diff <- numeric(K - 1)
  #for (k in 2:K) diff[k] <- sum(node.exists[k, ] == node.exists[k - 1, ])

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
        root <- y[[k]][y[[k]][, 2]==node, 1][1]
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
  divisor <- round(divisor)
  divisor[1] <- divisor[2]

  new.out <- cbind(out, divisor = c(divisor, rep(0, 2*K)))
  write.csv(new.out, file = file.path(out.dir, "info.csv"),
            row.names = FALSE, quote = FALSE)

  #max.flare <- 1000
  #max.gauss <- 50
  #div.cat <- pmax(divisor, max.flare)
  #for (i in 1:K) cat(div.cat[i], sep = ",")
  div.cat <- divisor
  cat(toJSON(div.cat), "\n", file = file.path(out.dir, "divisor.json"))
  #cat(toJSON(div.cat), file = "~/Git/summarytrees/inst/vis/divisor.json")

  servd <- requireNamespace('servr')
  if (open.browser) {
    if (!servd) {
      message("If the visualization doesn't render, install the servr package\n",
               "and re-run d3tree: \n install.packages('servr') \n",
              "Alternatively, you could configure your default browser to allow\n",
              "access to local files as some browsers block this by default")
      browseURL(sprintf("%s/index.html", out.dir))
    } else {
      servr::httd(dir = out.dir)
    }
  }
  return(invisible())
}
