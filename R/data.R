#' File structure of the 'flare' software package for data visualization
#'
#' A list of the file names and sizes in the flare software package,
#' including the hierarchical structure of the directory.
#'
#' @format A data frame with 252 rows and 4 variables:
#' \describe{
#'   \item{node}{node ID number for each file or directory, numbered from
#'               1 to n = 252 by default}
#'   \item{parent}{the node ID number of the parent of a given node}
#'   \item{weight}{the size of the file in bytes, where zero indicates
#'                 a directory rather than a file}
#'   \item{label}{the name of the file or directory}
#' }
#' @source \url{https://gist.github.com/mbostock/1093025}
"flare"
