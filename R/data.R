#' File structure of the R software package
#'
#' A list of the file names and sizes in the R software package (version 3.2.1,
#' aka 'World Famous Astronaut', released 2015-06-18), including the
#' hierarchical structure of the directory.
#'
#' @format A data frame with 4704 rows and 4 variables:
#' \describe{
#'   \item{node}{node ID number for each file or directory, numbered from
#'               1 to n = 4704 by default}
#'   \item{parent}{the node ID number of the parent of each node}
#'   \item{weight}{the size of the file in bytes, where zero indicates
#'                 a directory rather than a file}
#'   \item{label}{the name of the file or directory}
#' }
#' @source \url{https://cran.r-project.org/}
"Rsource"

#' Carl Gauss subtree of the Math Genealogy Tree
#'
#' A list of student-advisor relationships of mathematicians who descended from
#' Carl Gauss.
#'
#' @format A data frame with 43527 rows and 4 variables:
#' \describe{
#'   \item{node}{node ID number for each mathematician, numbered from
#'               1 to n = 43527 by default (note that these are not the same ID
#'               numbers used by the Mathematics Genealogy Project)}
#'   \item{parent}{the node ID number of the parent of a given
#'                 mathematician}
#'   \item{weight}{set to 1 by default for each mathematician}
#'   \item{label}{the full name of the mathematician}
#' }
#' @source This data is distributed with the permission of and in cooperation
#' with the Mathematics Genealogy Project (MGP):
#' \url{http://genealogy.math.ndsu.nodak.edu/}.
#'
#' Also note that:
#'
#' 1. This data may not be redistributed separately from the R package.
#'
#' 2. This is a snapshot of MGP data from June 7, 2012.
#'
#' 3. The node identifiers (1, 2, ..., 43537) here are not the same
#' identifiers used by the MGP (we mention this for clarification purposes).
"Gauss"

#' File structure of the 'flare' software package
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

#' URL counts within the "Top/Sports" branch of the DMOZ directory.
#'
#' A list of the counts of URLs that belong in each topic of the "Top/Sports"
#' subtree of the DMOZ directory.
#'
#' @format A data frame with 15018 rows and 4 variables:
#' \describe{
#'   \item{node}{node ID number for each topic, numbered from
#'               1 to n = 15018 by default}
#'   \item{parent}{the node ID number of the parent of each node}
#'   \item{weight}{the number of URLs assigned to each topic in the hierarchy}
#'   \item{label}{the name of the topic in the hierarchy}
#' }
#' @source \url{http://www.dmoz.org/rdf.html}
"dmoz"


