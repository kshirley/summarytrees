#' Interactively visualize summary trees in a browser
#'
#' @description Interactively visualize a set of summary trees in a browser
#'
#' @details This function will place the necessary html/js/css files (located in
#' \code{system.file("vis", package = "summarytrees")}) in a directory specified
#' by \code{out.dir}, start a local file server in that directory (if necessary),
#' and (optionally) open the default browser in this directory.
#'
#' @param json.object character string output from \link{prepare.vis()}.
#' @param out.dir directory to store html/js/json files.
#' @param open.browser Should R open a browser? If yes, this function will
#' attempt to create a local file server via the servr package.
#' This is necessary since the javascript needs to access local files and most
#' browsers will not allow this.
#'
#' @return An invisible object.
#' @export
#'

draw.vis <- function(json.object, out.dir = tempfile(),
                     open.browser = interactive()) {

  ## Copy html/js/css files to out.dir
  dir.create(out.dir)
  src.dir <- system.file("vis", package = "summarytrees")
  to.copy <- Sys.glob(file.path(src.dir, "*"))
  file.copy(to.copy, out.dir, overwrite = TRUE, recursive = TRUE)

  # write the json to a file:
  cat(json.object, file = file.path(out.dir, "data.json"))

  servd <- requireNamespace('servr')
  if (open.browser) {
    if (!servd) {
      message("If the visualization doesn't render, install the servr package\n",
              "and re-run draw.vis(): \n install.packages('servr') \n",
              "Alternatively, you could configure your default browser to allow\n",
              "access to local files as some browsers block this by default")
      browseURL(sprintf("%s/index.html", out.dir))
    } else {
      servr::httd(dir = out.dir)
    }
  }
  return(invisible())
}

