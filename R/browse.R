#' @title Open a Browser
#'
#' @description
#' Opens a browser to browse the provided \code{url}.
#' The default browser can be set via options:
#' \describe{
#'   \item{\code{gtfo.browse.bin}:}{Binary to call.}
#'   \item{\code{gtfo.browse.args}:}{Arguments for \code{bin}. Use \dQuote{\%s} as placeholder for the url.}
#' }
#'
#' If no option is set, \command{xdg-open} will be called on Linux, \command{open} on Mac OS and \command{start} on Windows.
#' These command open the associated default application.
#'
#' @param url [\code{character(1)}]\cr
#'   URL to open.
#' @export
#' @seealso \code{\link{term}} to start a terminal and \code{\link{fm}} to start a file manager.
#' @references \url{https://github.com/freedesktop/xdg-utils}
#' @examples
#' \dontrun{
#' # Open the R project web site
#' browse("https://www.r-project.org")
#' }
browse = function(url) {
  cmd = get_opts("browse") %??% get_open()
  if (is.null(cmd))
    stop("No suitable browser found. Please set one explicitly via options 'gtfo.browse.bin' and 'gtfo.browse.args'")
  exec(cmd, url)
}
