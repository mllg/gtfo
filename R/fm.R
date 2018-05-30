#' @title Open a File Manager
#'
#' @description
#' Opens a file manager to browse the provided \code{path}.
#' The default file manager can be set via options:
#' \describe{
#'   \item{\code{gtfo.fm.bin}:}{Binary to call.}
#'   \item{\code{gtfo.fm.args}:}{Arguments for \code{bin}. Use \dQuote{\%s} as placeholder for the path.}
#' }
#'
#' If no option is set, \command{xdg-open} will be called on Linux, \command{open} on Mac OS and \command{start} on Windows.
#' These commands open the associated default application.
#'
#' @param path [\code{character(1)}]\cr
#'   Path. Defaults to the current working directory as reported by \code{\link[base]{getwd}}.
#' @export
#' @seealso \code{\link{term}} to start a terminal and \code{\link{browse}} to start a browser.
#' @references \url{https://github.com/freedesktop/xdg-utils}
#' @examples
#' \dontrun{
#' # Brose R's working directory
#' fm()
#'
#' # Inspect R's tempdir
#' fm(tempdir())
#' }
fm = function(path = getwd()) {
  path = normalizePath(path, mustWork = TRUE)
  cmd = get_opts("fm") %??% get_open()
  if (is.null(cmd))
    stop("No suitable file manager found. Please set one explicitly via options 'gtfo.fm.bin' and 'gtfo.fm.args'")

  exec(cmd, path)
}
