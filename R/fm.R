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
#' If no option is set, \command{xdg-open} or \command{open} is called on Linux or Mac OS, respectively.
#' The windows explorer will be started under windows.
#'
#' @param path [\code{character(1)}]\cr
#'   Path. Defaults to the current working directory as reported by \code{\link[base]{getwd}}.
#' @export
#' @seealso \code{\link{term}} to start a terminal.
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
  cmd = get_opts("fm") %??% find_fm()
  if (is.null(cmd))
    stop("No suitable file manager found. Please set one explicitly via options 'gtfo.fm.bin' and 'gtfo.fm.args'")

  exec(cmd, path)
}

find_fm = function() {
  switch(get_os(),
    "linux" = cmd("xdg-open", "%s"),
    "darwin" = cmd("open", "%s"),
    "windows" = cmd("explorer", "%s"),
    NULL
  )
}
