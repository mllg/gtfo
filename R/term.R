#' @title Open a Terminal
#'
#' @description
#' Opens a terminal emulator in directory \code{path}.
#'
#' The possibilities to start a terminal are:
#' \describe{
#'   \item{\dQuote{tmux}:}{Create a tmux split if the R session runs inside tmux.}
#'   \item{\dQuote{rstudio}:}{Create a new RStudio terminal if the R session runs inside RStudio.}
#'   \item{\dQuote{opts}:}{If set, use the options \dQuote{gtfo.terminal.bin} and \dQuote{gtfo.terminal.args} to open a new terminal or terminal tab.}
#'   \item{\dQuote{default}:}{Open one of the hard-coded terminals. See note for details.}
#' }
#' The order of the different strategies can be controlled via the option \dQuote{gtfo.term.order} (default: \code{c("tmux", "rstudio", "opts", "default")}).
#'
#' @note
#' The default terminal will determined as follows:
#' \describe{
#'  \item{Mac OS:}{If called from \command{iTerm}, start \command{iTerm}. Start \command{Terminal} otherwise.}
#'  \item{Linux:}{Try to find the one of terminals in \code{$PATH} via \code{\link[base]{Sys.which}} and open the first one found:
#'    \command{kitty}, \command{alacritty}, \command{urxvt},
#'    \command{gnome-terminal}, \command{konsole},
#'    \command{xfce4-terminal}, \command{lxterminal}.}
#'  \item{Windows:}{Start powershell.}
#' }
#'
#' @param path [\code{character(1)}]\cr
#'   Path. Defaults to the current working directory as reported by \code{\link[base]{getwd}}.
#'
#' @export
#' @seealso \code{\link{fm}} to start a file manager and \code{\link{browse}} to start a browser.
#' @examples
#' \dontrun{
#' # Terminal in R's working directory
#' term()
#'
#' # Terminal in R's tempdir
#' term(tempdir())
#' }
term = function(path = getwd()) {
  assert_scalar(url)
  path = normalizePath(path, mustWork = TRUE)
  term.order = getOption("gtfo.term.order", c("tmux", "rstudio", "opts", "default"))
  for (x in term.order) {
    cmd = switch(x,
      "tmux" = find_tmux(),
      "rstudio" = find_rstudio_term(),
      "opts" = get_opts("term"),
      "default" = find_term(),
      stop(sprintf("Illegal element in option 'gtfo.term.order': %s", x), call. = FALSE)
    )
    if (!is.null(cmd))
      return(exec(cmd, path))
  }

  stop("No suitable terminal found. Please set one explicitly via options 'gtfo.terminal.cmd' and 'gtfo.terminal.args'")
}

find_tmux = function() {
  env = Sys.getenv("TMUX")
  if (nzchar(env))
    return(cmd("tmux", c("split-window", "-h", "-c %s")))
  return(NULL)
}

find_rstudio_term = function() {
  if (identical(Sys.getenv("RSTUDIO"), "1") &&
      getOption("gtfo.term.rstudio", TRUE) &&
      requireNamespace("rstudioapi", quietly = TRUE)) {
    return(cmd(expression(rstudioapi::terminalCreate())))
  }
  return(NULL)
}

find_term = function() {
  if (OS == "linux") {
    for (bin in names(linux.terminals)) {
      w = Sys.which(bin)
      if (nzchar(w))
        return(cmd(w, linux.terminals[[bin]]))
    }
  } else if (OS == "darwin") {
    if (identical(Sys.getenv("TERM_PROGRAM"), "iTerm.app"))
      return(cmd("open", args = c("-a iTerm.app",  "%s")))
    return(cmd("open", args = c("-a 'Terminal'", "%s")))
  } else if (OS == "windows") {
    return(cmd("Start-Process", c("powershell.exe", "-WorkingDirectory %s")))
  }

  return(NULL)
}

linux.terminals = list(
  "kitty" = "-d=%s",
  "alacritty" = "--working-directory=%s",
  "urxvt" = "-cd %s",
  "gnome-terminal" = "--working-directory=%s",
  "konsole" = "--workdir=%s",
  "xfce4-terminal" = "--working-directory=%s",
  "lxterminal" = "--working-directory=%s"
)
