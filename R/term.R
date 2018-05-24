#' @title Open a Terminal
#'
#' @description
#' Opens a terminal emulator in directory \code{path}.
#' First tries to create a \command{tmux} split, then to open a new terminal.
#' The default terminal can be set via options:
#' \describe{
#'   \item{\code{gtfo.term.bin}:}{Binary to call.}
#'   \item{\code{gtfo.term.args}:}{Arguments for \code{bin}. Use \dQuote{\%s} as placeholder for the path.}
#' }
#'
#' If no option is set, the terminal defaults to \command{iTerm} on Mac OS and \command{cmd.exe} on Windows.
#' On Linux, the first of the following terminal emulators found will be used:
#' \command{gnome-terminal}, \command{konsole}, \command{xfce4-terminal}, \command{kitty}, \command{alacritty},
#' \command{urxvt}, \command{lxterminal}.
#'
#' @param path [\code{character(1)}]\cr
#'   Path. Defaults to the current working directory as reported by \code{\link[base]{getwd}}.
#' @export
#' @seealso \code{\link{fm}} to start a file manager.
#' @examples
#' \dontrun{
#' # Terminal in R's working directory
#' term()
#'
#' # Terminal in R's tempdir
#' term(tempdir())
#' }
term = function(path = getwd()) {
  path = normalizePath(path, mustWork = TRUE)
  cmd = find_tmux() %??% get_opts("term") %??% find_term()
  if (is.null(cmd))
    stop("No suitable terminal found. Please set one explicitly via options 'gtfo.terminal.cmd' and 'gtfo.terminal.args'")

  exec(cmd, path)
}

find_tmux = function() {
  env = Sys.getenv("TMUX")
  if (nzchar(env))
    return(cmd("tmux", "split-window -h -c %s"))
  return(NULL)
}

find_term = function() {
  OS = tolower(Sys.info()[["sysname"]])

  if (OS == "linux") {
    for (bin in names(linux.terminals)) {
      w = Sys.which(bin)
      if (nzchar(w))
        return(cmd(w, linux.terminals[[bin]]))
    }
  } else if (OS == "darwin") {
    return(cmd("open", args = "-a iTerm.app %s"))
  } else if (OS == "windows") {
    return(cmd("cmd.exe", "%s"))
  }

  return(NULL)
}

linux.terminals = list(
  "gnome-terminal" = "--working-directory=%s",
  "konsole" = "--workdir=%s",
  "xfce4-terminal" = "--working-directory=%s",
  "kitty" = "-d=%s",
  "alacritty" = "--working-directory=%s",
  "urxvt" = "-cd %s",
  "lxterminal" = "--working-directory=%s"
)
