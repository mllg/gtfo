get_os = function() {
  tolower(Sys.info()[["sysname"]])
}

get_opts = function(target) {
  bin = getOption(sprintf("gtfo.%s.bin", target))
  if (is.null(bin))
    return(NULL)

  args = if (target == "term")
    getOption(sprintf("gtfo.%s.args", target), terminals[[bin]] %??% "%s")
  else
    getOption(sprintf("gtfo.%s.args", target), "%s")
  return(cmd(bin, args))
}

`%??%` = function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

cmd = function(bin, args) {
  structure(list(bin = bin, args = args), class = "cmd")
}

exec = function(cmd, path) {
  command = paste(cmd$bin, sprintf(paste0(cmd$args, collapse = " "), shQuote(path)))
  message("Running command: ", command)
  system(command, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
  invisible(TRUE)
}
