get_opts = function(target) {
  bin = getOption(sprintf("gtfo.%s.bin", target))
  if (is.null(bin))
    return(NULL)
  cmd(bin, getOption(sprintf("gtfo.%s.args", target), "%s"))
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
  system(command,
    ignore.stdout = FALSE,
    ignore.stderr = FALSE,
    wait = FALSE
  )
  invisible(TRUE)
}
