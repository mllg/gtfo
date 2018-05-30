OS = tolower(Sys.info()[["sysname"]])

get_opts = function(target) {
  bin = getOption(sprintf("gtfo.%s.bin", target))
  if (is.null(bin))
    return(NULL)

  args = if (target == "term")
    getOption(sprintf("gtfo.%s.args", target), linux.terminals[[bin]] %??% "%s")
  else
    getOption(sprintf("gtfo.%s.args", target), "%s")
  return(cmd(bin, args))
}

get_open = function() {
  switch(OS,
    "linux" = cmd("xdg-open", "%s"),
    "darwin" = cmd("open", "%s"),
    "windows" = cmd("start", "%s"),
    NULL
  )
}

`%??%` = function(lhs, rhs) {
  if (is.null(lhs)) rhs else lhs
}

cmd = function(bin, args = character(0L)) {
  structure(list(bin = bin, args = args), class = "cmd")
}

exec = function(cmd, uri) {
  if (is.expression(cmd$bin)) {
    eval(cmd$bin)
  } else {
    command = paste(cmd$bin, sprintf(paste0(cmd$args, collapse = " "), shQuote(uri)))
    if (isTRUE(getOption("gtfo.verbose", TRUE)))
      message("GTFO: ", command)
    if (OS == "windows") {
      shell(command, shell = "powershell.exe", wait = FALSE)
    } else {
      system(command, ignore.stdout = TRUE, ignore.stderr = TRUE, wait = FALSE)
    }
  }
  invisible(TRUE)
}