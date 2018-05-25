context("term")

test_that("default term is detected", {
  if (tolower(Sys.info()[["sysname"]]) %in% c("solaris", "linux")) {
    if (any(nzchar(Sys.which(names(terminals)))))
      expect_is(find_term(), "cmd")
  } else {
    expect_is(find_term(), "cmd")
  }
})

test_that("options are respected", {
  cmd = withr::with_options(list(gtfo.term.bin = "foo"), get_opts("term"))
  expect_is(cmd, "cmd")
  expect_equal(cmd$bin, "foo")
  expect_equal(cmd$args, "%s")
})
