context("term")

test_that("default term is detected", {
  if (OS %in% c("solaris", "linux")) {
    if (any(nzchar(Sys.which(names(linux.terminals)))))
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
