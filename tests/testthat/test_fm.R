context("fm")

test_that("default fm is detected", {
  skip_on_os("solaris")
  expect_is(find_fm(), "cmd")
})

test_that("options are parsed", {
  cmd = withr::with_options(list(gtfo.fm.bin = "foo"), get_opts("fm"))
  expect_is(cmd, "cmd")
  expect_equal(cmd$bin, "foo")
  expect_equal(cmd$args, "%s")
})
