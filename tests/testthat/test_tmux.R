context("tmux")

test_that("tmux is detected", {
  withr::with_envvar(list(TMUX = "foo"), {
    expect_is(find_tmux(), "cmd")
  })

  withr::with_envvar(list(TMUX = ""), {
    expect_null(find_tmux())
  })
})
