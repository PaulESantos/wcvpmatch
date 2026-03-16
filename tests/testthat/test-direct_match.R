test_that("all direct matches", {
  skip_if_no_default_backbone()
  expect_false(all(wcvp_direct_match(get_testset(mutation = 0))$direct_match))
})

test_that("no direct matches", {
  skip_if_no_default_backbone()
  expect_false(any(wcvp_direct_match(get_testset(mutation = 1))$direct_match))
  expect_false(any(wcvp_direct_match(get_testset(mutation = 2))$direct_match))
})
