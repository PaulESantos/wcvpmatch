test_that("all direct matches", {
  skip_if_no_default_backbone()
  expect_true(all(wcvp_genus_match(get_testset(mutation = 0))$genus_match))
  expect_true(all(wcvp_genus_match(get_testset(mutation = 1))$genus_match))
})

test_that("no direct matches", {
  skip_if_no_default_backbone()
  expect_false(any(wcvp_genus_match(get_testset(mutation = 2))$genus_match))
  expect_false(any(wcvp_genus_match(get_testset(mutation = 3))$genus_match))
})

