context("drugbank")

test_that("get_dbid returns correct results", {
  # test general
  comps <- c('Triclosan', 'Atrazin', 'xxxxxxxx')
  o1 <- get_dbid(comps, match = 'best')
  o2 <- get_dbid(comps, match = 'all')
  o3 <- get_dbid('Atrazin', match = 'first')
  o4 <- get_dbid('Triclosan', match = 'na')

  expect_is(o1, 'data.frame')
  expect_is(o2, 'list')
  expect_is(o3, 'data.frame')
  expect_is(o4, 'data.frame')

  expect_equal(o1$dbid, c('DB08604', 'DB00572', NA))
  expect_equivalent(o2[[2]], c( "DB02146", "DB00814", "DB00755", "DB00572"))
  expect_equal(o3$distance, 'first')
})
