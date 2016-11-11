context('All current tests')

test_that('We can get keywords', {
  url <- 'mhairihmcneill.com'
  api_key <- '04b3c95092c05f2b850ed0b208a9a8947dbec764'

  df <- get_keywords(url, api_key)
  expect_is(df, "data.frame")
  expect_equal(ncol(df), 3)
  expect_true(nrow(df) > 10)
})

test_that('We can get concepts', {
  url <- 'mhairihmcneill.com'
  api_key <- '04b3c95092c05f2b850ed0b208a9a8947dbec764'

  df <- get_concepts(url, api_key)
  expect_is(df, "data.frame")
  expect_equal(ncol(df), 6)
  expect_true(nrow(df) > 5)
})

test_that('We can get text', {
  url <- 'mhairihmcneill.com'
  api_key <- '04b3c95092c05f2b850ed0b208a9a8947dbec764'

  t <- get_text(url, api_key)
  expect_is(t, "character")
  expect_equal(length(t), 1)
})


test_that('We can get emotions', {
  url <- 'mhairihmcneill.com'
  api_key <- '04b3c95092c05f2b850ed0b208a9a8947dbec764'

  df <- get_emotions(url, api_key)
  expect_is(df, "data.frame")
  expect_equal(ncol(df), 3)
  expect_true(nrow(df) == 5)
})

test_that('We can get sentiment', {
  url <- 'mhairihmcneill.com'
  api_key <- '04b3c95092c05f2b850ed0b208a9a8947dbec764'

  s <- get_sentiment(url, api_key)
  expect_is(s, "numeric")
  expect_equal(length(s), 1)
})

