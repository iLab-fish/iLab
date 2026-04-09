library(testthat)
# library(stringi)

test_that("returns unchanged df when no non-ASCII present", {
  
  df <- data.frame(
    a = c("hello", "world"),
    b = c("123", "456"),
    stringsAsFactors = FALSE
  )
  
  out <- convert_non_ascii(df, interactive = FALSE)
  expect_identical(out, df)
})

test_that("auto-converts field_note column", {
  
  df <- data.frame(
    field_note = c("didn\x92t record", NA),
    other = c("ok", "fine"),
    stringsAsFactors = FALSE
  )
  
  out <- suppressWarnings(
    convert_non_ascii(df, interactive = FALSE)
  )
  
  expect_equal(out$field_note[1], "didn't record")
  expect_false(any(!stri_enc_isascii(out$field_note), na.rm = TRUE))
})

test_that("user abort stops execution", {
  
  df <- data.frame(
    a = c("bad\x92"),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    convert_non_ascii(
      df,
      interactive = TRUE
    ),
    "Process aborted by user",
    fixed = TRUE
  )
})

test_that("non-field_note columns convert when proceeding", {
  
  df <- data.frame(
    x = c("bad\x96dash"),
    field_note = "ok",
    stringsAsFactors = FALSE
  )
  
  out <- suppressWarnings(
    convert_non_ascii(
      df,
      interactive = FALSE
    )
  )
  
  expect_equal(out$x, "bad-dash")
})

# test_that("fails if non-ASCII remains after conversion", {
#   
#   df <- data.frame(
#     a = "\u2603", # snowman, not CP1252-fixable
#     stringsAsFactors = FALSE
#   )
#   
#   expect_error(
#     convert_non_ascii(
#       df,
#       interactive = FALSE,
#       sub = NA_character_
#     ),
#     "Non-ASCII characters remain after conversion"
#   )
# })
