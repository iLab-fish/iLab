# Tests for get_dir

test_that("Errors/warns on incorrect variable selection", {
  
  # Incorrect dir
  expect_error(get_dir("test"))
  # expect_snapshot_failure(get_dir("test"))
  
  # Incorrect sub.folder
  expect_error(get_dir(dir = "iLab_Fish", sub.folder = "test"))
  # expect_snapshot_failure(get_dir(dir = "iLab_Fish", sub.folder = "test"))
  
  # Incorrect root
  expect_error(get_dir(dir = "iLab_Fish", root = "HOME"))
  # expect_snapshot_failure(get_dir(dir = "iLab_Fish", root = "HOME"))
  
  # Unneccessary exclude
  expect_warning(get_dir(dir = "iLab_Fish", exclude = c("test")))
  
  # Multiple matches for dir
  expect_error(get_dir("fish"))
  # expect_snapshot_failure(get_dir("fish"))
})
