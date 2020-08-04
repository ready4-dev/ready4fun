context("Set up")
write_ws(
  getwd()
  )
test_that("Set up directories are created",{
  expect_true(dir.exists(paste0(getwd(),"/Readyforwhatsnext")))
  expect_true(dir.exists(paste0(getwd(),"/Readyforwhatsnext/Code")))
  expect_true(dir.exists(paste0(getwd(),"/Readyforwhatsnext/Data")))
  expect_true(dir.exists(paste0(getwd(),"/Readyforwhatsnext/Documentation")))
  expect_true(dir.exists(paste0(getwd(),"/Readyforwhatsnext/Insight")))
}
)

