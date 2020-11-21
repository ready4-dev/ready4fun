context("Set up")
write_ws(
  getwd()
  )
test_that("Set up directories are created",{
  expect_true(dir.exists(paste0(getwd(),"/ready4")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Code")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Data")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Documentation")))
  expect_true(dir.exists(paste0(getwd(),"/ready4/Insight")))
}
)

