context("parseRange")

test_that("parseRange works on edge cases", {
  expect_identical(parseRange(""), character())
  expect_identical(parseRange(character()), character())
})

test_that("parseRange works on single cases", {
    expect_identical(parseRange("1"), "1")
    expect_identical(parseRange("1, 2"), c("1", "2"))
    expect_identical(parseRange("1,2"), c("1", "2"))
    expect_identical(parseRange("1-3"), c("1", "2", "3"))
})

test_that("parseRange works on mixed ranges", {
    expect_identical(parseRange("1-3, 3"), c("1", "2", "3", "3"))
    expect_identical(parseRange("1-3,5"), c("1", "2", "3", "5"))
    expect_identical(parseRange("1-3,5, 0"), c("1", "2", "3", "5", "0"))
})

test_that("parseRange works with different separators", {
    expect_identical(parseRange("1; 2", sep = "; *"), c("1", "2"))
    expect_identical(parseRange("2*3", rsep = "\\* *"), c("2", "3"))
    expect_identical(parseRange("2*3;0", sep = "; *", rsep = "\\* *"), 
                     c("2", "3", "0"))
})
