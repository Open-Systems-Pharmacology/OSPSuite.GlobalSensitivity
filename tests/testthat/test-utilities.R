test_that("%||% returns the left-hand side when it is not NULL", {
  expect_equal("value" %||% "default", "value")
  expect_equal(0 %||% 99, 0)
  expect_equal(FALSE %||% TRUE, FALSE)
})

test_that("%||% falls back to the right-hand side when the left-hand side is NULL", {
  expect_equal(NULL %||% "default", "default")
})

test_that("error stops with the supplied message when the condition is TRUE", {
  expect_error(error(TRUE, "boom"), "boom")
})

test_that("error returns NULL invisibly when the condition is FALSE", {
  expect_null(error(FALSE, "should not stop"))
})

test_that("squareTheCircle converts round parentheses to square brackets", {
  expect_equal(squareTheCircle("f(x)"), "f[x]")
  expect_equal(squareTheCircle("Plasma (Peripheral)"), "Plasma [Peripheral]")
})

test_that("squareTheCircle leaves strings without parentheses unchanged", {
  expect_equal(squareTheCircle("no brackets here"), "no brackets here")
})
