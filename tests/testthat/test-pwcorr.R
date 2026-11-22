test_that("pwcorr matches cor() and cor.test()", {
  pw <- s.pwcorr(mtcars, mpg, hp, wt, sig = TRUE, obs = TRUE)
  expect_s3_class(pw, "st_pwcorr")
  expect_equal(nrow(pw), 6L)
  expect_equal(pw$var1, c("mpg", "hp", "hp", "wt", "wt", "wt"))
  expect_equal(pw$var2, c("mpg", "mpg", "hp", "mpg", "hp", "wt"))
  r <- pw$r[pw$var1 == "wt" & pw$var2 == "mpg"]
  expect_equal(r, cor(mtcars$wt, mtcars$mpg))
  expect_equal(pw$p_value[pw$var1 == "wt" & pw$var2 == "mpg"],
               cor.test(mtcars$wt, mtcars$mpg)$p.value)
  expect_true(all(is.na(pw$p_value[pw$var1 == pw$var2])))
  expect_equal(pw$obs, rep(32, 6))
  expect_output(print(pw), "1.0000")
})

test_that("pairwise and listwise deletion", {
  df <- data.frame(x = c(1, 2, 3, 4, NA, 6), y = c(2, 1, 4, 3, 5, NA), z = 1:6)
  pw <- s.pwcorr(df, x, y, z)
  expect_equal(pw$obs[pw$var1 == "z" & pw$var2 == "x"], 5)
  expect_equal(pw$r[pw$var1 == "z" & pw$var2 == "x"], cor(df$x, df$z, use = "pairwise"))
  lw <- s.pwcorr(df, x, y, z, listwise = TRUE)
  expect_true(all(lw$obs == 4))
  expect_equal(lw$r[lw$var1 == "z" & lw$var2 == "x"], cor(df[1:4, ])[3, 1])
})

test_that("adjustments, star and print", {
  pw <- s.pwcorr(mtcars, mpg, hp, qsec, sig = TRUE)
  bo <- s.pwcorr(mtcars, mpg, hp, qsec, sig = TRUE, bonferroni = TRUE)
  si <- s.pwcorr(mtcars, mpg, hp, qsec, sig = TRUE, sidak = TRUE)
  off <- pw$var1 != pw$var2
  expect_equal(bo$p_value[off], pmin(1, 3 * pw$p_value[off]))
  expect_equal(si$p_value[off], pmin(1, 1 - (1 - pw$p_value[off])^3))
  expect_error(s.pwcorr(mtcars, mpg, hp, bonferroni = TRUE, sidak = TRUE))
  out <- capture.output(print(s.pwcorr(mtcars, mpg, hp, qsec, star = 0.05)))
  expect_true(any(grepl("-0.7762*", out, fixed = TRUE)))
  pr <- capture.output(print(s.pwcorr(mtcars, mpg, hp, qsec, print = 0.01)))
  expect_false(any(grepl("0.4187", pr, fixed = TRUE)))
  expect_error(s.pwcorr(mtcars, mpg, hp, star = 5))
})

test_that("weights, groups and non-numeric variables", {
  df <- data.frame(x = c(1, 2, 3, 4), y = c(1, 3, 2, 5), w = c(1, 2, 1, 3))
  ex <- df[rep(seq_len(nrow(df)), df$w), ]
  fw <- s.pwcorr(df, x, y, wt = w)
  expect_equal(fw$r[2], cor(ex$x, ex$y))
  expect_equal(fw$obs[2], 7)
  expect_equal(fw$p_value[2], cor.test(ex$x, ex$y)$p.value)
  aw <- s.pwcorr(df, x, y, wt = w, weight_type = "aweight")
  expect_equal(aw$r[2], fw$r[2])
  expect_equal(aw$obs[2], 4)
  g <- mtcars |> dplyr::group_by(am) |> s.pwcorr(mpg, wt)
  expect_equal(names(g)[1], "am")
  expect_equal(nrow(g), 6L)
  expect_output(print(g), "-> am = 0")
  expect_error(s.pwcorr(iris, Sepal.Length, Species), "not numeric")
  expect_equal(unique(s.pwcorr(iris)$var1), names(iris)[1:4])
})

test_that("pwcorr exports", {
  f <- tempfile(fileext = ".html")
  expect_message(s.pwcorr(mtcars, mpg, hp, sig = TRUE, export = f), "Exported")
  html <- readLines(f, warn = FALSE)
  expect_true(any(grepl("pwcorr mpg hp, sig", html, fixed = TRUE)))
})
