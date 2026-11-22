# The output of the estimation commands is compared line by line with the
# output of Stata 15.1 for the same commands (reference files in stata/,
# written by Stata in batch mode with `set linesize 80`). The comparison uses
# Stata's factor-variable notation (c. for numeric variables in interactions,
# no notes on the interactions), options(statsalike.stata_notation = TRUE).

stata_dir <- test_path("stata")
cases <- utils::read.csv(file.path(stata_dir, "cases.csv"), stringsAsFactors = FALSE, quote = "'")

stata_data <- function(name) {
  switch(name,
    mtcars = mtcars,
    warpbreaks = warpbreaks,
    infert = {
      d <- infert[c("education", "age", "parity", "induced", "case", "spontaneous", "stratum")]
      d$education <- as.integer(d$education)
      d
    })
}

# several results in one case are printed one after the other; blank lines
# between them are not compared
squash <- function(x) {
  x <- sub("\\s+$", "", x)
  x <- x[!(x == "" & c(FALSE, x[-length(x)] == ""))]
  while (length(x) && x[1] == "") x <- x[-1]
  while (length(x) && x[length(x)] == "") x <- x[-length(x)]
  x
}

for (i in seq_len(nrow(cases))) {
  k <- cases[i, ]
  test_that(paste("identical to Stata 15:", k$id), {
    withr::local_options(width = 80, statsalike.stata_notation = TRUE)
    env <- new.env()
    env$d <- stata_data(k$data)
    if (nzchar(k$setup)) eval(parse(text = k$setup), env)
    res <- suppressWarnings(eval(parse(text = k$r), env))
    objs <- if (inherits(res, "st_output")) list(res) else res
    out <- unlist(lapply(objs, function(o) c(utils::capture.output(print(o)), "")))
    ref <- readLines(file.path(stata_dir, paste0(k$id, ".txt")), warn = FALSE)
    multi <- length(objs) > 1L
    expect_identical(if (multi) squash(out) else squash(out), if (multi) squash(ref) else squash(ref))
  })
}
