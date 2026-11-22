#' ---
#' title: "statsalike: examples for all functions"
#' subtitle: "Stata's describe, codebook, summarize, tabulate, table, pwcorr, regress, logit and etable in R"
#' author: "Stefan Kirchner"
#' output:
#'   html_document:
#'     toc: true
#' ---
#'
#' This report shows every function of the **statsalike** package with its
#' options. Each example names the Stata command whose output the
#' statsalike command reproduces.
#'
#' To rebuild the report, open this file in RStudio and use
#' *File > Compile Report* (Ctrl+Shift+K), or run
#' `rmarkdown::render("statsalike_examples.R")`.
#'
#' | Stata | statsalike |
#' |---|---|
#' | `describe` | `s.describe()` |
#' | `codebook` | `s.codebook()` |
#' | `summarize` / `sum` | `s.summarize()` / `s.sum()` |
#' | `tabulate` / `tab` | `s.tabulate()` / `s.tab()` |
#' | `tab1`, `tab2` | `s.tab1()`, `s.tab2()` |
#' | `tabulate x, generate()` | `s.tabgen()` |
#' | `table` (Stata 17 syntax) | `s.table()` |
#' | `pwcorr` | `s.pwcorr()` |
#' | `regress` | `s.regress()` |
#' | `logit` | `s.logit()` |
#' | `return list` after `tabulate` | `s.tests()` |
#' | `ereturn list` after `regress`/`logit` | `s.ereturn()` |
#' | `etable` (Stata 17 and later) | `s.etable()`, also for `lm()`, `glm()` and other R models |
#' | `collect export` / `putdocx` | `export =` option, `s.export()` |
#'
#' **Missing values in tables.** Unlike Stata, the table functions
#' (`s.tab()`, `s.tab1()`, `s.tab2()`, `s.table()`) show missing values as
#' a category `<NA>` by default, so that you always see how many
#' observations are missing. `s.tab(df, x)` therefore corresponds to Stata's
#' `tabulate x, missing`; `missing = FALSE` gives Stata's default. The
#' examples below use `missing = FALSE` wherever percentages, test
#' statistics or summary statistics should refer to valid observations only.
#'
#+ knitr-options, include = FALSE
knitr::opts_chunk$set(comment = "", collapse = FALSE)

#' <style>
#' pre, pre code { white-space: pre !important; overflow-x: auto; word-wrap: normal; word-break: normal; }
#' </style>

#' # Setup
#'
#' All examples use the ISSP Work Orientations cumulation 1989-2015 (GESIS,
#' ZA8797) from the seminar folder: four survey waves (1989, 1997, 2005,
#' 2015) from 32 countries with value labels, missing values and a weighting
#' factor. We read only the variables we need. `issp_all` holds all waves
#' and countries; most examples use `issp`, the 2015 wave for Germany, Great
#' Britain, Sweden and the United States.

#+ setup, message = FALSE, warning = FALSE
library(dplyr)
library(statsalike)

issp_file <- "../Kapitel Regression (s.)/ZA8797_v1_0_0.sav"
issp_vars <- c("year", "country", "SEX", "AGE", "MARITAL", "DEGREE", "WORK",
               "TYPORG2", "UNION", "WRKHRS", "v12", "v34", "v40", "TOPBOT",
               "WEIGHT")
issp_all <- haven::read_sav(issp_file, col_select = all_of(issp_vars)) |>
  # the value labels of year are the long study titles; keep the plain year
  mutate(year = haven::zap_labels(year))

# 2015 wave: DE-Germany, GB-Great Britain, SE-Sweden, US-United States
issp <- issp_all |>
  filter(year == 2015, country %in% c(276, 826, 752, 840)) |>
  select(-year)

dir.create("export", showWarnings = FALSE)

#' All output adapts to the width of the console (`getOption("width")`,
#' 80 characters by default). Wide tables are split into blocks and long
#' labels are shortened with `~`, as in Stata; a wider console, e.g.
#' `options(width = 120)`, shows more at once.
#'
#' The ISSP variables used below:
#'
#' * `year` (survey wave), `country`, `SEX`, `MARITAL` (marital status),
#'   `DEGREE` (highest education level), `WORK` (in paid work),
#'   `TYPORG2` (public or private employer), `UNION` (trade union
#'   membership): categorical, with value labels
#' * `AGE`, `WRKHRS` (hours worked weekly), `v40` (job satisfaction, 1 =
#'   completely satisfied to 7 = completely dissatisfied), `v12` (high income
#'   is important, 1 = strongly agree to 5 = strongly disagree), `v34` (work
#'   is stressful, 1 = always to 5 = never), `TOPBOT` (top-bottom
#'   self-placement, 1-10): numeric
#' * `WEIGHT`: weighting factor
#'
#' # `s.describe()`: describe
#'
#' `describe` lists all variables with storage type, display format, value
#' label and variable label.

#' Stata: `describe country SEX AGE DEGREE WRKHRS WEIGHT`
s.describe(issp, country, SEX, AGE, DEGREE, WRKHRS, WEIGHT)

#' Variables are selected with tidyselect, so ranges and helpers work too.
#' Stata: `describe SEX-DEGREE, numbers` and `describe, simple`
s.describe(issp, SEX:DEGREE, numbers = TRUE)
s.describe(issp, simple = TRUE)

#' Stata: `describe, short`
s.describe(issp_all, short = TRUE)

#' Long variable names are abbreviated like in Stata unless `fullnames = TRUE`.
issp |>
  rename(job_satisfaction_main_job = v40, hours_worked_weekly = WRKHRS) |>
  s.describe(job_satisfaction_main_job, hours_worked_weekly, fullnames = TRUE)

#' Like `describe, replace`, the result is a data set you can work with,
#' e.g. to search the variable labels (Stata: `lookfor work`):
s.describe(issp) |>
  as_tibble() |>
  filter(grepl("work", varlab, ignore.case = TRUE)) |>
  select(name, vallab, varlab)

#' # `s.codebook()`: codebook
#'
#' `codebook` describes the content of each variable: type, value labels,
#' range, units, unique and missing values, and either a tabulation (up to 9
#' values), summary statistics or examples.

#' Stata: `codebook SEX AGE DEGREE`
s.codebook(issp, SEX, AGE, DEGREE)

#' Stata: `codebook, compact`
s.codebook(issp, SEX, AGE, DEGREE, WRKHRS, v40, WEIGHT, compact = TRUE)

#' Stata: `codebook, problems`: constant or incompletely labelled variables
#' and text variables with blanks.
s.codebook(issp, problems = TRUE)

#' # `s.summarize()`: summary statistics
#'
#' Stata: `summarize`
s.summarize(issp)

#' Stata: `summarize AGE WRKHRS v40 [aweight = WEIGHT]`: with analytic
#' weights, an additional column shows the sum of the weights.
s.summarize(issp, AGE, WRKHRS, v40, wt = WEIGHT, weight_type = "aweight")

#' Stata: `summarize WRKHRS, detail`
s.sum(issp, WRKHRS, detail = TRUE)

#' Stata: `by country: summarize AGE WRKHRS`
issp |> group_by(country) |> s.sum(AGE, WRKHRS)

#' # `s.tab()`: one-way tables
#'
#' Stata: `tabulate DEGREE, missing`: the 23 respondents without an answer
#' appear as `<NA>`, displayed as in R; the result contains a genuine `NA`.
s.tab(issp, DEGREE)

#' Stata: `tabulate DEGREE` (missing values excluded, Stata's default)
s.tab(issp, DEGREE, missing = FALSE)

#' Stata: `tabulate year`, all waves of the cumulation
s.tab(issp_all, year)

#' Options are logical arguments with Stata's names.
#' Stata: `tabulate DEGREE, nolabel`
s.tab(issp, DEGREE, missing = FALSE, nolabel = TRUE)

#' SPSS user-defined missing values (in the ISSP the codes -9 to -1) become
#' `NA` when the file is read. Read with `user_na = TRUE`, they are kept and
#' shown with their value labels; `missing = FALSE` excludes them, as Stata
#' does:
issp_na <- haven::read_sav(issp_file, user_na = TRUE,
                           col_select = c(year, country, UNION)) |>
  filter(year == 2015, country %in% c(276, 826, 752, 840))
s.tab(issp_na, UNION)
s.tab(issp_na, UNION, missing = FALSE)

#' Stata: `tabulate DEGREE, sort plot`
s.tab(issp, DEGREE, missing = FALSE, sort = TRUE, plot = TRUE)

#' Stata: `tabulate DEGREE, nofreq`
s.tab(issp, DEGREE, missing = FALSE, nofreq = TRUE)

#' Stata: `tabulate DEGREE, subpop(female)`: only observations where the
#' expression is true are counted.
s.tab(issp, DEGREE, missing = FALSE, subpop = SEX == 2)

#' `if` and `in` become ordinary dplyr verbs.
#' Stata: `tabulate DEGREE if AGE < 30`
issp |> filter(AGE < 30) |> s.tab(DEGREE, missing = FALSE)

#' The `by` prefix becomes `group_by()`.
#' Stata: `by country: tabulate UNION, missing`
issp |> group_by(country) |> s.tab(UNION)

#' ## Weights
#'
#' Stata: `tabulate country [aweight = WEIGHT]`: analytic weights are
#' rescaled to the number of observations.
s.tab(issp, country, wt = WEIGHT, weight_type = "aweight")

#' Stata: `tabulate country [iweight = WEIGHT]`
s.tab(issp, country, wt = WEIGHT, weight_type = "iweight")

#' Frequency weights must be whole numbers, e.g. the counts of aggregated
#' data. Stata: `contract country DEGREE` followed by
#' `tabulate DEGREE [fweight = _freq]`
degree_counts <- issp |> count(country, DEGREE)
degree_counts
s.tab(degree_counts, DEGREE, wt = n, missing = FALSE)

#' ## Addon: entropy
#'
#' *This option does not exist in Stata.* `entropy = TRUE` adds the
#' Shannon entropy *H* = −Σ *p* · ln *p* of a one-way distribution and the
#' relative entropy *H* / ln *k* (*k* = number of categories). The
#' relative entropy is 0 if all observations fall into one category and 1 if
#' all categories are equally frequent; it does not depend on the base of
#' the logarithm. With `missing = FALSE` the entropy refers to the valid
#' answers; by default `<NA>` would count as a further category.
s.tab(issp, DEGREE, missing = FALSE, entropy = TRUE)

#' It works with weights, `group_by()` and `s.tab1()`; `s.tests()` returns
#' the values as data, e.g. to compare the diversity of education levels
#' across countries:
issp |>
  group_by(country) |>
  s.tab(DEGREE, wt = WEIGHT, weight_type = "aweight", missing = FALSE, entropy = TRUE) |>
  s.tests()

#' # `s.tab()`: two-way tables
#'
#' By default the missing education levels form a row `<NA>`.
#' Stata: `tabulate DEGREE SEX, missing`
s.tab(issp, DEGREE, SEX)

#' Stata: `tabulate DEGREE SEX, row column`
s.tab(issp, DEGREE, SEX, missing = FALSE, row = TRUE, column = TRUE)

#' Stata: `tabulate country SEX, cell expected cchi2 clrchi2`. The key
#' above the table names the values in each cell; `nokey = TRUE` omits it.
s.tab(issp, country, SEX, cell = TRUE, expected = TRUE, cchi2 = TRUE,
      clrchi2 = TRUE)

#' Test statistics. Stata: `tabulate DEGREE SEX, chi2 lrchi2 V gamma taub`
#' (the same as `all`)
s.tab(issp, DEGREE, SEX, missing = FALSE, all = TRUE)

#' Stata: `tabulate SEX TYPORG2, exact`: for 2x2 tables Stata also reports
#' the one-sided test.
s.tab(issp, SEX, TYPORG2, missing = FALSE, nofreq = TRUE, exact = TRUE, chi2 = TRUE)

#' Weighted cross tables. Stata:
#' `tabulate country TYPORG2 [aweight = WEIGHT], row`
s.tab(issp, country, TYPORG2, wt = WEIGHT, weight_type = "aweight",
      missing = FALSE, row = TRUE)

#' Grouped. Stata: `by country: tabulate SEX TYPORG2, row chi2`
issp |> group_by(country) |> s.tab(SEX, TYPORG2, missing = FALSE, row = TRUE, chi2 = TRUE)

#' ## `s.tests()`: test statistics as data
#'
#' Like `return list` after `tabulate`, `s.tests()` returns the test
#' statistics, ready for further use.
tab <- s.tab(issp, DEGREE, SEX, missing = FALSE, chi2 = TRUE, V = TRUE, gamma = TRUE)
s.tests(tab)

#' # `s.tab()` with `summarize()`
#'
#' Stata: `tabulate DEGREE, summarize(WRKHRS)`
s.tab(issp, DEGREE, summarize = WRKHRS, missing = FALSE)

#' Stata: `tabulate DEGREE [aweight = WEIGHT], summarize(WRKHRS) nostandard obs`
s.tab(issp, DEGREE, summarize = WRKHRS, wt = WEIGHT, weight_type = "aweight",
      missing = FALSE, nostandard = TRUE, obs = TRUE)

#' Two-way. Stata: `tabulate country SEX, summarize(WRKHRS) nofreq`
s.tab(issp, country, SEX, summarize = WRKHRS, nofreq = TRUE)

#' # `s.tab1()` and `s.tab2()`: several tables at once
#'
#' Stata: `tab1 SEX TYPORG2 UNION, missing`
s.tab1(issp, SEX, TYPORG2, UNION)

#' Stata: `tab2 UNION SEX TYPORG2, firstonly chi2`
s.tab2(issp, UNION, SEX, TYPORG2, missing = FALSE, firstonly = TRUE, chi2 = TRUE)

#' # `s.tabgen()`: indicator variables
#'
#' Stata: `tabulate UNION, generate(union_)`. One 0/1 variable per
#' category, labelled `UNION==value`; the grouping of the data is kept.
issp |>
  s.tabgen(UNION, prefix = "union_") |>
  select(UNION, starts_with("union_")) |>
  head(5)

#' # `s.table()`: tables of frequencies and statistics
#'
#' `s.table()` follows the syntax of Stata 17:
#' `table (rows) (columns) (tables), statistic(...)`.
#'
#' Stata: `table DEGREE, missing`
s.table(issp, DEGREE)

#' Stata: `table (DEGREE) (SEX)`
s.table(issp, DEGREE, SEX, missing = FALSE)

#' Nested rows and several statistics.
#' Stata: `table (country SEX) (TYPORG2), statistic(frequency) statistic(mean AGE) nformat(%9.1f mean)`
s.table(issp, c(country, SEX), TYPORG2,
        statistic = c("frequency", "mean"), vars = AGE,
        nformat = c(mean = "%9.1f"), missing = FALSE)

#' Statistics for several variables, only column variables.
#' Stata: `table () (SEX), statistic(mean AGE WRKHRS) statistic(sd AGE WRKHRS) statistic(p50 AGE WRKHRS)`
s.table(issp, cols = SEX, statistic = c("mean", "sd", "p50"),
        vars = c(AGE, WRKHRS))

#' Percentages. `rowpercent` and `colpercent` correspond to Stata's
#' `percent, across()`.
s.table(issp, country, SEX,
        statistic = c("percent", "rowpercent", "colpercent"))

#' A separate table for each value of a table variable, without totals.
#' Stata: `table (SEX) (TYPORG2) (country), nototals`
s.table(issp, SEX, TYPORG2, tabs = country, totals = FALSE, missing = FALSE)

#' Quantiles and further statistics, weighted.
#' Stata: `table (country) [aweight = WEIGHT], statistic(p25 WRKHRS) statistic(median WRKHRS) statistic(p75 WRKHRS) statistic(iqr WRKHRS) statistic(count WRKHRS)`
s.table(issp, country, statistic = c("p25", "median", "p75", "iqr", "count"),
        vars = WRKHRS, wt = WEIGHT, weight_type = "aweight")

#' Mean job satisfaction across all waves and the four countries.
#' (Sweden did not take part in 1989.)
#' Stata: `table (country) (year) if inlist(country, 276, 752, 826, 840), statistic(mean v40) nformat(%9.2f)`
issp_all |>
  filter(country %in% c(276, 826, 752, 840)) |>
  s.table(country, year, statistic = "mean", vars = v40, nformat = "%9.2f")

#' Missing values of the row, column and table variables are their own
#' category by default (`table ..., missing`); missing values of the
#' variables in `vars` are always excluded from the statistics.
#' Stata: `table (DEGREE), statistic(mean WRKHRS)`
s.table(issp, DEGREE, statistic = "mean", vars = WRKHRS, missing = FALSE)

#' `group_by()` works like Stata's `by`: one table per group, no total
#' across groups.
issp |> group_by(country) |> s.table(SEX, statistic = "mean", vars = AGE)

#' *Addon:* `firstcell` writes a text into the empty cell at the top left
#' of the table, e.g. what the numbers are.
s.table(issp, DEGREE, SEX, statistic = "mean", vars = WRKHRS, missing = FALSE,
        nformat = "%9.1f", firstcell = "Mean hours worked")

#' ## Display formats
#'
#' Stata describes how a number is shown by a *display format* such as
#' `%9.2f` or `%8.0g`. statsalike uses the same notation in the option
#' `nformat` of `s.table()` and `s.etable()` (Stata's `nformat()`), and
#' `s.describe()` lists the display format of every variable. A format reads
#' `%` *width* `.` *digits* *type*:
#'
#' | Format | Meaning | 1234.5678 | 0.0123 |
#' |---|---|---|---|
#' | `%9.0g` | general: up to 7 significant digits, Stata's default for numbers | `1234.568` | `0.0123` |
#' | `%8.0g` | the same (Stata's default for integer variables, byte, int) | `1234.568` | `0.0123` |
#' | `%9.2f` | fixed: exactly 2 decimals | `1234.57` | `0.01` |
#' | `%9.0f` | fixed: no decimals (rounded) | `1235` | `0` |
#' | `%9.3g` | general with 3 significant digits | `1.23e+03` | `0.0123` |
#' | `%9.2fc` | fixed with thousands separators (`c`) | `1,234.57` | `0.01` |
#' | `%10.3e` | exponential: 3 decimals | `1.235e+03` | `1.230e-02` |
#'
#' * The **width** (the 9 in `%9.2f`) is the number of characters Stata
#'   reserves. statsalike sizes its columns to their content, so the width is
#'   accepted but makes no difference.
#' * The **digits** are the number of decimals for `f` and `e` and the number
#'   of significant digits for `g`; `.0g` lets the program choose.
#' * `nformat` only changes how numbers are printed and exported; the
#'   values in the result (a tibble) keep their full precision.
#' * Stata drops the leading zero (`.0123`). The output of the estimation
#'   commands does so too, the tables of `s.table()` and `s.etable()` keep
#'   it (`0.0123`), as is usual in publications.
#' * Any `sprintf()` format works as well, e.g. `"%.1f%%"` for a percent sign.
#'
#' Without `nformat`, frequencies are whole numbers, percentages have two
#' decimals and other statistics the general format:
s.table(issp, country, statistic = c("frequency", "percent", "mean"), vars = WRKHRS)

#' With one format for all statistics (except the frequencies):
s.table(issp, country, statistic = c("frequency", "percent", "mean"), vars = WRKHRS,
        nformat = "%9.1f")

#' Different formats per statistic, named as in `statistic`:
s.table(issp, country, statistic = c("frequency", "mean", "sd"), vars = WRKHRS,
        nformat = c(frequency = "%9.0fc", mean = "%9.2f", sd = "%9.3f"))

#' `s.describe()` shows the display format of every variable (`format.stata`
#' as stored in a Stata file, otherwise Stata's default: `%10.0g` for numbers,
#' `%8.0g` for integers, `%td` for dates); `s.summarize(format = TRUE)` uses
#' these formats, as Stata's `summarize, format`.
s.describe(issp, AGE, WRKHRS, WEIGHT)

#' # `s.pwcorr()`: pairwise correlations
#'
#' Stata: `pwcorr AGE WRKHRS v40 v34 TOPBOT`. Each coefficient uses all
#' observations with valid values in both variables (pairwise deletion).
s.pwcorr(issp, AGE, WRKHRS, v40, v34, TOPBOT)

#' Significance levels, numbers of observations and stars.
#' Stata: `pwcorr AGE WRKHRS v40 v34, sig obs star(.05)`
s.pwcorr(issp, AGE, WRKHRS, v40, v34, sig = TRUE, obs = TRUE, star = 0.05)

#' Only coefficients significant at the 1% level, Bonferroni-adjusted, with
#' the observations that are complete in all variables.
#' Stata: `pwcorr AGE WRKHRS v40 v34 TOPBOT, print(.01) bonferroni listwise`
s.pwcorr(issp, AGE, WRKHRS, v40, v34, TOPBOT, print = 0.01, bonferroni = TRUE,
         listwise = TRUE)

#' Weighted and grouped. Stata: `by country: pwcorr WRKHRS v40 [aweight = WEIGHT], sig`
issp |>
  group_by(country) |>
  s.pwcorr(WRKHRS, v40, wt = WEIGHT, weight_type = "aweight", sig = TRUE)

#' The result is a tibble with one row per pair:
s.pwcorr(issp, AGE, WRKHRS, v40, sig = TRUE) |>
  as_tibble() |>
  filter(var1 != var2)

#' # `s.regress()`: linear regression
#'
#' Stata: `regress WRKHRS AGE i.SEX i.DEGREE`. Labelled numeric variables are
#' continuous unless they are listed in `categorical` (Stata's `i.`);
#' factors and character variables are always categorical.
s.regress(issp, WRKHRS, AGE, SEX, DEGREE, categorical = c(SEX, DEGREE))

#' Weighted with robust standard errors.
#' Stata: `regress WRKHRS AGE i.SEX i.country [pweight = WEIGHT]`;
#' probability weights imply robust standard errors, as in Stata. As in
#' Stata, the header then has no analysis-of-variance table (SS, df, MS):
#' its sums of squares and the F test based on them assume equal error
#' variances, which robust standard errors do not. F is a Wald test instead,
#' and R-squared and root MSE are still reported.
s.regress(issp, WRKHRS, AGE, SEX, country, categorical = c(SEX, country),
          wt = WEIGHT, weight_type = "pweight")

#' Standardized coefficients. Stata: `regress v40 WRKHRS v34 TOPBOT, beta`
s.regress(issp, v40, WRKHRS, v34, TOPBOT, beta = TRUE)

#' Cluster-robust standard errors, all 25 countries of the 2015 wave.
#' Stata: `regress WRKHRS AGE i.SEX if year == 2015, vce(cluster country)`
issp_all |>
  filter(year == 2015) |>
  s.regress(WRKHRS, AGE, SEX, categorical = SEX, cluster = country)

#' `s.ereturn()` returns the model statistics, like `ereturn list`:
s.regress(issp, WRKHRS, AGE, SEX, categorical = SEX) |> s.ereturn()

#' *Departure from Stata:* a numeric variable with value labels that enters
#' as a numeric variable gives a hint, once per variable and session, how to
#' enter it as a factor. `options(statsalike.labelled_hint = FALSE)` turns it
#' off. Here `DEGREE` has value labels and is not listed in `categorical`:
s.regress(issp, WRKHRS, AGE, DEGREE)

#' Dates (`Date`), times (`POSIXct`) and time differences (`difftime`) can be
#' regressors. They enter as numbers counted from R's origin 1970-01-01
#' (Stata counts from 1960-01-01), so the slope is the same as in Stata and
#' only the constant differs. A note below the output names the unit. For a
#' constant that can be interpreted, subtract a reference date first.
set.seed(1)
d_dates <- tibble(day = as.Date("2024-01-01") + 0:59,
                  sales = 100 + 0.5 * (0:59) + rnorm(60, sd = 5))
s.regress(d_dates, sales, day)
d_dates |>
  mutate(days = as.numeric(day - as.Date("2024-01-01"))) |>
  s.regress(sales, days)

#' # `s.logit()`: logistic regression
#'
#' Trade union membership (`UNION` = 1, "Yes, currently").
#' Stata: `generate member = UNION == 1 if !missing(UNION)` and
#' `logit member AGE i.SEX i.TYPORG2`, with the iteration log.
issp |>
  mutate(member = UNION == 1) |>
  s.logit(member, AGE, SEX, TYPORG2, categorical = c(SEX, TYPORG2))

#' Odds ratios without the iteration log, robust standard errors. High job
#' satisfaction means completely or very satisfied (`v40` 1 or 2).
#' Stata: `logit satisfied AGE WRKHRS i.SEX, or nolog vce(robust)`
issp |>
  mutate(satisfied = v40 <= 2) |>
  s.logit(satisfied, AGE, WRKHRS, SEX, categorical = SEX, or = TRUE,
          nolog = TRUE, vce = "robust")

#' # `s.etable()`: tables of several models
#'
#' `s.etable()` reproduces Stata's `etable` (Stata 17 and later): several
#' models side by side, each cell with the coefficient, significance stars
#' and the standard error in parentheses, the number of observations and
#' R-squared at the bottom. Row labels and column headings come from the
#' variable labels.
#'
#' Stata: `regress WRKHRS AGE i.SEX`, `estimates store m1`,
#' `regress WRKHRS AGE i.SEX i.DEGREE i.country`, `estimates store m2`,
#' `etable, estimates(m1 m2) showstars showstarsnote`
m1 <- s.regress(issp, WRKHRS, AGE, SEX, categorical = SEX)
m2 <- s.regress(issp, WRKHRS, AGE, SEX, DEGREE, country,
                categorical = c(SEX, DEGREE, country))
s.etable(m1, m2)

#' ## Own row and column labels
#'
#' Like `pred.labels` and `dv.labels` in `sjPlot::tab_model()`, row labels
#' and column headings can be set freely, with spaces, without renaming any
#' variable. `rowlabels` takes variable names, `"variable:category"` for a
#' category and `"_cons"` for the constant; the column headings are the
#' names of the models (or `collabels`), the labels of the model
#' statistics the names in `mstat`. `notes` replaces the notes below the
#' table, `title` sets a heading, and `export` writes the table to HTML or
#' Word.
s.etable("Model 1" = m1, "Model 2" = m2,
         rowlabels = c(AGE = "Age in years", SEX = "Sex (ref.: male)",
                    "SEX:Female" = "Female",
                    DEGREE = "Education (ref.: no formal qualification)",
                    country = "Country (ref.: Germany)",
                    "_cons" = "Constant"),
         mstat = c("Observations" = "N", "R²" = "r2", "Adjusted R²" = "r2_a"),
         title = "Table 3: Hours worked per week",
         export = "export/etable.html")

#' *Addon:* `firstcell` puts a text into the empty cell at the top left,
#' e.g. the dependent variable when the columns are named by model.
s.etable("M1" = m1, "M2" = m2, firstcell = "Hours worked per week",
         drop = "country")

#' ## Cell statistics, selection of rows
#'
#' Confidence intervals instead of standard errors, without the country
#' indicators. Stata: `etable, estimates(m1 m2) cstat(_r_b) cstat(_r_ci) drop(i.country)`
s.etable(m1, m2, cstat = "ci", drop = "country")

#' Further options: `cstat = "t"` or `"p"`, `keep`, `showbase = TRUE` for the
#' base categories, own significance levels with `stars`, and the number
#' format with `nformat`.
s.etable(m1, cstat = "p", stars = c(0.1, 0.05, 0.01), showbase = TRUE,
         nformat = "%9.2f")

#' ## Logit models
#'
#' Odds ratios with `eform = TRUE`; pseudo R-squared (McFadden, as in Stata)
#' and, as an addition to Stata, Nagelkerke's R-squared.
#' Stata: `logit member AGE i.SEX i.TYPORG2` and
#' `etable, cstat(_r_b) cstat(_r_se) eform mstat(N) mstat(r2_p)`
l1 <- issp |>
  mutate(member = UNION == 1) |>
  s.logit(member, AGE, SEX, TYPORG2, categorical = c(SEX, TYPORG2), nolog = TRUE)
s.etable("Union member" = l1, eform = TRUE,
         mstat = c("Observations" = "N", "Pseudo R²" = "r2_p", "Nagelkerke R²" = "r2_n"))

#' ## Addon: models estimated with R
#'
#' *This goes beyond Stata, whose `etable` only knows Stata's own
#' estimation results.* `s.etable()` also takes models estimated with R:
#' `lm()` and `glm()` directly (with Wald confidence intervals and model
#' statistics as in Stata), every other model class that
#' `sjPlot::tab_model()` supports via the package `parameters`. All of them
#' can be mixed in one table.
#'
#' For R's modelling functions, categorical variables are factors; haven's
#' `as_factor()` turns the value labels into factor levels (levels that do
#' not occur, such as the labels of missing-value codes, are dropped by
#' `lm()` and `glm()`). With the same data, `lm()` and `s.regress()` give
#' identical results, and the rows are merged because the category names
#' are the same:
issp_f <- issp |>
  mutate(across(c(SEX, DEGREE, TYPORG2, country), haven::as_factor),
         member = UNION == 1)
fit_lm <- lm(WRKHRS ~ AGE + SEX, data = issp_f)
s.etable("s.regress()" = m1, "lm()" = fit_lm)

#' The same for `glm()` and `s.logit()`:
fit_glm <- glm(member ~ AGE + SEX + TYPORG2, family = binomial, data = issp_f)
s.etable("s.logit()" = l1, "glm()" = fit_glm, eform = TRUE,
         mstat = c("N", "r2_p", "ll"))

#' R's formula syntax works as well: interactions and transformations keep
#' R's names and can be labelled like any other term.
fit_int <- lm(log(WRKHRS) ~ AGE + I(AGE^2) + SEX * TYPORG2, data = issp_f)
s.etable(fit_int,
         rowlabels = c("I(AGE^2)" = "Age squared",
                    "SEXFemale:TYPORG2Private employer" = "Female x private employer"),
         mstat = c("N", "r2", "r2_a"))

#' Other model classes need the package `parameters`. A multilevel model
#' with a random intercept for each of the 25 countries of the 2015 wave
#' (package `lme4`) next to the OLS model; only the fixed effects are shown,
#' with the marginal and conditional R-squared as model statistics:
#+ lmer, eval = requireNamespace("lme4", quietly = TRUE) && requireNamespace("parameters", quietly = TRUE)
issp15 <- issp_all |>
  filter(year == 2015) |>
  mutate(SEX = haven::as_factor(SEX))
fit_ols <- lm(WRKHRS ~ AGE + SEX, data = issp15)
fit_mlm <- lme4::lmer(WRKHRS ~ AGE + SEX + (1 | country), data = issp15)
s.etable("OLS" = fit_ols, "Multilevel" = fit_mlm,
         mstat = c("N", "r2", "r2_m", "r2_c"))

#' Further equations get their own block, e.g. the zero-inflation part of a
#' count model (package `pscl`). Its rows are addressed in `rowlabels` as
#' `"zero_inflated::term"`:
#+ zeroinfl, eval = requireNamespace("pscl", quietly = TRUE) && requireNamespace("parameters", quietly = TRUE)
issp_count <- issp_f |>
  filter(!is.na(WRKHRS), !is.na(v34)) |>
  mutate(overtime = pmax(round(WRKHRS) - 40, 0))
fit_zi <- pscl::zeroinfl(overtime ~ AGE + SEX | v34, data = issp_count)
s.etable("Overtime hours" = fit_zi,
         rowlabels = c("zero_inflated::" = "Zero part: no overtime",
                    "zero_inflated::v34" = "Work is stressful (1 = always, 5 = never)"),
         mstat = c("N", "ll"))

#' Which model classes are supported and how the table differs from
#' `sjPlot::tab_model()` is described in `help("s.etable")`.
#'
#' # Further models, tests and postestimation
#'
#' Probit, ordered probit, negative binomial and tobit regression work like
#' `s.logit()`. `ame = TRUE` reports the average marginal effects of all
#' regressors in place of the coefficients (an addition to Stata, where
#' `margins, dydx(*)` is a separate step). Log odds, odds ratios and average
#' marginal effects of one model fit into one table, with Tjur's R-squared as
#' a further addition. Stata: `logit member AGE i.SEX i.TYPORG2`, `margins, dydx(*)`
issp_m <- issp |>
  mutate(member = UNION == 1, across(c(SEX, TYPORG2), haven::as_factor))
lg <- s.logit(issp_m, member, AGE, SEX, TYPORG2, nolog = TRUE)
lg_or <- s.logit(issp_m, member, AGE, SEX, TYPORG2, nolog = TRUE, or = TRUE)
lg_ame <- s.logit(issp_m, member, AGE, SEX, TYPORG2, nolog = TRUE, ame = TRUE)
s.etable("Log-Odds" = lg, "Odds Ratios" = lg_or, "AME" = lg_ame,
         mstat = c("N", "r2_p", "r2_t"))
s.probit(issp_m, member, AGE, SEX, TYPORG2, nolog = TRUE)

#' Interactions need no prefixes (`i.`, `c.`). Whether a variable is a
#' factor or numeric follows from its type, and a note below the output says
#' how each one enters the interaction. Stata: `regress WRKHRS c.AGE##i.SEX`
reg_int <- s.regress(issp_m, WRKHRS, "AGE##SEX")
reg_int
s.margins(reg_int, SEX, at = list(AGE = 50), pwcompare = "effects")
s.lincom(reg_int, "2.SEX + 2.SEX#AGE")

#' Contrasts of factor variables. A name alone gives the joint test of the
#' variable (all categories equal), an operator the single contrasts:
#' `r.` against the base category, `ar.` against the previous category
#' (further: `a.`, `g.`, `h.`, `j.`, `p.`, `q.`, see `help("s.contrast")`).
#' Stata: `contrast DEGREE country` and `contrast ar.DEGREE`
s.contrast(m2, DEGREE, country)
s.contrast(m2, ar.DEGREE)

#' With interactions: the joint test of the interaction and the simple
#' effects of `SEX` within every category of `TYPORG2` (`@`).
#' Stata: `regress WRKHRS AGE i.SEX##i.TYPORG2`, `contrast SEX#TYPORG2 r.SEX@TYPORG2, effects`
reg_fac <- s.regress(issp_m, WRKHRS, AGE, "SEX##TYPORG2")
s.contrast(reg_fac, "SEX#TYPORG2", "r.SEX@TYPORG2", effects = TRUE)

#' Tests of means and a reliability coefficient. Stata: `ttest WRKHRS, by(SEX)`,
#' `oneway WRKHRS DEGREE, tabulate` and `alpha v12 v34 v40, item`
s.ttest(issp_m, WRKHRS, by = SEX)
s.oneway(issp, WRKHRS, DEGREE, tabulate = TRUE)
s.alpha(issp, v12, v34, v40, item = TRUE)
#'
#' # Working with the results
#'
#' Every result is a tibble in tidy format. It prints in Stata's layout but
#' can be processed further like any data set.
s.tab(issp, DEGREE, SEX, missing = FALSE, row = TRUE) |>
  as_tibble() |>
  select(DEGREE, SEX, freq, row_pct) |>
  head(6)

#' Wide format with tidyr:
s.table(issp, DEGREE, statistic = c("mean", "median"), vars = WRKHRS,
        totals = FALSE, missing = FALSE) |>
  tidyr::pivot_wider(names_from = statistic, values_from = value)

#' Directly into a chart:
#+ plot, fig.width = 7, fig.height = 3.5, warning = FALSE
library(ggplot2)
s.tab(issp, country, UNION, missing = FALSE, row = TRUE) |>
  ggplot(aes(x = row_pct, y = country, fill = UNION)) +
  geom_col() +
  labs(x = "Row percentage", y = NULL, fill = "Union member") +
  theme_minimal()

#' # Export to Word and HTML
#'
#' Every function that prints a table has an `export` option (the equivalent of Stata's
#' `collect export` or `putdocx`). The file type follows from the extension.
#' The table is printed as usual and also written to the file.
s.tab(issp, DEGREE, SEX, missing = FALSE, row = TRUE, chi2 = TRUE,
      export = "export/crosstab.docx")

s.table(issp, c(country, SEX), TYPORG2, statistic = c("frequency", "mean"),
        vars = AGE, missing = FALSE, export = "export/table.html")

#' `s.export()` writes existing results, also several into one file. Each
#' table is headed by the command that produced it; with `title` you
#' can set your own headings.
results <- list(
  s.describe(issp, country, SEX, AGE, DEGREE, WRKHRS),
  s.tab(issp, DEGREE),
  s.tab(issp, country, TYPORG2, wt = WEIGHT, weight_type = "aweight", missing = FALSE,
        row = TRUE),
  s.tab(issp, DEGREE, summarize = WRKHRS, missing = FALSE),
  s.table(issp, cols = SEX, statistic = c("mean", "sd"), vars = c(AGE, WRKHRS))
)
s.export(results, "export/all_tables.docx")
s.export(results, "export/all_tables.html",
         title = c("Variables", "Education", "Employer by country (weighted)",
                   "Working hours by education", "Age and working hours by sex"))

#' The exported files:
#' [crosstab.docx](export/crosstab.docx),
#' [table.html](export/table.html),
#' [all_tables.docx](export/all_tables.docx),
#' [all_tables.html](export/all_tables.html),
#' [etable.html](export/etable.html).
#'
#' `s.export()` returns its input invisibly, so it can end a pipe:
issp |>
  group_by(country) |>
  s.tab(SEX, TYPORG2, missing = FALSE, chi2 = TRUE) |>
  s.export("export/by_country.html")
