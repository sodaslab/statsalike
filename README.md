# statsalike

statsalike provides R commands for describing data (`describe`, `codebook`, `summarize`, `tabulate`, `table`, `pwcorr`), simple tests (`ttest`, `oneway`, `anova`, `alpha`), regression models (`regress`, `logit`, `probit`, `mlogit`, `ologit`, `oprobit`, `poisson`, `nbreg`, `tobit`), postestimation (`margins`, `predict`, `estat`, `test`, `lincom` and others) and regression tables (`etable`), in a tidyverse-friendly way. The commands and their output follow the corresponding commands of Stata®.

> Stata is a registered trademark of StataCorp LLC. statsalike is an independent implementation in R. It is not affiliated with, endorsed by or sponsored by StataCorp and contains no Stata code.
>
> The package was coded with the help of Claude (Anthropic), see [Use of AI](#use-of-ai).

* The data always comes first, so the commands work with `|>` and `%>%`.
* Variables are given unquoted (tidy evaluation, tidyselect).
* `group_by(x)` gives one result per group.
* Frequency, analytic, importance and (for regressions) sampling weights are supported (see `?statsalike-weights`).
* Value and variable labels from `haven` are displayed.
* The result is a **tibble** that prints as a formatted table. As soon as you process it further with `filter()`, `mutate()` etc., it becomes an ordinary tibble.

## Motivation

I transitioned from Stata to R years ago, but I always struggled with compiling basic descriptive tables and other basic routines. In order to get the results I was used to, I had to combine several packages. `statsalike` is meant to change that and to support everyone who is shifting from Stata to R or using both programs. The package is supposed to fill this gap and provide familiar output in R.

## Regression analysis (version 0.2.0)

With version 0.2.0 the package was extended from descriptive statistics to regression analysis. It covers linear, logistic, probit, multinomial logistic, ordered logistic and probit, Poisson, negative binomial and tobit regression (`s.regress()`, `s.logit()`, `s.probit()`, `s.mlogit()`, `s.ologit()`, `s.oprobit()`, `s.poisson()`, `s.nbreg()`, `s.tobit()`), categorical variables and interactions, and the commands for interpreting and checking the models. These are `s.margins()` (also with pairwise comparisons) and `s.marginsplot()`, `s.predict()`, `s.estat()`, `s.test()`, `s.lincom()`, `s.contrast()`, `s.lrtest()`, `s.linktest()`, `s.lroc()`, `s.lsens()` and the diagnostic plots (`s.rvfplot()`, `s.rvpplot()`, `s.lvr2plot()`, `s.avplots()`, `s.cprplot()`, `s.qnorm()`, `s.kdensity()`). The t test, one-way and n-way analysis of variance and Cronbach's alpha were added as well (`s.ttest()`, `s.oneway()`, `s.anova()`, `s.alpha()`).

**Independent implementation.** The computations and the output of all commands were written from scratch in R. They are based on the published statistical methods, on the public documentation of the reference commands and on the output these commands display (e.g. the step rules for maximum likelihood or the layout of the tables). No Stata code was copied, translated or otherwise used in the package. The same applies to other R packages whose approaches were compared (e.g. `sandwich`, `lmtest`, `car`, `margins`, `MASS`, `nnet`, `survival`). None of their code is part of `statsalike`. The package neither contains nor calls Stata.

**Tested against the reference output.** To check the implementation, the functions were compared automatically with the original commands of Stata 15.1. Scripts run the same commands on the same data in both programs and compare the output line by line. This covered about 350 commands on more than 20 datasets (from small examples such as `auto`, `nlsw88` and `mtcars` to ALLBUS 2021 and the cumulated ISSP Work Orientations). 83 of these comparisons are part of the package tests (`tests/testthat/test-stata-identity.R`), with the reference output in `tests/testthat/stata/`. Stata is only needed to rewrite these reference files, not to run the tests. The results were also compared with the corresponding R functions (`lm()`, `glm()`, `MASS::polr()`, `MASS::glm.nb()`, `survival::survreg()`, `sandwich::vcovHC()`, `t.test()`, `car::Anova()` and others).

Because `statsalike` follows the reference commands, some results differ from those of common R functions, mostly because of different defaults (e.g. robust standard errors HC1 instead of HC3, predicted probabilities instead of log odds from `predict()`, another version of the Breusch-Pagan test, equal variances in the t test). The help page `?"statsalike-stata-vs-r"` lists these differences and shows how to get the same results in R.

Time-series, panel and multilevel models and the survey design commands (`svy:`) are deliberately not covered.

## Where statsalike departs from the reference commands

* **No prefixes** `i.` **and** `c.` Whether a variable enters a model as a factor or as a numeric variable follows from its type in R. Interactions are written as `"x##g"` instead of `"c.x##i.g"`. Below the output a note tells how every variable enters the interaction, e.g. `cyl#wt = cyl (factor) x wt (numeric)`. The prefixes are still understood, and `options(statsalike.stata_notation = TRUE)` brings back the original labels.
* **Dates and times** (`Date`, `POSIXct`, `difftime`) can be regressors. They enter as numbers counted from R's origin 1970-01-01 (Stata counts from 1960-01-01). Only the constant differs. A note below the output names the unit and the origin.
* **Hint for labelled numeric variables.** Numeric variables with value labels (e.g. from .dta or .sav files) enter as numeric variables. The estimation commands show a hint, once per variable and session, how to enter them as factors (`categorical = x` or `haven::as_factor()`). `options(statsalike.labelled_hint = FALSE)` turns it off.
* **Average marginal effects in the estimation command.** `ame = TRUE` in `s.regress()`, `s.logit()` and the other estimation commands reports the average marginal effects of all regressors (`margins, dydx(*)`) in place of the coefficients. With `s.etable()` they can be shown next to the coefficients and odds ratios of the same model.
* **Tjur's R²** after logit and probit models, in `s.ereturn()` (`r2_t`) and in `s.etable(mstat = "r2_t")`.
* **Graphs** use the default design of ggplot2.

## Installation

```r
install.packages("statsalike")   # once the package is on CRAN
```

The development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("SODASLab/statsalike")
```

Bug reports and suggestions are welcome at <https://github.com/SODASLab/statsalike/issues>.

## Naming

All commands start with `s.` (for statsalike commands). Neither base R nor common packages have functions with these names so far. However, `ade4`/`adegraphics` also use the `s.` prefix for scatter plots (`s.label`, `s.class`, ...), so the two could be confused. If you load both packages, you can write `statsalike::s.tab()` to be safe.

## Overview of commands

The table below lists each statsalike command next to the Stata command whose output it follows. The table commands (`s.tab()`, `s.tab1()`, `s.tab2()`, `s.table()`) show missing values as a category by default (the option `missing`). Add `missing = FALSE` for percentages and test statistics of valid observations (see the implementation notes below).

| Reference command (Stata)                                           | statsalike                                                                                                |
|---------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------|
| `describe`                                                          | `s.describe(df)`                                                                                         |
| `describe x y, numbers`                                             | `s.describe(df, x, y, numbers = TRUE)`                                                                   |
| `codebook`                                                          | `s.codebook(df)`                                                                                         |
| `codebook, compact` / `codebook, problems`                          | `s.codebook(df, compact = TRUE)` / `s.codebook(df, problems = TRUE)`                                     |
| `summarize`                                                         | `s.summarize(df)` or `s.sum(df)`                                                                         |
| `summarize x [aweight = w], detail`                                 | `s.sum(df, x, wt = w, weight_type = "aweight", detail = TRUE)`                                           |
| `pwcorr x y z, sig obs star(.05)`                                   | `s.pwcorr(df, x, y, z, sig = TRUE, obs = TRUE, star = 0.05)`                                             |
| `pwcorr x y z, sig bonferroni listwise`                             | `s.pwcorr(df, x, y, z, sig = TRUE, bonferroni = TRUE, listwise = TRUE)`                                  |
| `ttest x == 0` / `ttest x, by(g) unequal`                           | `s.ttest(df, x, mu = 0)` / `s.ttest(df, x, by = g, unequal = TRUE)`                                      |
| `ttest x == y`                                                      | `s.ttest(df, x, y)`                                                                                      |
| `oneway y g, tabulate bonferroni`                                   | `s.oneway(df, y, g, tabulate = TRUE, bonferroni = TRUE)`                                                 |
| `anova y a##b c.x`                                                  | `s.anova(df, y, "a##b", x, continuous = x)`                                                              |
| `alpha x1 x2 x3, item std`                                          | `s.alpha(df, x1, x2, x3, item = TRUE, std = TRUE)`                                                       |
| `regress y x1 x2 i.g`                                               | `s.regress(df, y, x1, x2, g)` (`g` a factor, or `categorical = g` for a numeric `g`)                     |
| `regress y x, vce(robust)` / `vce(hc3)` / `vce(cluster c)`          | `s.regress(df, y, x, vce = "robust")` / `vce = "hc3"` / `cluster = c`                                    |
| `regress y x, beta`                                                 | `s.regress(df, y, x, beta = TRUE)`                                                                       |
| `regress y c.x##i.g ib3.h`                                          | `s.regress(df, y, "x##g", h, categorical = h, base = c(h = 3))`                                          |
| `logit y x1 x2, or`                                                 | `s.logit(df, y, x1, x2, or = TRUE)`                                                                      |
| `logit y x1 x2` and `margins, dydx(*)`                              | `s.logit(df, y, x1, x2, ame = TRUE)`                                                                     |
| `probit y x1 x2` / `oprobit y x1 x2`                                | `s.probit(df, y, x1, x2)` / `s.oprobit(df, y, x1, x2)`                                                   |
| `mlogit y x1 x2, baseoutcome(1) rrr`                                | `s.mlogit(df, y, x1, x2, baseoutcome = 1, rrr = TRUE)`                                                   |
| `ologit y x1 x2, or`                                                | `s.ologit(df, y, x1, x2, or = TRUE)`                                                                     |
| `poisson y x, exposure(t) irr`                                      | `s.poisson(df, y, x, exposure = t, irr = TRUE)`                                                          |
| `nbreg y x, irr`                                                    | `s.nbreg(df, y, x, irr = TRUE)`                                                                          |
| `tobit y x, ll(0)` / `tobit y x, ll ul`                             | `s.tobit(df, y, x, ll = 0)` / `s.tobit(df, y, x, ll = TRUE, ul = TRUE)`                                  |
| `margins, dydx(*)` / `margins, dydx(*) atmeans`                     | `s.margins(m, dydx = "*")` / `s.margins(m, dydx = "*", atmeans = TRUE)`                                  |
| `margins g, at(x=(1(1)5))` / `margins a#b`                          | `s.margins(m, g, at = list(x = 1:5))` / `s.margins(m, "a#b")`                                            |
| `margins g, pwcompare(effects)`                                     | `s.margins(m, g, pwcompare = "effects")`                                                                 |
| `marginsplot`                                                       | `s.marginsplot(s.margins(m, ...))`                                                                       |
| `predict r, residuals` / `rstandard` / `cooksd`                     | `s.predict(df, m, "r", type = "residuals")`                                                              |
| `estat hettest` / `vif` / `ovtest` / `ic`                           | `s.estat(m, "hettest")` etc.                                                                             |
| `estat gof, group(10)` / `estat classification`                     | `s.estat(m, "gof", group = 10)` / `s.estat(m, "classification")`                                         |
| `test x1 x2` / `testparm i.g`                                       | `s.test(m, x1, x2)` / `s.test(m, "i.g")`                                                                 |
| `lincom x1 - x2`                                                    | `s.lincom(m, "x1 - x2")`                                                                                 |
| `contrast g` / `contrast r.g, effects` / `contrast a#b r.a@b`       | `s.contrast(m, g)` / `s.contrast(m, r.g, effects = TRUE)` / `s.contrast(m, "a#b", "r.a@b")`              |
| `lrtest m1 m2`                                                      | `s.lrtest(m1, m2)`                                                                                       |
| `linktest` / `lroc` / `lsens`                                       | `s.linktest(m)` / `s.lroc(m)` / `s.lsens(m)`                                                             |
| `rvfplot`, `rvpplot x`, `lvr2plot`, `qnorm r`, `kdensity r, normal` | `s.rvfplot(m)`, `s.rvpplot(m, x)`, `s.lvr2plot(m)`, `s.qnorm(df, r)`, `s.kdensity(df, r, normal = TRUE)` |
| `avplot x` / `avplots` / `cprplot x, lowess`                        | `s.avplot(m, x)` / `s.avplots(m)` / `s.cprplot(m, x, lowess = TRUE)`                                     |
| `ereturn list`                                                      | `s.ereturn(model)`                                                                                       |
| `etable, estimates(m1 m2) showstars`                                | `s.etable(m1, m2)` (own labels with `rowlabels = c(x = "My label")` and `collabels`)                     |
| `tabulate x, missing`                                               | `s.tab(df, x)` (missing values are shown by default)                                                     |
| `tabulate x`                                                        | `s.tab(df, x, missing = FALSE)`                                                                          |
| `tabulate x, missing sort plot`                                     | `s.tab(df, x, sort = TRUE, plot = TRUE)`                                                                 |
| `tabulate x y, row col chi2 V`                                      | `s.tab(df, x, y, missing = FALSE, row = TRUE, column = TRUE, chi2 = TRUE, V = TRUE)`                     |
| `tabulate x y, exact gamma taub lrchi2`                             | `s.tab(df, x, y, missing = FALSE, exact = TRUE, gamma = TRUE, taub = TRUE, lrchi2 = TRUE)`               |
| `tabulate x y, all expected cchi2`                                  | `s.tab(df, x, y, all = TRUE, expected = TRUE, cchi2 = TRUE)`                                             |
| `tabulate x, summarize(y)`                                          | `s.tab(df, x, summarize = y)`                                                                            |
| `tabulate x, generate(d)`                                           | `s.tabgen(df, x, prefix = "d")`                                                                          |
| `tab1 x y z`                                                        | `s.tab1(df, x, y, z)`                                                                                    |
| `tab2 x y z, firstonly`                                             | `s.tab2(df, x, y, z, firstonly = TRUE)`                                                                  |
| `tabulate x [fweight = w]`                                          | `s.tab(df, x, wt = w)`                                                                                   |
| `tabulate x [aweight = w]`                                          | `s.tab(df, x, wt = w, weight_type = "aweight")`                                                          |
| `by g: tabulate x`                                                  | `df \\|> group_by(g) \|> s.tab(x)`                                                                       |
| `tabulate x if age > 30`                                            | `df \\|> filter(age > 30) \|> s.tab(x)`                                                                  |
| `table (x) (y)`                                                     | `s.table(df, x, y)`                                                                                      |
| `table (x z) (y)`                                                   | `s.table(df, c(x, z), y)`                                                                                |
| `table () (y) (z)`                                                  | `s.table(df, cols = y, tabs = z)`                                                                        |
| `table x, stat(mean a b) stat(sd a b)`                              | `s.table(df, x, statistic = c("mean", "sd"), vars = c(a, b))`                                            |
| `table x y, stat(freq) stat(percent) nototals`                      | `s.table(df, x, y, statistic = c("frequency", "percent"), totals = FALSE)`                               |
| `table x, stat(p25 a) nformat(%9.2f)`                               | `s.table(df, x, statistic = "p25", vars = a, nformat = "%9.2f")`                                         |
| `return list` (test statistics)                                     | `s.tests(tab)`                                                                                           |
| `collect export tab.docx` / `putdocx`                               | `s.tab(df, x, y, export = "tab.docx")` or `s.export(tab, "tab.docx")`                                    |
| *(no equivalent, addon)*                                            | `s.tab(df, x, entropy = TRUE)`                                                                           |

### Addons

* `s.etable()` also takes models estimated with R, `lm()` and `glm()` directly, and every model class that `sjPlot::tab_model()` supports (via the suggested package 'parameters'), e.g. `lme4::lmer()`, `glmmTMB::glmmTMB()`, `MASS::polr()`, `nnet::multinom()`, `pscl::zeroinfl()`, `survival::coxph()`, `fixest::feols()` or `survey::svyglm()`. They can be mixed with the models of `statsalike` in one table. Further equations (zero-inflation, cut points, outcomes of multinomial models) get their own block. Additional model statistics are Nagelkerke's R² (`r2_n`), Tjur's R² (`r2_t`) and the marginal and conditional R² of mixed models (`r2_m`, `r2_c`).
* `s.etable()` also shows average marginal effects (models with `ame = TRUE` or results of `s.margins()`), e.g. log odds, odds ratios and marginal effects of one model in three columns.
* `firstcell` in `s.etable()` and `s.table()` writes a text into the top left cell of the table, e.g. the dependent variable (`firstcell = "Hours worked per week"`).
* `entropy = TRUE` in `s.tab()` and `s.tab1()` (one-way tables) reports the Shannon entropy *H* = −Σ *p* ln *p* of the distribution and the relative entropy *H* / ln *k* (*k* = number of categories, 0 = all observations in one category, 1 = uniform distribution). It respects weights and missing categories (`missing = FALSE` for valid observations only). The values are also returned by `s.tests()`.

## Examples

A report with examples for every function and option is in the folder `examples/` of the source repository (it is not part of the installed package). It consists of the script `examples/statsalike_examples.R` (open it in RStudio and use *File > Compile Report*) and the compiled `examples/statsalike_examples.html`, plus the files exported in it under `examples/export/`.

```r
library(dplyr)
library(statsalike)

df <- as_tibble(mtcars)

s.describe(df)

df |> s.tab(cyl)
```

```
  cyl |  Freq.  Percent    Cum.
------+------------------------
    4 |     11    34.38   34.38
    6 |      7    21.88   56.25
    8 |     14    43.75  100.00
------+------------------------
Total |     32   100.00
```

```r
df |> s.tab(cyl, am, row = TRUE, chi2 = TRUE)
df |> s.tab(cyl, summarize = mpg)
df |> group_by(vs) |> s.tab(gear)
df |> s.table(cyl, am, statistic = c("mean", "sd"), vars = mpg, nformat = "%9.2f")
```

A logit model with odds ratios and average marginal effects in one table

```r
d <- mutate(df, cyl = factor(cyl))
m <- s.logit(d, am, wt, cyl, nolog = TRUE)
m_or <- s.logit(d, am, wt, cyl, nolog = TRUE, or = TRUE)
m_ame <- s.logit(d, am, wt, cyl, nolog = TRUE, ame = TRUE)
s.etable("Log-Odds" = m, "Odds Ratios" = m_or, "AME" = m_ame, mstat = c("N", "r2_p", "r2_t"))
```

### Export to Word or HTML

Every command that prints a table has an `export` option. The file type follows from the extension. `s.export()` does the same for existing results and can put several tables into one file.

```r
df |> s.tab(cyl, am, row = TRUE, chi2 = TRUE, export = "crosstab.docx")
df |> s.table(cyl, am, statistic = c("mean", "sd"), vars = mpg, export = "means.html")

s.export(list(s.describe(df), s.tab(df, cyl), s.table(df, cyl, am)), "all_tables.docx")
```

The exported tables have the same layout as the printed output (rules, nested rows, multi-line cells) and are headed by the command that produced them (`title = FALSE` removes it, or pass your own titles). HTML files are self-contained. Word export needs the packages `officer` and `flextable`.

The results are tidy and can be used directly.

```r
s.tab(df, cyl, am) |>
  ggplot2::ggplot(ggplot2::aes(cyl, row_pct, fill = am)) +
  ggplot2::geom_col()

s.table(df, cyl, statistic = "mean", vars = c(mpg, hp)) |>
  tidyr::pivot_wider(names_from = variable, values_from = value)
```

## Implementation notes

* **Option names** follow the reference commands, e.g. `row`, `column`, `nofreq`, `nolabel`, `nokey`, `missing` and `V`. They are passed as `TRUE`/`FALSE`. Combinations that are not meaningful raise an error, e.g. `sort` with two-way tables or `chi2` with one-way tables.
* **Missing values** are shown as separate categories (or with their value label) in the tables of `s.tab()`, `s.tab1()`, `s.tab2()` and `s.table()` by default, so that the number of missing observations is always visible. This deviates from the reference commands. `missing = FALSE` excludes them. With the default, missing categories count towards percentages, totals and test statistics, so use `missing = FALSE` for results based on valid observations. All other commands exclude missing values. Missing are `NA`, empty strings `""`, the extended missing values `.a`–`.z` of .dta files and SPSS user-defined missing values (`haven::read_sav(user_na = TRUE)`). Missing values are displayed as R prints them, that is `<NA>` for missing categories, `NA` for statistics that cannot be computed and `NA(a)`, `NA(b)`, ... for extended missing values. The results of `s.tab()` and `s.table()` contain a genuine `NA`, so `is.na()` and `filter()` work as usual. Missing values of summarized variables are always excluded from statistics.
* **Fisher's exact test** is computed exactly where this is feasible in about a second. For larger r×c tables a Monte Carlo p-value (100,000 replications, drawn from R's random number generator, so `set.seed()` makes the result reproducible) is reported as `Fisher's exact (simulated)`.
* **Text with an invalid encoding** (e.g. a Latin-1 file read as UTF-8) is printed without errors. Columns may then be slightly misaligned.
* **Quantiles** in `s.table()` follow the definition of the reference commands, also with weights. For unweighted data this matches `quantile(type = 2)`.
* **ASEs** for gamma and tau-b are computed following Agresti.
* `s.describe()` shows R storage types (`dbl`, `int`, `chr`, `fct`, ...) and the display format. The format comes from `format.stata` (haven). If it is missing, the default format is used.
* **Display formats** (`%9.2f`, `%9.0g`, `%12.0fc`, ...) are understood by `nformat` in `s.table()` and `s.etable()` and shown by `s.describe()` and `s.summarize(format = TRUE)`. The section "Display formats" in `?"statsalike-stata-vs-r"` explains them.
* `table`**,** `percent`**.** The denominator is the respective table, so with `tabs` each sub-table is computed on its own. `rowpercent` and `colpercent` correspond to `percent, across()`.
* `group_by()` **with** `s.table()` gives one table per group, without a total table across groups.
* `s.summarize()` uses the same formulas as the reference command, including percentiles, skewness and kurtosis with weights. A `meanonly` option is not needed, as all statistics are returned as a tibble.
* **Estimation commands** (`s.regress()`, `s.logit()`, `s.probit()`, `s.mlogit()`, `s.ologit()`, `s.oprobit()`, `s.poisson()`, `s.nbreg()`, `s.tobit()`) and their postestimation commands reproduce the reference output of Stata 15.1 character by character (see *Regression analysis* above and `?"statsalike-stata-vs-r"` for the points where this differs from common R practice). They use listwise deletion and enter factors and character variables as indicator variables with the first category as base. Robust standard errors are HC1, cluster-robust ones use the usual small-sample adjustment, and sampling weights imply robust standard errors. The maximum-likelihood models use the same Newton-Raphson steps as the reference (identical iteration logs, except for `s.tobit()`, whose starting values differ). `s.margins()` computes the delta-method standard errors with analytic derivatives instead of numerical ones. In rare cases the last displayed digit therefore differs.

## Use of AI

`statsalike` was coded with the help of Claude, the AI model of Anthropic (via Claude Code). Claude drafted large parts of the code, the documentation, the tests and the example report, based on the author's specification and design decisions. All AI-assisted output was reviewed, tested and verified by the author, who is solely responsible for the package and its correctness.

## Trademark

Stata® is a registered trademark of StataCorp LLC, College Station, TX, USA. The name is used here only to identify the commands whose output statsalike follows. `statsalike` is not affiliated with, endorsed by or sponsored by StataCorp. The package is an independent implementation. It reproduces the functionality and output layout of these commands from published statistical methods and contains no code or documentation of Stata.

## License

GPL-3