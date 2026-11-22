#' statsalike: Tables, Summaries and Regressions for Data Frames
#'
#' The package provides commands for describing data, tabulating,
#' summarizing, simple tests and regression. Their output follows the
#' corresponding commands of 'Stata'. All commands start with `s.` (for
#' statsalike commands).
#'
#' 'Stata' is a registered trademark of StataCorp LLC. statsalike is an
#' independent implementation in R. It is not affiliated with, endorsed by or
#' sponsored by StataCorp and contains no 'Stata' code. The package was coded
#' with the help of Claude (Anthropic), see the section Use of AI.
#'
#' The table lists each statsalike command next to the reference command.
#'
#' | Stata                         | statsalike                                  |
#' |-------------------------------|--------------------------------------------|
#' | `tabulate x, missing`         | [s.tabulate()] or [s.tab()]                |
#' | `tabulate x`                  | `s.tab(df, x, missing = FALSE)`            |
#' | `tabulate x y, row chi2`      | `s.tab(df, x, y, row = TRUE, chi2 = TRUE)` |
#' | `tab1 x y z`                  | [s.tab1()]                                 |
#' | `tab2 x y z`                  | [s.tab2()]                                 |
#' | `tabulate x, summarize(y)`    | `s.tab(df, x, summarize = y)`              |
#' | `tabulate x, generate(d)`     | [s.tabgen()]                               |
#' | `table (r) (c), stat(mean y)` | [s.table()]                                |
#' | `describe`                    | [s.describe()]                             |
#' | `codebook`                    | [s.codebook()]                             |
#' | `summarize x, detail`         | [s.summarize()] or [s.sum()]               |
#' | `pwcorr x y z, sig obs`       | `s.pwcorr(df, x, y, z, sig = TRUE, obs = TRUE)` |
#' | `ttest x, by(g)`              | [s.ttest()]                                |
#' | `oneway y g, bonferroni`      | [s.oneway()]                               |
#' | `anova y a##b`                | [s.anova()]                                |
#' | `alpha x1 x2 x3, item`        | [s.alpha()]                                |
#' | `regress y x1 x2`             | [s.regress()]                              |
#' | `regress y c.x##i.g`          | `s.regress(df, y, "x##g")` (`g` a factor)  |
#' | `logit y x1 x2`               | [s.logit()]                                |
#' | `probit y x1 x2`              | [s.probit()]                               |
#' | `mlogit y x1 x2`              | [s.mlogit()]                               |
#' | `ologit y x1 x2`              | [s.ologit()]                               |
#' | `oprobit y x1 x2`             | [s.oprobit()]                              |
#' | `poisson y x1 x2, irr`        | [s.poisson()]                              |
#' | `nbreg y x1 x2, irr`          | [s.nbreg()]                                |
#' | `tobit y x1 x2, ll(0)`        | [s.tobit()]                                |
#' | `margins g, dydx(*) at(x=(1 2))` | [s.margins()]                           |
#' | `margins g, pwcompare`        | `s.margins(m, g, pwcompare = TRUE)`        |
#' | `marginsplot`                 | [s.marginsplot()]                          |
#' | `predict r, residuals`        | [s.predict()]                              |
#' | `estat hettest` / `vif` / `ovtest` / `ic` / `gof` / `classification` | [s.estat()] |
#' | `test x1 x2`, `testparm i.g`  | [s.test()]                                 |
#' | `lincom x1 - x2`              | [s.lincom()]                               |
#' | `contrast r.g`, `contrast a#b r.a@b` | [s.contrast()]                      |
#' | `lrtest m1 m2`                | [s.lrtest()]                               |
#' | `linktest`, `lroc`, `lsens`   | [s.linktest()], [s.lroc()], [s.lsens()]    |
#' | `rvfplot`, `rvpplot`, `lvr2plot`, `qnorm`, `kdensity` | [s.rvfplot()], [s.rvpplot()], [s.lvr2plot()], [s.qnorm()], [s.kdensity()] |
#' | `avplot`, `avplots`, `cprplot` | [s.avplot()], [s.avplots()], [s.cprplot()] |
#' | `ereturn list`                | [s.ereturn()]                              |
#' | `etable, estimates(m1 m2)`    | `s.etable(m1, m2)`                         |
#' | `by g: tabulate x`            | `df |> group_by(g) |> s.tab(x)`            |
#' | `tabulate x [fweight = w]`    | `s.tab(df, x, wt = w)` (see [statsalike-weights]) |
#' | `collect export t.docx`       | `export = "t.docx"` or [s.export()]        |
#' | *(addon, not in Stata)*       | `s.tab(df, x, entropy = TRUE)`             |
#' | *(addon, not in Stata)*       | `s.etable(lm(...), glm(...), lmer(...))`   |
#' | *(addon, not in Stata)*       | `s.logit(df, y, x1, x2, ame = TRUE)`       |
#'
#' All functions that analyze data take the data as their first argument (so
#' they work with pipes) and select variables with tidy evaluation
#' (unquoted). [s.etable()], [s.tests()], [s.ereturn()] and [s.export()]
#' take results instead. Results are tibbles that print in Stata's layout.
#' Once a result is processed further with dplyr, it behaves like an
#' ordinary tibble.
#'
#' @section Estimation commands and Stata 15:
#' The estimation commands ([s.regress()], [s.logit()], [s.probit()],
#' [s.mlogit()], [s.ologit()], [s.oprobit()], [s.poisson()], [s.nbreg()],
#' [s.tobit()]) and their postestimation commands ([s.margins()],
#' [s.estat()], [s.test()], [s.lincom()], [s.lrtest()], [s.predict()] and
#' others) reproduce the output of Stata 15.1 character by character,
#' including the iteration log (the same Newton-Raphson steps as Stata's
#' `ml`), the omission of collinear and perfectly predicting variables, the
#' display of factor variables and interactions and Stata's number formats.
#' The package tests compare the output with reference output written by
#' Stata 15.1. Results can differ in the last displayed digit where Stata
#' itself is imprecise (it differentiates `margins` numerically, statsalike
#' analytically) and for coefficients that are not identified (numerical
#' noise around 0). The iteration log of [s.tobit()] differs from Stata's,
#' because Stata refines its starting values with a grid search. Missing
#' statistics are shown as `.` in these tables, as in Stata.
#'
#' Three points depart from Stata on purpose (see [statsalike-stata-vs-r]).
#' Variables need no prefixes `i.` and `c.` (their type in R decides), and
#' a note below the output tells how the variables enter interactions.
#' `ame = TRUE` reports average marginal effects directly in the estimation
#' command. Tjur's R-squared is available after logit and probit models.
#'
#' @section Regression analysis, an independent implementation tested against Stata:
#' Version 0.2.0 extended the package by regression analysis (linear,
#' logistic, probit, multinomial, ordered, Poisson, negative binomial and
#' tobit regression, factor variables and interactions, `margins`,
#' predictions, tests and regression diagnostics) and by simple tests (t
#' test, analysis of variance, Cronbach's alpha). The computations and the
#' output were rebuilt from scratch in R from the published statistical
#' methods, Stata's documentation and the output Stata displays. No original
#' Stata software code was copied, translated or otherwise used, and the
#' package neither contains nor calls Stata. The same holds for the R
#' packages whose approaches were compared (e.g. sandwich, lmtest, car,
#' margins, MASS, nnet, survival).
#'
#' To test the implementation, the functions were compared automatically with
#' the original commands of Stata 15.1. The same commands were run on the same
#' data in Stata (batch mode) and in R, and the output was compared line by
#' line (about 350 commands on more than 20 datasets). 83 of these comparisons
#' are package tests with reference output written by Stata 15.1.
#'
#' Following Stata means that some results differ from those of common R
#' functions, mostly because of different defaults (see
#' [statsalike-stata-vs-r]). Time-series, panel and multilevel models and
#' Stata's survey design commands (`svy:`) are not covered.
#'
#' @section Motivation:
#' I transitioned from Stata to R years ago, but I always struggled with
#' compiling basic descriptive tables and other basic routines. To get the
#' results I was used to, I had to combine several packages. statsalike is
#' meant to change that and to support everyone who is shifting from Stata to
#' R or using both programs. The package is supposed to fill this gap and
#' provide familiar output in R.
#'
#' @section Use of AI:
#' statsalike was coded with the help of Claude, the AI model of Anthropic
#' (via Claude Code). Claude drafted large parts of the code, the
#' documentation, the tests and the example report, based on the author's
#' specification and design decisions. All AI-assisted output was reviewed,
#' tested and verified by the author, who is solely responsible for the
#' package and its correctness.
#'
#' @section Missing values:
#' Unlike Stata, the table functions [s.tabulate()], [s.tab1()], [s.tab2()]
#' and [s.table()] show missing values as categories by default
#' (`missing = TRUE`), so that the number of missing observations is always
#' visible. `missing = FALSE` excludes them, as Stata does by default. All
#' other functions exclude missing values, as in Stata.
#'
#' Missing values are displayed as R prints them, not as Stata's `.`, that
#' is `<NA>` for missing categories, `NA` for statistics that cannot be
#' computed and `NA(a)`, `NA(b)`, ... for Stata's extended missing values
#' (haven's tagged missing values). The category columns of the results of
#' [s.tabulate()] and [s.table()] contain a genuine `NA`, so `is.na()` and
#' `dplyr::filter()` work as usual.
#'
#' @section Trademark:
#' 'Stata' is a registered trademark of StataCorp LLC, College Station, TX,
#' USA. The name is used only to identify the commands whose output
#' statsalike follows. statsalike is not affiliated with, endorsed by or
#' sponsored by StataCorp. The package is an independent implementation. It
#' reproduces the functionality and output layout of these commands from
#' published statistical methods and contains no code or documentation of
#' 'Stata'.
#'
#' @keywords internal
"_PACKAGE"
