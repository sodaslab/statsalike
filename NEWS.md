# statsalike 0.2.1

* The name of the package was changed again, it is now statsalike. The
  functions and their arguments are unchanged. Code written for an earlier
  version needs `library(statsalike)` and the option names
  `options(statsalike.stata_notation = ...)` and
  `options(statsalike.labelled_hint = ...)`. The help pages are
  `?statsalike`, `?"statsalike-stata-vs-r"` and `?"statsalike-weights"`.
* README and `?statsalike` name Stata only where it identifies the
  reference commands, state that Stata is a registered trademark of
  StataCorp LLC and that the package is not affiliated with StataCorp, and
  say that the package was coded with the help of Claude (Anthropic). The
  prefix `s.` stands for statsalike commands.

# statsalike 0.2.0

## Contrasts, display formats and smaller additions

* New command `s.contrast()` re-implements Stata's `contrast` after the
  estimation commands (except mlogit): joint tests of factor variables and
  interactions (the Type III tests), the operators `r.`, `a.`, `ar.`, `g.`,
  `h.`, `j.`, `p.` and `q.`, simple effects with `@` (`"r.a@b"`) and the
  option `effects`, as contrasts of the margins of the linear prediction
  with the other factor variables balanced (`asbalanced`).
* `s.etable()` and `s.table()` gain `firstcell`, a text for the top left
  cell of the table (e.g. the dependent variable), which Stata leaves empty.
* Dates (`Date`), times (`POSIXct`) and time differences (`difftime`) can
  be regressors. They enter as numbers counted from R's origin 1970-01-01;
  a note below the output names the unit and the origin (Stata counts from
  1960-01-01, so only the constant differs).
* Numeric regressors with value labels (from Stata or SPSS) give a hint,
  once per variable and session, how to enter them as factors
  (`options(statsalike.labelled_hint = FALSE)` turns it off).
* New section "Display formats" in `?"statsalike-stata-vs-r"`: Stata's
  formats (`%9.2f`, `%9.0g`, `%12.0fc`, ...) in `nformat`, `s.describe()`
  and `s.summarize(format = TRUE)`.

## Further commands

* `s.probit()` and `s.oprobit()` re-implement Stata's `probit` and
  `oprobit`, `s.nbreg()` the negative binomial regression `nbreg` (with the
  likelihood-ratio test of alpha = 0, `exposure`, `offset` and `irr`) and
  `s.tobit()` the censored regression `tobit` (`ll`, `ul`).
* `s.ttest()`, `s.oneway()` (with `tabulate`, `bonferroni`, `scheffe` and
  `sidak`), `s.anova()` (Type III sums of squares, also with continuous
  covariates) and `s.alpha()` (Cronbach's alpha with `item`, `std`, `asis`
  and `casewise`) re-implement `ttest`, `oneway`, `anova` and `alpha`.
* New postestimation commands `s.lincom()`, `s.linktest()`, `s.lroc()`,
  `s.lsens()`, `s.avplot()`, `s.avplots()` and `s.cprplot()`.
* `s.margins()` gains `pwcompare` for pairwise comparisons of margins
  (`pwcompare` and `pwcompare(effects)` in Stata).
* `s.regress()` gains `vce = "hc2"` and `vce = "hc3"`.

## Departures from Stata and additions

* Variables no longer need Stata's prefixes `i.` and `c.`. Whether a
  variable enters as a factor or as a numeric variable follows from its
  type, interactions are written as `"x##g"`. Below the output of models with
  interactions, and below `s.etable()`, a note tells how every variable
  enters the interaction. The prefixes are still understood, and
  `options(statsalike.stata_notation = TRUE)` brings back Stata's labels.
* `ame = TRUE` in all estimation commands reports the average marginal
  effects of all regressors in place of the coefficients. `s.etable()`
  shows such models, and results of `s.margins()`, as columns of marginal
  effects, e.g. next to the coefficients and odds ratios of the same model.
* Tjur's R-squared after logit and probit models (`r2_t` in `s.ereturn()`
  and in `s.etable(mstat = )`).
* All graphs use the default design of ggplot2 instead of a Stata-like
  scheme.

## Breaking changes

* The arguments `labels` and `col_labels` of `s.etable()` are renamed to
  `rowlabels` and `collabels`.
* The Monte Carlo p-value of Fisher's exact test for large r x c tables
  (`exact = TRUE`, reported as "Fisher's exact (simulated)") no longer uses
  a fixed internal seed. It draws from R's random number generator, as
  `stats::fisher.test(simulate.p.value = TRUE)` does, so `set.seed()` before
  the call makes the result reproducible (CRAN policy).
* Labels of interactions with numeric variables no longer carry the prefix
  `c.` (`am#wt` instead of `am#c.wt`), also in the `variable` column of the
  results.
## New commands

* `s.mlogit()`, `s.ologit()` and `s.poisson()` re-implement Stata's `mlogit`
  (most frequent outcome as base, `baseoutcome`, `rrr`), `ologit` (cut
  points, `or`) and `poisson` (`exposure`, `offset`, `irr`), with weights,
  robust and cluster-robust standard errors and `group_by()`.
* `s.margins()` re-implements Stata's `margins`: predictive margins,
  average marginal effects (`dydx`), marginal effects at the means
  (`atmeans`), `at()` with all combinations, margins by factor variables and
  their interactions (`"a#b"`), `predict()` options and every outcome of
  mlogit and ologit; delta-method standard errors with analytic derivatives.
  `s.marginsplot()` draws the result (ggplot2).
* Postestimation for checking regressions: `s.predict()` (fitted values,
  probabilities, residuals, standardized and studentized residuals, Cook's
  distance, leverage, standard error of the prediction), `s.estat()`
  (`hettest`, `vif`, `ovtest`, `ic`, `gof`, `classification`), `s.test()`
  (`test`/`testparm`), `s.lrtest()`, and the plots `s.rvfplot()`,
  `s.rvpplot()`, `s.lvr2plot()`, `s.qnorm()` and `s.kdensity()`.
* Interactions and squares can be given as strings in `...` or in
  `interact`, e.g. `s.regress(df, y, "age##sex", educ, hours)`, interactions
  of factors and numeric variables, squares (`"age#age"`) and other base
  categories (`base`).

## Documentation

* New help page `?"statsalike-stata-vs-r"`: where Stata, and therefore
  statsalike, differs from common R practice (robust and cluster-robust
  standard errors, confidence intervals, predictions, margins, diagnostic
  tests, information criteria) and how to get the same results in R.
* README and `?statsalike` describe the regression extension, state that it
  is an independent implementation without any Stata code and explain the
  automated comparison with Stata 15.1.

## Output identical to Stata 15

* The estimation commands now reproduce Stata 15.1 character by character
  (checked against Stata 15.1 for about 350 commands on 23 datasets, among
  them ALLBUS 2021 and the cumulated ISSP Work Orientations, 83 of them are
  package tests with reference output written by Stata): the base category is not displayed, factor
  blocks are separated by blank lines, labels of interactions are broken as
  in Stata, the label column widens up to the console width (13 characters
  at 80 columns), column headings and number formats are those of Stata 15
  ("Coef.", "Std. Err.", "[95% Conf. Interval]"), missing statistics are `.`.
* `s.margins()` with `atmeans` evaluates at the means in single precision
  and shows them as Stata does (single precision only for variables that
  look like float variables); a long `dy/dx w.r.t.` list is wrapped as in
  Stata. `s.lrtest()` abbreviates the model names to 14 characters.
* `s.etable()`: a variable entered as a continuous variable in one model and
  as a factor variable in another now gets its own coefficient row and its
  own factor heading.
* `s.logit()` follows Stata's `ml` exactly: the same Newton-Raphson steps
  with Stata's forward stepping and step halving (identical iteration log),
  variables whose nonzero values predict the outcome perfectly are dropped
  with their observations ("note: 1.rep78 != 0 predicts failure perfectly"),
  the constant is kept when variables are collinear with it, and the
  footnote on completely determined outcomes is given.
* `s.regress()` omits collinear variables as Stata does (the one with the
  smallest scale), notes empty cells of interactions, reports
  `(sum of wgt is ...)` for analytic and sampling weights, and with fewer
  clusters than coefficients reports the model test as Stata (`F(0, 1) = .`).
* Grouped models are headed by Stata's `by` line.

# statsalike 0.1.4

## Addon beyond Stata

* `s.etable()` also accepts models estimated with R, alone or mixed with
  `s.regress()` and `s.logit()` in one table. Stata's `etable` has no
  counterpart for this.
  * `lm()` and `glm()` are read directly: Wald confidence intervals and
    model statistics (R-squared, F, root MSE, log likelihood; for binomial
    and Poisson models log likelihood, LR chi-squared and McFadden's pseudo
    R-squared) as Stata reports them.
  * All other classes are read with `parameters::model_parameters()`, the
    source of `sjPlot::tab_model()`. Checked against `tab_model()` for 38
    model types (among them `glm.nb`, `polr`, `multinom`, `clm`, `lmer`,
    `glmer`, `lme`, `gls`, `glmmTMB` with zero-inflation, `coxph`,
    `survreg`, `betareg`, `zeroinfl`, `hurdle`, `feols`, `feglm`, `plm`,
    `lm_robust`, `svyglm`, `gam`, `rq`, `ivreg`, `tobit`, `lmrob`, `rlm`,
    `vglm` and `stan_glm`): the coefficients are identical.
  * Factors and logical regressors are shown as categorical variables;
    further equations (zero-inflation, cut points, outcomes of multinomial
    models) get their own block, addressed in `labels` as
    `"equation::term"`.
* New model statistics `r2_n` (Nagelkerke), `r2_m` and `r2_c` (marginal
  and conditional R-squared of mixed models) and the argument `level` for
  the confidence intervals of models estimated with R.
* 'parameters', 'insight' and 'performance' are suggested packages; they are
  only needed for models other than `lm()` and `glm()`.

## Other changes

* `s.regress()` stores the log likelihood (Stata's `e(ll)`; not for
  importance and sampling weights), shown by `s.ereturn()` and by
  `s.etable(..., mstat = "ll")`.

## Bug fixes

* The notes of `s.etable()` only mention standard errors and stars if the
  table shows them (Bayesian models have neither).
* Long column headings of `s.etable()` (e.g. variable labels of survey
  data) are wrapped to the width of their column instead of being cut off
  and splitting the table into blocks.
* `s.etable()` takes the variable labels of factors from the data when the
  model frame has lost them (`lm()` and `glm()` drop unused factor levels,
  e.g. the labels of missing-value codes after `haven::as_factor()`).
* The coefficient tables of `s.regress()` and `s.logit()` are 78 characters
  wide, as in Stata, so that they end flush with the header above (they
  were two characters narrower; the header of `s.logit()` one character
  wider). The t/z and p columns have Stata's widths.
* The `Kapitel*` course folders inside the source folder are excluded from
  the package build (`.Rbuildignore`); the tests' and examples' optional
  packages are listed in `Suggests`.

# statsalike 0.1.3

* New `s.etable()` re-implements Stata's `etable`: several `s.regress()` and
  `s.logit()` models side by side, with coefficients, significance stars and
  standard errors in parentheses (or confidence intervals, t/z statistics or
  p-values) and model statistics such as N and R-squared. Row labels come
  from the variable labels or are set freely with `labels`, including
  spaces, without renaming variables (like `pred.labels` in
  `sjPlot::tab_model()`); column headings come from the argument names or
  `col_labels`. Supports `keep`, `drop`, `showbase`, `eform`, own notes
  below the table (`notes`) and export to HTML and Word.
* `s.regress()` and `s.logit()` store the variable labels of the model
  variables for `s.etable()`.

# statsalike 0.1.2

## Breaking change

* The table functions `s.tabulate()`/`s.tab()`, `s.tab1()`, `s.tab2()` and
  `s.table()` now show missing values as categories by default
  (`missing = TRUE`), so that the number of missing observations is always
  visible. This deviates from Stata, which excludes missing values unless the
  option `missing` is given. Use `missing = FALSE` to get the previous
  behaviour and Stata's default. As with Stata's `missing` option, missing
  categories count towards percentages, totals and test statistics; for
  results based on valid observations only, set `missing = FALSE`.
* `s.tabgen()` keeps its default `missing = FALSE`: indicator variables are
  `NA` for observations with a missing value, as before.

# statsalike 0.1.1

* New `s.pwcorr()` re-implements Stata's `pwcorr` (pairwise correlations)
  with the options `obs`, `sig`, `star()`, `print()`, `bonferroni`, `sidak`
  and `listwise`, frequency and analytic weights and `group_by()`.
* New help page `?statsalike-weights` explains the four weight types
  (`fweight`, `aweight`, `iweight`, `pweight`), which commands accept them
  and how to use survey weights.

# statsalike 0.1.0

* First version (not released on CRAN).

## Functions

* `s.describe()` re-implements Stata's `describe` with the options `simple`,
  `short`, `fullnames` and `numbers`; the result corresponds to
  `describe, replace`.
* `s.tabulate()` (short form `s.tab()`) re-implements `tabulate` for one-way
  and two-way tables, including `summarize()`, all cell options (`row`,
  `column`, `cell`, `expected`, `cchi2`, `clrchi2`) and test statistics
  (`chi2`, `lrchi2`, `exact`, `gamma`, `taub`, `V`, `all`).
* `s.tab1()` and `s.tab2()` re-implement `tab1` and `tab2`.
* `s.tabgen()` creates indicator variables like `tabulate, generate()`.
* `s.table()` re-implements `table` in the syntax of Stata 17: nested row,
  column and table variables, totals and subtotals, and 27 statistics plus
  the percentiles `p1`-`p99`, with weighted quantiles that follow Stata's
  definition.
* `s.tests()` returns the test statistics of a two-way table as a tibble.
* `s.summarize()` (short form `s.sum()`) re-implements `summarize` with the
  options `detail`, `format` and `separator()`, including weights.
* `s.codebook()` re-implements `codebook` with the options `compact`,
  `header`, `tabulate()` and `problems`.
* `s.regress()` re-implements `regress` with `vce(robust)`,
  `vce(cluster)`, `level()`, `beta`, `noconstant`, weights including
  `pweight`, and categorical regressors (`i.var`).
* `s.logit()` re-implements `logit` with iteration log, `or`, `nolog`,
  robust and cluster-robust standard errors.
* `s.ereturn()` returns the model statistics of `s.regress()` and
  `s.logit()`, like `ereturn list`.
* Addon beyond Stata: `entropy = TRUE` in `s.tab()` and `s.tab1()` reports
  the absolute (Shannon) and relative entropy of one-way distributions.
* `s.export()` and the `export` argument of all table functions write the
  output to HTML or Word (DOCX) files.

## Behaviour

* Grouped data (`dplyr::group_by()`) works like Stata's `by` prefix.
* Frequency, analytic and importance weights (`fweight`, `aweight`,
  `iweight`) are supported as in Stata.
* Missing values follow Stata: `NA`, empty strings, extended missing values
  (`.a`-`.z`) and SPSS user-defined missing values imported with 'haven' are
  excluded unless `missing = TRUE`. They are displayed as in R (`<NA>`,
  `NA`, `NA(a)`), and the category columns of the results contain a genuine
  `NA`.
* Output follows Stata's display rules, e.g. `%9.0g` numbers, notes on
  collinear terms and the position of factor levels in coefficient tables.
  `s.regress()` and `s.logit()` were checked against published Stata output
  and against 'estimatr', 'sandwich' and 'survey'.
* Results are tibbles in tidy format that print in Stata's layout and become
  ordinary tibbles when processed with dplyr.
