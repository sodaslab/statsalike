#' Where Stata (and statsalike) differ from common R practice
#'
#' statsalike reproduces the results of Stata. For the regression commands
#' this means that some results differ from those of common R functions,
#' although both are correct. Mostly the two programs use different defaults,
#' sometimes different conventions or algorithms. This page lists these
#' points for the estimation commands, the tests and their postestimation
#' commands, together with the settings that give the same result in R. It
#' also lists where statsalike deliberately departs from Stata. Differences in
#' the descriptive commands (weights, missing values) are described in
#' [statsalike-weights] and [statsalike].
#'
#' @section Where statsalike departs from Stata:
#' * **No prefixes i. and c.** Whether a variable enters a model as a factor
#'   or as a numeric variable follows from its type in R. Factors and
#'   character variables are categorical, numeric variables (also labeled
#'   variables from haven) are continuous unless they are named in
#'   `categorical`. Interactions are therefore written without prefixes,
#'   `"x##g"` instead of Stata's `"c.x##i.g"`. The prefixes `i.`, `c.` and
#'   `ib#.` are still understood. Output labels follow the same rule, the
#'   interaction of `wt` and the factor `am` is labeled `am#wt`, not
#'   `am#c.wt`.
#' * **Which types are numeric.** As Stata's `c.` count all numeric storage
#'   types that [s.describe()] shows: `dbl` (double), `int` (integer), `lgl`
#'   (logical, `TRUE` = 1) and numeric variables with value labels (haven's
#'   `dbl+lbl`, `int+lbl`). As Stata's `i.` count `fct` (factor), `ord`
#'   (ordered factor) and `chr` (character). Dates (`date`, class `Date`),
#'   times (`dttm`, `POSIXct`) and time differences (`drtn`, `difftime`) enter as
#'   numbers: R stores a date as days and a time as seconds since 1970-01-01,
#'   whereas Stata counts days (`%td`) and milliseconds (`%tc`) since
#'   1960-01-01. The coefficient of a date is therefore the same as in Stata,
#'   but the constant refers to another day (1970-01-01). A note below the
#'   output names the unit and the origin. For an interpretable constant,
#'   subtract a reference date first, e.g.
#'   `mutate(days = as.numeric(date - as.Date("2020-01-01")))`.
#' * **Hint for labeled numeric variables.** Variables imported from Stata or
#'   SPSS with value labels (e.g. education coded 1 to 5) are numeric in R and
#'   enter as continuous variables. If at least two labeled values occur, the
#'   estimation commands show a hint (once per variable and session) how to
#'   enter the variable as a factor (`categorical = x` or
#'   `haven::as_factor()`). Variables given with the prefix `c.` get no hint,
#'   `options(statsalike.labelled_hint = FALSE)` turns the hint off.
#' * **Notes on interactions.** Below the output of a model with
#'   interactions, and below [s.etable()], a note tells how every variable
#'   enters the interaction, e.g. "cyl#wt = cyl (factor) x wt (numeric)".
#'   Stata has no such note. `options(statsalike.stata_notation = TRUE)` brings
#'   back Stata's labels and drops the notes.
#' * **Average marginal effects in the estimation command.** `ame = TRUE` in
#'   [s.regress()], [s.logit()], [s.probit()] and the other estimation
#'   commands shows the average marginal effects of all regressors (Stata's
#'   `margins, dydx(*)`) in place of the coefficients. Stata needs a separate
#'   `margins` call. The result can be shown with [s.marginsplot()] and in
#'   [s.etable()], e.g. next to the coefficients and odds ratios of the same
#'   model.
#' * **Tjur's R-squared.** After [s.logit()] and [s.probit()], [s.ereturn()]
#'   returns Tjur's coefficient of discrimination (`r2_t`), the mean predicted
#'   probability of the cases with the outcome minus that of the cases
#'   without it. [s.etable()] shows it with `mstat = "r2_t"`. Stata does not
#'   report it. It equals `performance::r2_tjur()`.
#' * **Graphs** use the default design of ggplot2, not Stata's graph scheme.
#' * **Tables** ([s.etable()]) follow the `etable` command of Stata 17, all
#'   other output the layout of Stata 15.
#'
#' @section Specifying the model:
#' * **Categorical variables.** Factors and character variables enter the
#'   model as indicator variables with the first category as base, as with
#'   R's treatment contrasts. `base = c(x = 3)` sets another base category as
#'   `stats::relevel()` does in R.
#' * **Interactions** `"x##g"` correspond to `x * g` in an R formula, `"x#g"`
#'   to `x:g` and `"x#x"` (numeric `x`) to `I(x^2)`. As in Stata, base
#'   categories are not displayed. Combinations of categories without
#'   observations are shown as `(empty)` (R gives `NA`).
#' * **Collinear variables.** [s.regress()] omits collinear variables as
#'   Stata's `regress` does. Of two collinear variables it can drop a
#'   different one than [stats::lm()], which always sets the later variable
#'   in the formula to `NA` (of `wt` and `2 * wt` Stata drops `wt`). The
#'   maximum-likelihood commands drop the later variable, as R does. Omitted
#'   variables are listed as `(omitted)` with a note.
#' * **Perfect prediction.** If a variable predicts the outcome of a logit or
#'   probit model perfectly for some of its values, [s.logit()] and
#'   [s.probit()] drop the variable and these observations, with Stata's note
#'   (e.g. "1.rep78 != 0 predicts failure perfectly"). [stats::glm()] keeps
#'   them, warns "fitted probabilities numerically 0 or 1 occurred" and
#'   reports huge coefficients and standard errors. In the case of complete
#'   separation statsalike stops with an error.
#' * **Missing values** are excluded listwise, as with `na.omit`. The number
#'   of observations is shown in the header (`Number of obs`). Models with
#'   different variables are therefore often estimated on different samples.
#'
#' @section Estimation:
#' * **Algorithm.** The maximum-likelihood models are fitted with Newton-
#'   Raphson steps and the step rules and convergence criteria of Stata's `ml`
#'   (relative change of the coefficients at most 1e-6 or of the log
#'   likelihood at most 1e-7, and a scaled gradient below 1e-5), and the
#'   iteration log is printed. [stats::glm()] uses iteratively reweighted
#'   least squares, `MASS::polr()`, `nnet::multinom()` and
#'   `survival::survreg()` use other optimizers. The estimates agree to many
#'   digits with those of `glm()`. Those of functions that stop earlier or use
#'   a numerical Hessian can differ in the last digits.
#' * **Probit.** [s.probit()] gives the coefficients of
#'   `glm(family = binomial(link = "probit"))`. Its standard errors use the
#'   observed information, as Stata does, `glm()` the expected information.
#'   For the probit link the two differ slightly (for the logit link they are
#'   the same). [s.oprobit()] corresponds to `MASS::polr(method = "probit")`.
#' * **Base outcome of the multinomial logit.** [s.mlogit()] uses the most
#'   frequent outcome as base, as Stata does, `nnet::multinom()` the first
#'   level. Use `baseoutcome` to choose it.
#' * **Ordered models.** [s.ologit()] and [s.oprobit()] have no constant but
#'   cut points (`/cut1`, `/cut2`, ...), with P(y <= j) = F(cut_j - xb). This
#'   is the parametrization of `MASS::polr()` (`zeta`), but other R functions
#'   (e.g. some with `+ xb`) reverse the sign of the coefficients.
#' * **Poisson regression.** `exposure = t` corresponds to
#'   `offset(log(t))` in an R formula.
#' * **Negative binomial regression.** [s.nbreg()] reports the dispersion as
#'   `alpha` (and `/lnalpha`), the variance being mu + alpha * mu^2.
#'   `MASS::glm.nb()` reports `theta` = 1 / alpha. The coefficients agree, the
#'   standard errors of `glm.nb()` treat theta as known and can differ in the
#'   last digits. The likelihood-ratio test of alpha = 0 lies on the boundary
#'   of the parameter space and is therefore tested against a mixture of
#'   chi-squared distributions (`chibar2(01)`, half the usual p-value).
#' * **Tobit regression.** [s.tobit()] reports the variance of the error
#'   (`var(e.y)`, as Stata 15), `survival::survreg(dist = "gaussian")` the log
#'   of its standard deviation (`Log(scale)`). As in Stata, the tests of the
#'   coefficients are t tests with N - k degrees of freedom, `survreg()` uses
#'   z tests. Stata 15 refines its starting values with a grid search that
#'   statsalike does not reproduce. The iteration log can therefore differ and
#'   the estimates can differ in the seventh digit, where both stop.
#' * **Model test and fit.** The ML commands report the likelihood-ratio test
#'   against the model with the constant only (`LR chi2`, with robust or
#'   cluster-robust standard errors a Wald test, `Wald chi2`) and McFadden's
#'   pseudo R-squared, 1 - ll / ll_0. `summary.glm()` reports deviances
#'   instead. The LR statistic is the difference of null and residual deviance.
#' * **Weights.** Frequency weights (`fweight`) replicate observations, so N
#'   and the degrees of freedom are the sum of the weights. `lm(weights = )`
#'   treats weights as precision weights and gives wrong standard errors for
#'   frequency weights. Analytic weights (`aweight`) give the coefficients and
#'   standard errors of `lm(weights = )`. Sampling weights (`pweight`) always
#'   imply robust standard errors (see [statsalike-weights]).
#' * **Standardized coefficients** (`beta = TRUE`) use the standard deviations
#'   of the estimation sample (weighted, if weights are given).
#'
#' @section Standard errors, confidence intervals and tests:
#' * **Robust standard errors** after [s.regress()] are HC1, i.e. HC0 times
#'   N / (N - k). The default of `sandwich::vcovHC()` is HC3. Stata's result is
#'   `sandwich::vcovHC(m, type = "HC1")`. `vce = "hc2"` and `vce = "hc3"` give
#'   `type = "HC2"` and `type = "HC3"`.
#' * **Robust standard errors of the ML models** are HC0 times N / (N - 1).
#'   `sandwich::vcovHC(m, type = "HC0")` lacks this factor, `type = "HC1"`
#'   uses N / (N - k) instead.
#' * **Cluster-robust standard errors** use the factor
#'   G / (G - 1) * (N - 1) / (N - k) after regress and G / (G - 1) after the ML
#'   models (G = number of clusters). After regress these are the standard
#'   errors of `sandwich::vcovCL(m, cluster = ~ c, type = "HC1")`. After
#'   regress, t tests, confidence intervals and the F test use G - 1 degrees
#'   of freedom, not N - k as `lmtest::coeftest()` does by default (pass
#'   `df = G - 1` to get Stata's p-values). With fewer clusters than
#'   coefficients the overall F test cannot be computed and is shown as `.`.
#'   HC2 and HC3 with clusters (added in Stata 18) are not available.
#' * **Confidence intervals** of the ML models are Wald intervals, b +/- z *
#'   SE. `confint()` for a `glm()` uses the profile likelihood and gives
#'   (slightly) different, asymmetric intervals. `confint.default()` gives the
#'   Wald intervals. After regress the intervals are those of `confint.lm()`.
#' * **Odds ratios, relative risk ratios and incidence-rate ratios**
#'   (`or`, `rrr`, `irr`, `eform`) are exp(b). Their standard errors are
#'   exp(b) * SE (delta method), the confidence limits exp() of those of b, and
#'   z and p are the ones of the coefficient.
#' * **Tests of coefficients** ([s.test()]) are Wald tests, F tests with the
#'   residual degrees of freedom after regress and tobit, chi-squared tests
#'   after the other ML models. For nested linear models estimated on the same
#'   sample the F test equals the one of `anova(m1, m2)`. `anova()` for glm
#'   models gives the likelihood-ratio test, which is [s.lrtest()].
#' * **Linear combinations** ([s.lincom()]) correspond to
#'   `car::linearHypothesis()` or `multcomp::glht()`, with the t distribution
#'   after regress and the normal distribution after the ML models.
#' * **Contrasts** ([s.contrast()]) are contrasts of the margins of the
#'   linear prediction with all other factor variables weighted equally
#'   (Stata's `asbalanced`), as `emmeans::contrast()` with
#'   `emmeans(m, ~ x)`. The joint test of a factor variable equals the
#'   Type III test of `car::Anova(type = 3)` with sum-to-zero contrasts, so
#'   it differs from R's `anova()` (Type I) in models with interactions or
#'   unbalanced data. The operators correspond to R's coding schemes: `r.` to
#'   `contr.treatment`, `g.` to deviations from the grand mean
#'   (`contr.sum`), `ar.` to `MASS::contr.sdif`, `j.` (up to scaling) to
#'   `contr.helmert`, `h.` to Helmert contrasts in the other direction and
#'   `p.` to `contr.poly` (orthonormal polynomials). The p-values are not adjusted for multiple comparisons.
#' * **Likelihood-ratio test.** [s.lrtest()] refuses models estimated on
#'   different numbers of observations, as Stata does. Restrict both models to
#'   the same cases first (e.g. with `tidyr::drop_na()` on all variables of
#'   the larger model).
#'
#' @section Tests of means and scales:
#' * **t test** ([s.ttest()]). Stata assumes equal variances in the
#'   two-sample test, `t.test()` does not (`var.equal = FALSE` is R's
#'   default). Stata's test is `t.test(var.equal = TRUE)`, `unequal = TRUE`
#'   gives R's default (Satterthwaite's degrees of freedom). `welch = TRUE`
#'   uses Welch's formula for the degrees of freedom, which `t.test()` does
#'   not offer.
#' * **One-way analysis of variance** ([s.oneway()]) gives the F test of
#'   `anova(lm(y ~ g))` and Bartlett's test of `bartlett.test()`. The
#'   Bonferroni comparisons equal `pairwise.t.test(p.adjust.method =
#'   "bonferroni")` with the pooled standard deviation. Scheffé and Šidák
#'   comparisons are not in base R.
#' * **Analysis of variance** ([s.anova()]) reports partial (Type III) sums of
#'   squares, as Stata does. `anova()` in R gives sequential (Type I) sums of
#'   squares, which agree only for the last term or in balanced designs.
#'   `car::Anova(type = 3)` with sum-to-zero contrasts (`contr.sum`) gives
#'   Stata's table.
#' * **Cronbach's alpha** ([s.alpha()]) reverses items that load negatively
#'   on the first principal component of the items, as Stata does, unless
#'   `asis = TRUE`.
#'   `psych::alpha()` only warns and needs `check.keys = TRUE`. The default is
#'   the unstandardized alpha (from the covariances), `std = TRUE` gives the
#'   standardized alpha (from the correlations).
#'
#' @section Margins:
#' * **Predictive margins and average marginal effects** ([s.margins()])
#'   average over the observations of the estimation sample. The variable of
#'   interest is set to a value for everyone, the other variables keep their
#'   observed values. `ggeffects::ggpredict()` instead sets the other
#'   variables to their means and factors to their reference category
#'   (predictions for a "typical" case). Its results are therefore closer to
#'   `atmeans = TRUE`, but not identical (see the next point). The approach of
#'   [s.margins()] is the one of `marginaleffects::avg_predictions()` with
#'   `variables` (counterfactual values) and `marginaleffects::avg_slopes()`.
#'   `by` in marginaleffects instead averages within the observed groups.
#' * **`atmeans`** sets continuous variables to their means and factor
#'   variables to the shares of their categories in the sample (e.g. 0.47 for
#'   women), not to a reference category as `ggpredict()` does, nor to equal
#'   weights for all categories as `emmeans::emmeans()` does by default. As
#'   in Stata, the predictions are evaluated at the means in single precision
#'   (float), which can change the seventh digit.
#' * **`dydx` for categorical variables.** For a factor the marginal effect is
#'   the discrete change from the base category. For a numeric 0/1 variable it
#'   is the derivative. The output notes which one is shown.
#' * **Pairwise comparisons** (`pwcompare`) are the differences of all pairs
#'   of margins without adjustment for multiple comparisons, as
#'   `emmeans::pairs(adjust = "none")` (with the sign reversed, Stata
#'   subtracts the earlier category from the later one).
#' * **Standard errors** are delta-method standard errors. statsalike uses
#'   analytic derivatives, Stata numerical ones, so in rare cases the last
#'   displayed digit can differ. After regress and tobit the tests use the t
#'   distribution with the residual degrees of freedom, otherwise the normal
#'   distribution.
#' * **`at` with several variables** uses all combinations of the values,
#'   with the first variable varying slowest.
#'
#' @section Predictions and regression diagnostics:
#' * **Default of [s.predict()].** After [s.logit()] and [s.probit()] it gives
#'   the predicted probability, after [s.poisson()] and [s.nbreg()] the
#'   predicted number of events. `predict.glm()` gives the linear prediction
#'   (log odds, log count) unless `type = "response"`.
#' * **Out-of-sample predictions.** Fitted values and probabilities are
#'   computed for every observation with nonmissing regressors, also for
#'   observations outside the estimation sample (e.g. with a missing dependent
#'   variable). `predict(m)` in R only gives the estimation sample (with
#'   `na.action = na.exclude` padded with `NA`).
#' * **Breusch-Pagan test** ([s.estat()] `"hettest"`). Stata's default is the
#'   version that assumes normal errors, with the fitted values as the only
#'   variable. `lmtest::bptest()` by default uses all regressors and the
#'   studentized (Koenker) version. Stata's default corresponds to
#'   `lmtest::bptest(m, ~ yhat, data = transform(df, yhat = fitted(m)),
#'   studentize = FALSE)`. `iid = TRUE` gives the studentized version
#'   (`studentize = TRUE`), `rhs = TRUE` uses the regressors of the model
#'   (`lmtest::bptest(m, studentize = FALSE)`).
#' * **RESET test** (`"ovtest"`) uses the second to fourth powers of the
#'   fitted values. `lmtest::resettest()` by default only uses the second and
#'   third (`lmtest::resettest(m, power = 2:4)` gives Stata's test).
#' * **Specification link test** ([s.linktest()]) regresses the outcome on
#'   the prediction and its square, as Stata does. There is no direct
#'   counterpart in base R.
#' * **Variance inflation factors** (`"vif"`) are given for every coefficient,
#'   also for every category of a factor variable. `car::vif()` reports one
#'   generalized VIF (GVIF) per factor.
#' * **Information criteria** (`"ic"`) count the coefficients only. For a
#'   linear regression `AIC()` and `BIC()` in R also count the residual
#'   variance as a parameter, so R's values are larger by 2 (AIC) and by
#'   log(N) (BIC). The log likelihood is the same. For the ML models the
#'   values agree with R. The corrected criteria AICc and CAIC of Stata 18 are
#'   not available.
#' * **Hosmer-Lemeshow test** (`"gof"` with `group`). The groups are formed by
#'   the quantiles of the predicted probabilities as in Stata, with tied
#'   probabilities kept in one group. `ResourceSelection::hoslem.test()` cuts
#'   the groups differently, so the statistic can differ. Without `group` the
#'   Pearson test is computed over covariate patterns, not over observations.
#' * **Classification table** (`"classification"`). An observation counts as
#'   positive if its predicted probability is at least the cutoff (0.5).
#' * **ROC curve** ([s.lroc()]). The area under the curve equals the
#'   Mann-Whitney statistic, e.g. `pROC::auc()`.
#' * **Added-variable and component-plus-residual plots** ([s.avplots()],
#'   [s.cprplot()]) correspond to `car::avPlots()` and `car::crPlots()`. Unlike
#'   `crPlots()`, [s.cprplot()] adds a smooth line only with `lowess = TRUE`.
#' * **Normal quantile plot** ([s.qnorm()]) plots the sorted values against
#'   the normal quantiles at i / (N + 1), with the mean and standard deviation
#'   of the variable. `qqnorm()` uses `ppoints()` and `qqline()` a line through
#'   the quartiles.
#' * **Kernel density** ([s.kdensity()]) uses Stata's Epanechnikov kernel,
#'   the bandwidth 0.9 * min(SD, IQR / 1.349) * N^(-1/5) and 50 points.
#'   `density()` uses a Gaussian kernel by default.
#' * **Standardized and studentized residuals, leverage and Cook's distance**
#'   agree with `rstandard()`, `rstudent()`, `hatvalues()` and
#'   `cooks.distance()`.
#'
#' @section Display:
#' * Numbers use Stata's formats (`%9.0g` etc., without a leading zero,
#'   `.123`). p-values have three decimals, so 0.000 means p < 0.0005.
#' * Long variable names are abbreviated with `~` (`arbeitszeit~e`), long
#'   value labels end in `..`, and labels of interactions are broken over
#'   several lines, as in Stata.
#' * Statistics that cannot be computed are shown as `.`.
#'
#' @section Display formats:
#' Stata stores with every variable a *display format* that says how its
#' values are printed, and its table commands take a format for their
#' numbers (`nformat()`). statsalike uses the same notation:
#'
#' * in `nformat` of [s.table()] and [s.etable()] (Stata's `nformat()`),
#'   for all statistics (`nformat = "%9.2f"`) or per statistic
#'   (`nformat = c(mean = "%9.2f", sd = "%9.3f")`, [s.table()] only),
#' * in [s.describe()], which lists the format of every variable
#'   (`Display format`): the format stored in a Stata file (haven keeps it as
#'   the attribute `format.stata`), otherwise Stata's default, `%10.0g` for
#'   numbers (double), `%8.0g` for integers, logical variables and factors,
#'   `%td` for dates, `%tc` for times and `%-#s` (left-aligned, # characters)
#'   for strings,
#' * in `format = TRUE` of [s.summarize()], which shows the mean, standard
#'   deviation, minimum and maximum in the format of each variable, as
#'   Stata's `summarize, format`.
#'
#' A numeric format reads `%` *width* `.` *digits* *type* (`c`):
#'
#' * **Type** `f` (fixed) shows exactly *digits* decimals: `%9.2f` gives
#'   1234.57 and 0.01, `%9.0f` gives 1235 and 0.
#' * **Type** `g` (general) chooses between fixed and exponential notation.
#'   With `.0` (`%9.0g`, `%8.0g`, `%10.0g`, Stata's defaults) it shows up to
#'   seven significant digits and drops trailing zeros: 1234.568, 0.0123,
#'   42. With digits greater than zero it shows that many significant
#'   digits: `%9.3g` gives 1.23e+03 and 0.0123.
#' * **Type** `e` (exponential) shows *digits* decimals of the mantissa:
#'   `%10.3e` gives 1.235e+03.
#' * **Suffix** `c` adds thousands separators: `%12.2fc` gives 1,234.57.
#' * **Width** (the 9 in `%9.2f`) is the number of characters that Stata
#'   reserves for the column. statsalike sizes every column to its content, so
#'   the width (and the `-` for left alignment) is accepted but has no effect.
#'
#' Formats only change what is printed and exported. The results (tibbles)
#' keep the values in full precision. Any other `sprintf()` format works as
#' well, e.g. `"%.1f%%"`. Unlike Stata, the tables of [s.table()] and
#' [s.etable()] keep the leading zero of numbers between -1 and 1 (0.5, not
#' .5); the output of the estimation commands follows Stata.
#'
#' Without `nformat`, [s.table()] shows frequencies as whole numbers,
#' percentages with two decimals, proportions with four and all other
#' statistics in the general format `%9.0g`, as Stata does. [s.etable()]
#' uses `%9.3f`.
#'
#' @examples
#' m <- s.regress(mtcars, mpg, wt, hp, vce = "robust")
#' # the same standard errors in R
#' # sandwich::vcovHC(lm(mpg ~ wt + hp, mtcars), type = "HC1")
#'
#' l <- s.logit(mtcars, am, wt, nolog = TRUE)
#' d <- s.predict(mtcars, l, "p")   # probabilities, as predict(glm, type = "response")
#'
#' r <- s.regress(mtcars, mpg, wt, hp)
#' s.estat(r, "hettest")
#' # the same test in R
#' # m <- lm(mpg ~ wt + hp, mtcars)
#' # lmtest::bptest(m, ~ yhat, data = transform(mtcars, yhat = fitted(m)), studentize = FALSE)
#'
#' s.estat(r, "ic")   # AIC(lm(mpg ~ wt + hp, mtcars)) is larger by 2
#'
#' # interactions without prefixes, with a note on how the variables enter
#' d2 <- transform(mtcars, am = factor(am, labels = c("automatic", "manual")))
#' s.regress(d2, mpg, "wt##am")
#' @name statsalike-stata-vs-r
NULL
