#' Weights in statsalike
#'
#' @description
#' Like Stata, statsalike distinguishes four types of weights. A weighted
#' command gets the weighting variable with `wt =` (Stata's `[weight = w]`)
#' and its type with `weight_type =`, e.g.
#' `s.tab(df, x, wt = w, weight_type = "aweight")` for Stata's
#' `tabulate x [aweight = w]`. Without `weight_type`, frequency weights
#' (`"fweight"`) are assumed, as in Stata.
#'
#' In all types an observation with weight 2 counts twice as much as an
#' observation with weight 1, so weighted percentages, means and
#' coefficients are the same for every type that a command allows. The types
#' differ in what a weight *means*. This determines the reported number of
#' observations and frequencies and whether test statistics and standard
#' errors can be computed.
#'
#' @section Overview:
#'
#' | Type | `weight_type =` | Meaning of the weight | Number of observations | Test statistics | Typical use |
#' |---|---|---|---|---|---|
#' | Frequency weights | `"fweight"` (default) | Whole number: how many identical observations a row of the data stands for. Non-integer weights are an error. | Sum of the weights | Allowed; computed as if each row were repeated `w` times | Aggregated data, e.g. one row "industry, works council, 25 firms" |
#' | Analytic weights | `"aweight"` | Relative weight; rescaled internally to sum to the number of observations. Originally the inverse of the variance of an observation (e.g. a mean over `w` units). | Number of observations (rows) | Not in tables (as in Stata); allowed in [s.pwcorr()] and [s.regress()] | Survey weights in descriptive tables, e.g. post-stratification or design weights around 1 |
#' | Importance weights | `"iweight"` | Used as given, without interpretation; negative weights are allowed. | Sum of the weights | Not in tables; allowed in [s.regress()] and [s.logit()] | Grossing up to the population, when the weight is the number of persons an observation represents |
#' | Sampling weights | `"pweight"` | Inverse of the probability that an observation was selected into the sample. | Number of observations | Robust (sandwich) standard errors are implied | Regression models with survey data |
#'
#' @section Which commands accept which type:
#'
#' | Command | `fweight` | `aweight` | `iweight` | `pweight` |
#' |---|---|---|---|---|
#' | [s.tabulate()], [s.tab1()], [s.tab2()] | yes | yes | yes | no |
#' | [s.summarize()] | yes | yes | yes (not with `detail`) | no |
#' | [s.table()] | yes | yes | yes | no |
#' | [s.pwcorr()] | yes | yes | no | no |
#' | [s.regress()] | yes | yes | yes | yes |
#' | [s.logit()] | yes | no | yes | yes |
#'
#' In [s.tabulate()], test statistics (`chi2`, `lrchi2`, `exact`, `gamma`,
#' `taub`, `V`) are only computed without weights or with frequency weights.
#' With other weights the command stops with an error, as Stata's
#' `tabulate` does.
#'
#' @section Survey weights:
#' Population surveys such as the ALLBUS, the ESS or the SOEP provide weights
#' that correct for unequal selection probabilities (e.g. the oversampling of
#' East Germany in the ALLBUS) or for deviations from official statistics
#' (post-stratification). These weights have decimals and vary around 1.
#' Which of several weighting variables fits depends on the research
#' question (statements about persons or households, about the whole country
#' or regions) and is explained in the documentation of each survey.
#'
#' * For **descriptive tables** (percentages, means) use
#'   `weight_type = "aweight"`. The number of observations stays the number
#'   of respondents, only the observations count differently.
#'   `weight_type = "iweight"` gives the same percentages and means but shows
#'   the sum of the weights as frequencies.
#' * **Tests** such as Pearson's chi-squared assume that each counted case is
#'   an observed unit. Weighted frequencies ("617.10 men") do not meet this
#'   assumption, and a test computed from them has wrong p-values. As in
#'   Stata, [s.tabulate()] therefore refuses tests with analytic and
#'   importance weights. Usually the weighted percentages are reported
#'   together with the tests of the unweighted table. Design-based tests
#'   (e.g. the Rao-Scott correction in `survey::svychisq()`) are beyond the
#'   scope of statsalike.
#' * For **regression models** with survey data use
#'   `weight_type = "pweight"` in [s.regress()] or [s.logit()], which gives
#'   robust standard errors.
#'
#' @section Missing and zero weights:
#' Observations with a missing or zero weight are excluded from all
#' computations. Negative weights are only allowed as importance weights.
#'
#' @examples
#' df <- data.frame(x = c("a", "a", "b", "b"), w = c(1.2, 0.8, 0.5, 1.5))
#'
#' # survey weight: same N, weighted percentages
#' s.tab(df, x, wt = w, weight_type = "aweight")
#'
#' # importance weights: same percentages, frequencies = sum of the weights
#' s.tab(df, x, wt = w, weight_type = "iweight")
#'
#' # aggregated data: one row per category with its frequency
#' agg <- data.frame(x = c("a", "b"), n = c(25, 10))
#' s.tab(agg, x, wt = n)
#'
#' # regression with sampling weights (robust standard errors)
#' s.regress(mtcars, mpg, hp, wt = qsec, weight_type = "pweight")
#' @name statsalike-weights
NULL
