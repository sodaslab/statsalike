## New submission (package name changed)

This package was submitted to CRAN before under a different name (first
submission 0.1.0, resubmission 0.2.0 after the review of 2026-11 by Leonore
Hochhauser). I have changed the name to 'statsalike' so that the package
name no longer contains the trademark 'Stata'. The package was never
published on CRAN under the earlier name, so no users or reverse
dependencies are affected.

Version 0.2.1 is identical to the resubmitted version 0.2.0 apart from the
name (package name, help topics statsalike, statsalike-stata-vs-r and
statsalike-weights, and the option names statsalike.*). All changes made in
response to the review are kept:

* single quotes in Title and Description only around software and package
  names ('Stata', 'dplyr', 'haven', 'tibble', 'Word'). Command names, weight
  types, the file format and the company name are unquoted, function names
  are written with parentheses (group_by()).
* the Title has 63 characters and no longer names 'tidyverse'.
* the weight types are described in plain words.
* executable examples for all exported functions, no \donttest{}.
* the package sets no seed and does not touch `.Random.seed`.

## R CMD check results

0 errors | 0 warnings | 1 note

* Local: Windows 11 x64, R 4.6.2 (current release), tarball built with
  R 4.6.2, `R CMD check --as-cran` including the PDF manual.
* win-builder, R-devel and R-release: see the results of this submission.
* The NOTE is "New submission" and "Possibly misspelled words in
  DESCRIPTION". The flagged words are the names of the 'Stata' commands the
  package reproduces (anova, avplot, codebook, cprplot, estat, etable,
  lincom, linktest, lroc, lrtest, lsens, marginsplot, mlogit, nbreg, ologit,
  oneway, oprobit, poisson, pwcorr, tobit, ttest), the file format DOCX and
  the company name StataCorp LLC. They are spelled correctly.

## Notes for the reviewer

* 'Stata' is a registered trademark of StataCorp LLC. The package is not
  affiliated with or endorsed by StataCorp. It is an independent
  implementation in R of the output of 'Stata' commands and contains no
  'Stata' code or documentation. The trademark notice is included in the
  Description, the package help page and the README.
* For large r x c tables, `s.tabulate(..., exact = TRUE)` falls back to a
  Monte Carlo p-value. The package sets no seed. The simulation draws from
  the user's random number stream, as `stats::fisher.test()` does, and the
  documentation points to `set.seed()` for reproducible results.
* Examples and tests write files only to `tempdir()`.
* Output is printed with cat() only inside print methods.

## Downstream dependencies

There are currently no downstream dependencies for this package.
