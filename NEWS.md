## fcds 0.1.1

* Add option `discard_unseen_levels` to `count_fcds()` that allows the user to
  control whether unobserved factor levels are dropped from the returned data
  (`TRUE`/`FALSE`) or to provide a list of columns where unobserved factor
  levels should be dropped.

## fcds 0.1.0

* Updated the FCDS import recoding specification for the 2019 FCDS release.

* Added `fcds_recoding()` to access and specify previous versions of the FCDS
  recoding specification.

* Added `year_group_levels` to `complete_year_groups()` to allow for custom
  year groups. Mostly for testing because the year groups change based on the
  FCDS release year.
  
* Updated built-in population and example data
