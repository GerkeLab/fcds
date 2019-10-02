## fcds 0.1.4

* Added `county_name` argument to `count_fcds()` that adds the county into the
  count groups. This value can be a vector of counties, or `TRUE` to include all
  counties in the source data. Alternatively, if the value is `"moffitt"` then
  the counts are filtered to counties in Moffitt's catchment area.

## fcds 0.1.2

* Add ICD-O-3 codes from [IACR](http://www.iacr.com.fr/index.php?option=com_content&view=category&layout=blog&id=100&Itemid=577)
  because the SEER ICD-O-3 codes are not complete (but are likely the most common).

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
