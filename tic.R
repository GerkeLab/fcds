do_package_checks()

get_stage("install") %>%
  add_step(step_install_cran("ggplot2")) %>%
  add_step(step_install_cran("sf")) %>%
  add_step(step_install_cran("USAboundaries")) %>%
  add_step(step_install_cran("USAboundariesData"))


if (Sys.getenv("id_rsa") != "" && !ci()$is_tag()) {
  # pkgdown documentation can be built optionally. Other example criteria:
  # - `inherits(ci(), "TravisCI")`: Only for Travis CI
  # - `Sys.getenv("BUILD_PKGDOWN") != ""`: If the env var "BUILD_PKGDOWN" is set
  # - `Sys.getenv("TRAVIS_EVENT_TYPE") == "cron"`: Only for Travis cron jobs
  do_pkgdown(path = "docs", branch = "gh-pages")
}
