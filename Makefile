.PHONY: datasets docs checks

datasets:
	@R --quiet -f "data-raw/cancer.R"
	@R --quiet -f "data-raw/hip.R"
	@R --quiet -f "data-raw/kva.R"

docs:
	@R -e "styler::style_dir(filetype = c('r', 'rmd'))"
	@R --quiet -e "devtools::document()"
	@R --quiet -e "devtools::build_readme()"
	@R --quiet -e "pkgdown::build_site()"

checks:
	make docs
	@R -e "devtools::check()"
	@R -e "devtools::check_win_devel(quiet = TRUE)"
	@R -e "devtools::check_win_oldrelease(quiet = TRUE)"
	@R -e "devtools::check_win_release(quiet = TRUE)"
	@R -e "rhub::check_for_cran()"
