.PHONY: datasets docs checks style

datasets:
	@R --quiet -f "data-raw/cancer2.R"
	@R --quiet -f "data-raw/hip.R"
	@R --quiet -f "data-raw/kva.R"

style:
	@R -e "styler::style_dir(filetype = c('r', 'rmd'))"

docs:
	make style
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
