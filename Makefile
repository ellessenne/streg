.PHONY: datasets docs all

all:
	make docs

datasets:
	@R --quiet -f "data-raw/cancer.R"
	@R --quiet -f "data-raw/hip.R"
	@R --quiet -f "data-raw/kva.R"

docs:
	@R --quiet -e "devtools::document()"
	@R --quiet -e "devtools::build_readme()"
	@R --quiet -e "pkgdown::build_site()"
