.PHONY: datasets

datasets:
	@R --quiet -f "data-raw/cancer.R"
	@R --quiet -f "data-raw/hip.R"
	@R --quiet -f "data-raw/kva.R"
