.SILENT: thesis diff
latexargs = -output-directory=temp -interaction=batchmode -file-line-error-style

mklatex: mktemp
	latexmk -pdf -auxdir=temp -outdir=temp -pdflatex=pdflatex -bibtex thesis
	# latexmk -pdf -auxdir=temp -outdir=temp -pdflatex=lualatex -bibtex thesis
	cat temp/thesis.pdf > thesis.pdf

thesis: mktemp
	latex $(latexargs) thesis || make errors
	bibtex -terse temp/A || make errors
	bibtex -terse temp/B || make errors
	latex $(latexargs) thesis || make errors
	pdflatex $(latexargs) thesis || make errors
	cat temp/thesis.pdf > thesis.pdf

diff: mktemp
	latex $(latexargs) thesis-diff || make errors
	bibtex temp/A || make errors
	bibtex temp/B || make errors
	latex $(latexargs) thesis-diff || make errors
	pdflatex $(latexargs) thesis-diff || make errors
	rm thesis-diff.tex

clean:
	test -e temp
	rm -f temp/*
	
mktemp:
	mkdir -p temp

errors:
	grep  ":[^:]*:" temp/thesis.log
	false
