thesis:
	latexmk -silent -pdflatex='pdflatex -shell-escape -synctex=1 %O %S' -pdf thesis

clean:
	rm -f \
		*.aux *.toc *.out *.pdf *.auxlock \
		*-figure*.dpth \
		*.fdb_latexmk *.fls *.log \
		*.synctex.gz *.synctex.gz\(busy\) \
		*.brf *-blx.bib *.bcf *.blg *.bbl *.run.xml
