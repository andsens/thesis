thesis:
	latex thesis.tex
	bibtex A
	bibtex B
	latex thesis.tex
	dvips -t letter -o thesis.ps thesis.dvi
	ps2pdf14 -dPDFSETTINGS=/prepress -dEmbedAllFonts=true thesis.ps thesis.pdf
	make clean

diff:
	latex thesis-diff.tex
	bibtex A
	bibtex B
	latex thesis-diff.tex
	dvips -t letter -o thesis-diff.ps thesis-diff.dvi
	ps2pdf14 -dPDFSETTINGS=/prepress -dEmbedAllFonts=true thesis-diff.ps thesis-diff.pdf
	rm thesis-diff.tex
	make clean

latex:
	latex thesis
	bibtex thesis
	latex thesis && (pkill -USR1 xdvi\* || echo "No xdvi!")

xdvi:
	xdvi -background white thesis.dvi -s 5 -sidemargin 0.0cm -topmargin 1.0cm -hush -expertmode 0 -geometry 750x987+0+0 >& /dev/null &

clean:
	rm *.aux
	rm *.bbl
	rm *.blg
	rm *.dvi
	rm *.log
	rm *.toc
	rm *.ps