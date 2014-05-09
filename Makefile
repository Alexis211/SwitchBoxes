IMG=svg/boxsys-2-1.pdf_tex \
    svg/boxsys-4-6.pdf_tex

Article.pdf: $(IMG) Article.tex
	pdflatex Article.tex -interaction batchmode

%.pdf_tex: %.svg
	inkscape -z -D --file=$< --export-pdf=$(basename $@).pdf --export-latex
