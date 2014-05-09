IMG=svg/2wires_box.pdf_tex \
    svg/4wires_box.pdf_tex \
    svg/8wires_box.pdf_tex

Article.pdf: $(IMG) Article.tex
	pdflatex Article.tex -interaction batchmode

%.pdf_tex: %.svg
	inkscape -z -D --file=$< --export-pdf=$(basename $@).pdf --export-latex
	
