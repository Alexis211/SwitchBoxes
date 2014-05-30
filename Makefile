IMG=svg/boxsys-2-1.pdf_tex \
    svg/boxsys-4-6.pdf_tex \
    svg/boxsys-5-9.pdf_tex \
    svg/boxsys-6-12.pdf_tex \
    svg/boxsys-8-20.pdf_tex \
	svg/box0.pdf_tex \
	svg/box1.pdf_tex \
	svg/boxes01.pdf_tex \
	svg/intuition.pdf_tex

Article.pdf: $(IMG) Article.tex
	pdflatex Article.tex -interaction batchmode

%.pdf_tex: %.svg
	inkscape -z -D --file=$< --export-pdf=$(basename $@).pdf --export-latex
