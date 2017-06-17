.PHONY: doc clean

doc:
	LHS2TEX=lhs2TeX lhs2TeX doc/doc.lhs > doc/doc.tex
	latexmk -pdf -outdir=doc -jobname=doc doc/doc.tex

clean:
	latexmk -outdir=doc -c doc/doc.tex
	rm -f doc/doc.ptb doc/doc.tex

distclean:
	latexmk -f -outdir=doc -C doc/doc.tex
	rm -f doc/doc.ptb doc/doc.tex doc/doc.pdf
