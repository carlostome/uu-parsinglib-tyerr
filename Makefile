.PHONY: doc clean
SRC = \
	src/Text/ParserCombinators/UU/TyErr/Core.lhs    \
	src/Text/ParserCombinators/UU/TyErr/Derived.lhs \
	src/GHC/TypeErrors/PP.lhs                       \
	src/GHC/TypeErrors/Utils.lhs

LHS2TEX_DIR = lhs2TeX

doc: $(LHS2TEX_DIR)/custom.fmt $(SRC)
	LHS2TEX=$(LHS2TEX_DIR) lhs2TeX doc/doc.lhs > doc/doc.tex
	latexmk -pdf -outdir=doc -jobname=doc doc/doc.tex

clean:
	latexmk -outdir=doc -c doc/doc.tex
	rm -f doc/doc.ptb doc/doc.tex

distclean:
	latexmk -f -outdir=doc -C doc/doc.tex
	rm -f doc/doc.ptb doc/doc.tex doc/doc.pdf
