# MATH3701 Supplementary Proofs — Makefile
# Run: make all   (builds all PDFs)
#      make clean (removes auxiliary files)

PROOFS = proof_exponential_family_moments \
         proof_IRLS \
         proof_natural_spline_optimality \
         proof_smoothing_spline_solution \
         proof_partial_likelihood

PDFS = $(addsuffix .pdf, $(PROOFS))

.PHONY: all clean

all: $(PDFS)

%.pdf: %.tex proof_preamble.tex
	pdflatex -interaction=nonstopmode $<
	pdflatex -interaction=nonstopmode $<

clean:
	rm -f *.aux *.log *.out *.toc *.synctex.gz
