@ECHO ON
xelatex -synctex=1 -interaction=nonstopmode --shell-escape report-results.tex
start report-results.pdf