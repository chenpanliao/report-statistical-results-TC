@ECHO ON
xelatex report-results.tex -shell-escape report-results.tex
start report-results.pdf