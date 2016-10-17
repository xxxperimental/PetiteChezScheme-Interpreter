#!/bin/bash
pdflatex timelog-m.tex
rm timelog-m.aux
rm timelog-m.log

pdflatex timelog-w.tex
rm timelog-w.aux
rm timelog-w.log

while getopts "o" OPT
do
	case $OPT in
		o)
			open timelog-m.pdf
			open timelog-w.pdf;;
	esac
done
