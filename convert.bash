#!/usr/bin/bash

INFILE=$1
ERRFILE=
LDTD=~/work/SLattes/LMLLattes.DTD
L2MODS=~/work/SLattes/lattes2mods.xsl

xmllint --schema $LATTESDTD INFILE 2>> ERRFILE  >> OUT
xsltproc $LATTESTOMODS $INFILE 2>> ERRFILE >> $MODS
xml2bib -b -w $MODS >> $BIBTEX 2>> ERRFILE



