#!/bin/bash

./star.py
latex cs_notes.tex
dvips cs_notes.dvi -o
