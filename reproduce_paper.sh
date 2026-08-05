#!/usr/bin/env bash

# this is an entry point script to reproduce the paper: 
# Biparental vertical transmission of *Aedes* anphevirus, Guadeloupe mosquito virus, and verdadero virus in colonized *Aedes aegypti*
# by Tillie Dunham, Karla Saavedra-Rodriguez, Brian Foy, Christie Mayo, and Mark Stenglein
#
# Mark Stenglein, Aug 2026

# create a new directory in the pwd to provide a 
# location in which to build singularity image 
# (presumably with ample storage)
mkdir -p $HOME/.cache/singularity
export SINGULARITY_TMPDIR=$HOME/.cache/singularity

nextflow run -resume reproduce_paper.nf -output-dir workflow_output
