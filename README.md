# Biparental vertical transmission of *Aedes* anphevirus, Guadeloupe mosquito virus, and verdadero virus in colonized *Aedes aegypti*

This repository contains code and data necessary to reproduce the paper **Biparental vertical transmission of *Aedes* anphevirus, Guadeloupe mosquito virus, and verdadero virus in colonized *Aedes aegypti*** by Tillie Dunham, Karla Saavedra-Rodriguez, Brian Foy, Christie Mayo, and Mark Stenglein

This paper was implemented as a [quarto markdown document](https://quarto.org/) that uses R scripts to generate paper text and figures.  

This paper has been [deposited as a preprint](https://www.biorxiv.org/content/10.64898/2026.07.20.739548v1) upon initial submission for review.

## To reproduce the paper

To recreate the paper document, [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) [this repository](https://github.com/tdunham19/CM3_Mosquito_Paper.git), change into the repository directory, and run the `reproduce_paper.sh` script:

```
# clone
git clone https://github.com/tdunham19/CM3_Mosquito_Paper.git

# change directory
cd CM3_Mosquito_Paper

# run entry script to reproduce paper
./reproduce_paper.sh
```

Alternatively, you can reproduce the paper by running the nextflow workflow [from github](https://docs.seqera.io/nextflow/sharing):
```
nextflow run -resume tdunham19/CM3_Mosquito_Paper -output-dir workflow_output 
```

### How does this all work?

[reproduce_paper.sh](./reproduce_paper.sh) just runs a nextflow pipeline defined in [main.nf](./main.nf).  This pipeline does a couple main things:

1. The workflow first calls [an R script](./bin/Aedes_ISV_vertical_transmission_analysis.R) that performs analyses using input data contained in this repository. This script wrangles and analyzes data, performs modeling, generates figures and tables, and outputs paragraphs of text for the paper as markdown-format text files.  
2. The workflow then  renders the paper using quarto. The [quarto-markdown format paper document](./paper/paper.qmd) includes some sections of the paper text and it also includes figures and programmatically generated text.  References are provided in a seperate [bibtex format file](./paper/references.bib).
3. Programmatically generated text, figures, and tables, as well as the rendered paper (word document) are output to a workflow_output directory.

#### A note on figures and edited figures

The R code provided in this repository generates the main figures contained in the paper's results. In many cases, these fgures were not used directly but were subsequently edited using [Affinity Designer](https://www.affinity.studio/) to add additional labels. The edited versions of the figures are provided in the [edited_figures directory](./edited_figures/) of this repository. It would be ideal to use programmatically generated figures directly but it can be difficult to have R output figures exactly as you want them to appear in the final paper (e.g. you might want more complicated labeling than is straightforward to implement in ggplot2).  Other figures were not generated in R, for instance the [experimental workflow supplemental figure](./edited_figures/Rearing_schedule_and_lifecycle.png), which was created in BioRender. These are all just included in the quarto paper document as PDF or PNG (etc) images.

### Dependencies

The main dependencies required to reproduce the paper are [nextflow](https://www.nextflow.io/) and [singularity](https://docs.sylabs.io/guides/latest/user-guide/#). **Nextflow and Singularity must be installed on your system to reproduce the paper.**  Other dependencies are handled via a [custom](./docker/README.md) docker/singularity [image](https://github.com/users/stenglein-lab/packages/container/package/aedes_isv_vt) that will be downloaded automatically by nextflow.

