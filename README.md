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

### Dependencies

The main dependencies to reproduce the paper are [nextflow](https://www.nextflow.io/) and [singularity](https://docs.sylabs.io/guides/latest/user-guide/#). Nextflow and Singularity must be installed on your system to reproduce the paper.  Other dependencies are handled via a [custom](./docker/README.md) docker/singularity [image](https://github.com/users/stenglein-lab/packages/container/package/aedes_isv_vt) that will be downloaded automatically by nextflow.

### Figures and Edited Figures

The R code provided in this repository generates the main figures contained in the paper's results. These fgures were subsequently edited using [Affinity Designer](https://www.affinity.studio/) to add additional labels. The edited versions of the figures are provided in the edited_figures directory of this repository.
