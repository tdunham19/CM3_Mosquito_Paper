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

The main dependencies to reproduce the paper are [nextflow](https://www.nextflow.io/) and [singularity](https://www.anaconda.com/docs/getting-started/miniconda/system-requirements).  Other dependencies are handled via a [custom](./docker/README.md) docker/singularity [image](https://github.com/users/stenglein-lab/packages/container/package/aedes_isv_vt) that will be downloaded automatically by nextflow.
