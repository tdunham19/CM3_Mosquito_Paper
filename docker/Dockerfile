# ---------------------------------------------------------------------------
#
# This dockerfile defines a docker/singularity image needed to reproduce
# the paper described in this repository.
#
# It handles software dependencies, namely:
#
# - R
# - R packages, like tidyverse, patchwork, etc
# - quarto
# 
# In the interest of reproducibility, it uses:
# - Pinned R version via rocker/r-ver
# - Pinned package versions via a fixed Posit Package Manager (PPM) CRAN
#   snapshot date, installed as fast pre-compiled binaries 
# - Quarto CLI installed directly 
#
# To build for linux/amd64:
#   docker buildx build --platform linux/amd64 -t stenglein-lab/aedes_isv_vt:1.0 .
# ---------------------------------------------------------------------------

# the base rocker R image
FROM rocker/r-ver:4.5.0

# --- Pin the CRAN snapshot date for full package-version reproducibility ---
# Every install.packages() call below will resolve against packages exactly
# as they existed on this date. Change this date deliberately when you want
# to bump versions; otherwise builds stay reproducible indefinitely.
ENV CRAN_SNAPSHOT_DATE=2026-08-01
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/noble/${CRAN_SNAPSHOT_DATE}'))" \
    >> /usr/local/lib/R/etc/Rprofile.site

# --- System dependencies commonly needed by tidyverse / plotting packages ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libuv1-dev \
    pandoc \
    curl \
    gdebi-core \
    && rm -rf /var/lib/apt/lists/*

# --- Install Quarto CLI ---
# Check https://quarto.org/docs/get-started/ for the current release version
ARG QUARTO_VERSION=1.10.18
RUN curl -LO https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && gdebi --non-interactive quarto-${QUARTO_VERSION}-linux-amd64.deb \
    && rm quarto-${QUARTO_VERSION}-linux-amd64.deb

# --- Install R packages (pinned by the CRAN_SNAPSHOT_DATE set above) ---
RUN Rscript -e "install.packages(c( \
    'tidyverse', \
    'readxl', \
    'binom', \
    'patchwork', \
    'lme4', \
    'broom.helpers', \
    'broom.mixed', \
    'fs', \
    'gtsummary', \
    'sjPlot', \
    'flextable', \
    'knitr', \
    'ggpubr', \
    'rstatix' \
    ))"

# --- Sanity check that everything loads at build time (fail fast) ---
RUN Rscript -e "library(tidyverse); library(readxl); library(binom); \
    library(patchwork); library(lme4); library(broom.helpers); library(broom.mixed); library(fs); library(gtsummary); library(sjPlot); \
    library(flextable); library(knitr); library(ggpubr); library(rstatix); \
    cat('All packages loaded OK\n')"

WORKDIR /workflow
CMD ["R"]
