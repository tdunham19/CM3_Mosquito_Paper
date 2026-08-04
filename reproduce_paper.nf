/*
 A nextflow workflow to reproduce the paper described in this repository

 Mark Stenglein 8/3/2026 
 */

workflow {

  main:
    
    // run code that generates papers and programmatically-generated text
    generate_figures_and_text()

    // run code to generate the .doc format paper 
    qmd_ch = Channel.fromPath(params.qmd, checkIfExists: true)
    reproduce_paper(qmd_ch, 
                    generate_figures_and_text.out.figures,
                    generate_figures_and_text.out.tables,
                    generate_figures_and_text.out.text )

  publish:
    paper    = reproduce_paper.out.paper
    figures  = generate_figures_and_text.out.figures
    tables   = generate_figures_and_text.out.tables
    text     = generate_figures_and_text.out.text   
}

// workflow main output files 
output {
    paper {
        path 'paper'
        mode 'link'
    }
    figures {
        path 'analysis_output/figures'
        mode 'link'
    }
    tables {
        path 'analysis_output/tables'
        mode 'link'
    }
    text {
        path 'analysis_output/text'
        mode 'link'
    }
}

process generate_figures_and_text {

  // this custom docker/singularity container includes
  // necessary tools (R, R packages, etc)
  container 'ghcr.io/stenglein-lab/aedes_isv_vt:1.0'

  output:
    path "figures", emit: figures
    path "tables",  emit: tables
    path "text",    emit: text

  script:
  """
    # run R script that does main analysis heavy-lifting 
    # this script is in the bin dir so nextflow will be able to find it
    # and it has executable permissions so can be run 
    # it has a Rscript shebang
    Aedes_ISV_vertical_transmission_analysis.R
  """
}

process reproduce_paper {

  // this custom docker/singularity container includes
  // necessary tools (R, R packages, etc)
  container 'ghcr.io/stenglein-lab/aedes_isv_vt:1.0'

  input:
    path qmd
    // including the following paths as inputs to this process 
    // will cause the corresponding directories to be available
    // as relative paths to the .qmd document
    path figures
    path tables
    path text

  output:
    path "paper.docx", emit: paper

  script:
  """
    # render quarto markdown file to docx
    quarto render $qmd --to docx
  """
}


