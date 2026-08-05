/*
 A nextflow workflow to reproduce the paper described in this repository

 Mark Stenglein 8/3/2026 
 */

workflow {

  main:
    
    // run code that generates papers and programmatically-generated text
    data_ch = Channel.fromPath(params.qpcr_data, checkIfExists: true)
    generate_figures_and_text(data_ch)

    // run code to generate the .doc format paper 
    Channel.of( 
     tuple(
        file(params.qmd), 
        file(params.refs), 
        file(params.csl), 
        file(params.custom_doc)
     ) ).set { paper_files_ch }

    // qmd_ch       = Channel.fromPath(params.qmd, checkIfExists: true)
    edited_figures_ch = Channel.fromPath(params.edited_figures, checkIfExists: true)
    render_paper(paper_files_ch,
                 edited_figures_ch,
                 generate_figures_and_text.out.figures,
                 generate_figures_and_text.out.tables,
                 generate_figures_and_text.out.text )

  publish:
    figures  = generate_figures_and_text.out.figures
    tables   = generate_figures_and_text.out.tables
    text     = generate_figures_and_text.out.text   
    paper    = render_paper.out.paper
}

// workflow main output files 
output {
    paper {
        mode 'link'
    }
    figures {
        path 'figures'
        mode 'link'
    }
    tables {
        path 'tables'
        mode 'link'
    }
    text {
        path 'text'
        mode 'link'
    }
}

process generate_figures_and_text {

  // this custom docker/singularity container includes
  // necessary tools (R, R packages, etc)
  container 'ghcr.io/stenglein-lab/aedes_isv_vt:1.1'

  input:
    path qPCR_data

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
    Aedes_ISV_vertical_transmission_analysis.R $qPCR_data
  """
}

process render_paper {

  // this custom docker/singularity container includes
  // necessary tools (R, R packages, etc)
  container 'ghcr.io/stenglein-lab/aedes_isv_vt:1.1'

  input:
    tuple path (qmd), path (refs), path (csl), path (custom_doc)
    // including the following paths as inputs to this process 
    // will cause the corresponding directories to be available
    // as relative paths to the .qmd document
    path edited_figures
    path figures
    path tables
    path text

  output:
    path "paper.docx", emit: paper

  script:
  """
    # render quarto markdown file to docx
    quarto render $qmd --to docx -P baseDir:"$projectDir"
  """
}


