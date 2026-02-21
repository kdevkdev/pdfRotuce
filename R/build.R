#' Title
#'
#' @param src
#'
#' @return
#' @export
#'
#' @examples
build <- function(folder, src = "manuscript.docx", meta_csv = "metadata.csv", render_only = F, outformats = c("html", "pdf", "jats"), ...){

  # get basename without file extension
  basename = tools::file_path_sans_ext(basename(src))

  # create directories
  if(endsWith(folder,"/") || endsWith(folder,"\\")) # remove trailing slash
    folder <- substr(folder,1, nchar(folder)-1)

  doc_file       = paste0(folder,"/", src)
  metacsv_path   = paste0(folder,"/", meta_csv)
  build_path     = paste0(folder, "/build")
  out_path       = paste0(folder, "/out")
  rmd_outfile    = paste0(build_path, "/", basename, ".Rmd")
  xml_outpathO   = paste0(out_path, "/", "jatsxml_", basename, "/")
  pdf_outfile    = paste0(build_path, "/", basename, ".pdf")
  pdf_outfileO   = paste0(out_path, "/", basename, ".pdf")
  html_outfile   = paste0(build_path, "/", basename, ".html")
  html_outfileO  = paste0(out_path, "/", basename, ".html")
  rmd_outfileO   = paste0(out_path, "/", basename, ".Rmd")
  bib_outfileO   = paste0(out_path, "/", "references.bib")

  # create directory if fokder does not exist yet
  if(!dir.exists(out_path)) dir.create(out_path)
  else if(file.exists(out_path) && !dir.exists(out_path)) stop(paste0("file called '", out_path, "' exists at the specified location for output path but is not a directory!"))

  # check if we can find the manuscript
  stopifnot("doc manuscript file not found at specified location" = file.exists(doc_file))

  # if xml not wished
  if(!("jats" %in% outformats)) xml_outpathO = NULL

  # check if we should markdownify
  if(render_only !=T){

    # firsmarkdownify and write to file
    markdownify( src_docx = doc_file, doc_folder = folder, working_folder = build_path, meta_csv = metacsv_path, rmd_outfile = rmd_outfile, xml_outpath = xml_outpathO, ...)
  }


  # if successfull in writing file
  if(file.exists(rmd_outfile))
  {
    # save old option and set new
    bckp_tinytexclean <- options(tinytex.clean = FALSE)
    # try to rendr
    # put all rmd, latex related files in build path and manually copy, as far as i understand rmarkdown::render does not allow to specify output_file and output_dir that do not correspond to each other
    # only render if pdf included in outformats
    respath_pdf = NULL
    if("pdf" %in% outformats){
      respath_pdf  = rmarkdown::render(input = rmd_outfile, output_dir = build_path, output_format = "bookdown::pdf_document2",  clean = F, output_options = list("keep_md" = T, "keep_tex" = T))
    }
    #respath_html = rmarkdown::render(input = rmd_outfile, output_dir = build_path, output_format = "bookdown::html_document2", clean = F, output_options = list("keep_md" = T, "keep_tex" = T))

    # for some reason the custom template provided in the yaml header of manuscript.Rdm does not work for thehtml_document2,hence specify explicitly heare
    # only render if html included in outformats
    respath_html = NULL
    if("html" %in% outformats){
      bookdown_frmt = bookdown::html_document2(template = "template.html", clean = F, toc = TRUE, toc_depth = 3,
                          output_options = list("keep_md" = T, "keep_tex" = T), global_numbering = TRUE)

      #bookdown_frmt$pandoc$args = c(bookdown_frmt$pandoc$args, "--table-of-contents", "--toc-depth", "3")
      respath_html = rmarkdown::render(input = rmd_outfile, output_dir = build_path, output_format = bookdown_frmt)
    }

    if(!is.null(respath_pdf) &&  file.exists(pdf_outfile) && "pdf" %in% outformats){

      file.copy(from = pdf_outfile, to = pdf_outfileO, overwrite = T)
      #file.copy(from = rmd_outfile, to = rmd_outfileO, overwrite = T) # no longer do this -> depends on tabchnk code
      file.copy(from = paste0(build_path, "/", "references.bib"), to = bib_outfileO, overwrite = T)


      hgl_note("Copied requested PDF output file successfully to ouput dir")

    } else{
      hgl_warn("No PDF output file found, could not copy to out directory")
    }


    if(!is.null(respath_html) &&  file.exists(html_outfile) && "html" %in% outformats){

      file.copy(from = html_outfile, to = html_outfileO, overwrite = T)
      hgl_note("Copied requested HTML output file successfully to ouput dir")

    } else{
      hgl_warn("No HTML output file found, could not copy to out directory")
    }



    # restore tinytex.clean option
    options(tinytex.clean = bckp_tinytexclean)
  }
}
