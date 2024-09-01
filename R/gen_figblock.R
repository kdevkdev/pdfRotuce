
gen_figblock = function(fig_opts, fig_counter){

  # generate label and unique id
  fig_uind = "fig_" %+% fig_counter  %+% "_"  #%+% gsub(x = chapname, pattern = "-|-", replacement = "")
  fig_unid_latex = gsub(pattern = "_", replacement = "", x = fig_uind) # remove _ for latex
  fig_chunk_label = fig_unid_latex



  if('src' %in% names(fig_opts)){
    fig_src = paste0("../",fig_opts['src']) # add one level up because its not in the build dir but one level above
  }
  else
    stop("Figure src not provided")


  # manual label overrriade?
  if('label' %in% names(fig_opts)){
    fig_chunk_label = fig_opts['label']

    if(!grepl(x = fig_chunk_label, pattern = "^[a-zA-Z0-9]{1,}$")){
      stop(paste0("figure label '", fig_chunk_label, "' contains other chars than letters (A-z) or numbers"))
    }
  }

  # caption provided?
  fig_caption = ""
  if('caption' %in% names(fig_opts)){
    fig_caption = fig_opts['caption']

    fig_caption <-escape_caption(fig_caption)
  }

  # label provided?
  fig_label = ""
  if('label' %in% names(fig_opts)){
    fig_label = fig_opts['label'][[1]]
  }


  fig_wide = F
  if('wide' %in% fig_opts){
    fig_wide = TRUE
  }




  #cap = gsub(pattern = "%", x = cap, replacement = "\\\\\\\\%") # enable this code later to enable '%' chars

  # table captions to return
  fig_capts = list()
  # return later to compose global list
  fig_capts[paste0("cap",fig_chunk_label)] = paste0("(ref:cap", fig_chunk_label, ") ", fig_caption)



  #   ctab_chunk = "
  # ::: {.FigureMC data-latex=\"{"%+% fig_src %+% "}{" %+% fig_caption %+% "}\"}
  #
  # :::
  #
  # "
  #  ctab_chunk

  lab = ""
  if(!is.null(fig_label) & is.character(fig_label)){
    lab = paste0("\\label{", fig_label,"}")
  }

  # two different environements depending if wide is specified
  if(fig_wide == F)
  {
    # empty phamtom command to trick the lua filter
    # (that does not seem to work if there is nothing in the environment otherwise, but we used a custom latex env to transmit the params)
    "
::: {.FigureMC data-latex=\"{"%+% fig_src %+% "}{"%+% fig_caption %+%"}\"}
\\phantom{}
"%+% lab %+%"
:::"
  }
  else{
    "::: {.FigureMW data-latex=\"{"%+% fig_src %+% "}{"%+% fig_caption %+%"}\"}
\\phantom{}
"%+% lab %+%"
:::"
  }
}
