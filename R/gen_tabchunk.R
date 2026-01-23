escape_caption <- function(str){
  str = stringr::str_replace_all(str, pattern = "\\@", replacement = "\\\\\\\\@")
  str = stringr::str_replace_all(str, pattern = "\\%", replacement = "\\\\\\\\%")
  str = stringr::str_replace_all(str, pattern = "\\$", replacement = "\\\\\\\\$")
  str
}
#' Title
#'
#' @param ct_csv
#' @param tab_opts
#' @param tab_counter
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
gen_tabchunk = function(ct_csv, tab_opts, tab_counter, folder ="", chunklabels = list(), compat_cell_md_parsing = F){
  # table captions to return
  tab_capts = list()

  # some table options may depend on the data
  ncol =  NCOL(ct_csv)
  nrow = NROW(ct_csv)


  # table options, captuion, etc need to be in nexts paragraph
  if(!is.vector(tab_opts)) tab_opts = c() # set to empty if invalid (tables withotu table tags are allowed)

  # generate label and unique id
  tab_uind = "tab_" %+% tab_counter  %+% "_"  #%+% gsub(x = chapname, pattern = "-|-", replacement = "")
  tab_unid_latex = gsub(pattern = "_", replacement = "", x = tab_uind)
  tab_chunk_label = tab_unid_latex

  if(!endsWith(folder, "/") & nchar(folder) > 0){
    folder = paste0(folder, "/")
  }

  if(!dir.exists(paste0(folder, "tmp_tables"))){
    dir.create(paste0(folder, "tmp_tables"))
  }

  tab_fname = paste0(folder, "tmp_tables/", tab_uind, ".csv")
  tab_rmd_fname = paste0("tmp_tables/", tab_uind, ".csv")


  # manual label overrriade?
  if('label' %in% names(tab_opts)){
    tab_chunk_label = tab_opts['label']

    if(!grepl(x = tab_chunk_label, pattern = "^[a-zA-Z0-9]{1,}$")){
      stop(paste0("table label '", tab_chunk_label, "' contains other chars than letters (A-z) or numbers"))
    }
  }

  # check if chunklabel is unique, append if necessary
  while(tab_chunk_label %in% chunklabels) tab_chunk_label = paste0(tab_chunk_label, "-0")

  # caption provided?
  tab_caption = ""
  if('caption' %in% names(tab_opts)){
    tab_caption = tab_opts['caption']

    # escape %, @, $
    tab_caption = escape_caption(tab_caption)

  }

  # whole page stretch or single columns?
  tab_mode = "twocolumn" # default
  if('landscape' %in% tab_opts){
    tab_mode = "landscape"
  }
  else if('wide' %in% tab_opts){
      tab_mode = "wide"
  }

  # longtable?
  tab_long = F
  if('long' %in% tab_opts){
    tab_long = TRUE
  }

  tab_colwidths = NULL
  tab_colwidths_hux = NA
  if('colwidths' %in% names(tab_opts)){
    tab_colwidths_hux = tab_colwidths = tab_opts['colwidths'][[1]] |> dput()  |> capture.output()|> paste0(collapse = "")
  }

  tab_colaligns = NULL
  tab_colaligns_hux = rep(NA, ncol) # default values

  if('colaligns' %in% names(tab_opts)){
    tab_colaligns = tab_opts['colaligns'][[1]] |> dput()  |> capture.output() |> paste0(collapse = "")
    tab_colaligns_hux = ifelse(tab_opts['colaligns'][[1]] == "c", "center",
                               ifelse(tab_opts['colaligns'][[1]] == "l", "left",
                                      ifelse(tab_opts['colaligns'][[1]] == "r", "right", rep(NA, ncol))))
  }

  # aligment matrix for huxtable, repeat values in each column
  rowals = vector("character", ncol)
  for(i in 1:ncol){

    if(is.na(tab_colaligns_hux[i])) rowals[i] = "huxtable::set_align(col = " %+% i %+% ", value =  " %+% tab_colaligns_hux[i] %+% " ) |>"
    else rowals[i]                            = "huxtable::set_align(col = " %+% i %+% ", value = '" %+% tab_colaligns_hux[i] %+% "') |>"
  }
  tab_colspec_call_hux = paste(collapse = "\n" ,rowals)


  tab_gridmode = "academicgrid"
  if('fullgrid' %in% tab_opts){
    tab_gridmode = "fullgrid"
  } else if('topdowngrid' %in% tab_opts){
    tab_gridmode = "topdowngrid"
  }

  tab_footnote = NULL
  if('footnote' %in% names(tab_opts)){
    tab_footnote = escape_caption(tab_opts['footnote'])
  }

  # huxtable border and bold style specifications


  #print(paste0("Writing table: ", tab_fname))
  data.table::fwrite(ct_csv, tab_fname, col.names = F) # do not write first row of column indexes

  # more huxpsecific stuff
  # gridmode
  tab_borderspec_call_hux = ""
  header_inds = which(startsWith(ct_csv[[1]], "#"))
  tcalls = NULL
  if(tab_gridmode == "academicgrid"){


    # LOOP  through header inds



    tcalls = c("huxtable::set_top_border(row =1)",
      "huxtable::set_bottom_border(row = nrow)")


  } else if(tab_gridmode == "fullgrid"){
    tcalls = c("huxtable::set_all_borders()")

  } else if(tab_gridmode == "topdowngrid"){

    tcalls = c("huxtable::set_top_border(row =1)",
      "huxtable::set_bottom_border(row = nrow)")

  } else hgl_error("unkwown tab gridmode")
  tab_borderspec_call_hux = paste(tcalls, collapse = "|>\n")

  # add pipe operator if not epty
  if(nchar(tab_borderspec_call_hux) > 0) tab_borderspec_call_hux = tab_borderspec_call_hux %+% "|>"

  # genereate rmarkdown chunks
  ctab_chunk = "

```{=latex}
```{r " %+%  tab_chunk_label  %+%",echo=F" %+% ", results = 'asis'}

tab_dat<-read.csv('" %+% tab_rmd_fname %+% "', check.names = F, header = F)
ncol =  NCOL(tab_dat)
  tex = pdfRotuce::rabulify(tab_dat, mode = '" %+%  tab_mode %+% "',
              caption = '" %+% tab_caption %+% "',
              footnote = '" %+% tab_footnote %+% "',
              label = '" %+% tab_chunk_label %+%"' , long = '" %+% tab_long %+% "',
              colwidths = " %+%tab_colwidths%+% ", colaligns = " %+% tab_colaligns %+% ",
              gridmode = '" %+% tab_gridmode %+% "', compat_cell_md_parsing =  "%+%compat_cell_md_parsing  %+% ")
cat(tex)
```
```

```{=html}
```{r " %+%  tab_chunk_label  %+%"-html,echo=F" %+% ", results = 'asis'}

tab_dat<-read.csv('" %+% tab_rmd_fname %+% "', check.names = F, header = F)
ncol =  NCOL(tab_dat)
nrow = NROW(tab_dat)



html = huxtable::hux(tab_dat, add_colnames = F) |>
  huxtable::theme_article() |>
  huxtable::set_all_borders(value = 0) |>
  huxtable::set_col_width(col = 1:ncol, value= " %+% tab_colwidths_hux %+%") |> \n "%+%
  tab_colspec_call_hux %+%
  tab_borderspec_call_hux %+% "
  huxtable::set_caption(value = '" %+% tab_caption %+%"') |>
  huxtable::set_label(value = '" %+% tab_chunk_label %+%"') |>
  huxtable::add_footnote(text = '" %+% tab_footnote %+%"') |>
  huxtable::set_all_padding(value = 4) |>
  huxtable::set_markdown(TRUE) |>
  huxtable::print_html()

# infuse more css to fix margins introduced by markdown = T
html = '<style>
#content .huxtable-cell > p { margin-bottom:0; }
.huxtable-cell p:first-child {
margin-top:0;
margin-bottom:0;}
</style>' %+% html

cat(html)
```
```
"

  return(list(chunk = ctab_chunk, label = tab_chunk_label))
}
