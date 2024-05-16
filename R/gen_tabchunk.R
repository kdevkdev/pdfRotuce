escape_caption <- function(str){
  str = stringr::str_replace_all(str, pattern = "\\@", replacement = "\\\\\\\\@")
  str = stringr::str_replace_all(str, pattern = "\\%", replacement = "\\\\\\\\%")
  str = stringr::str_replace_all(str, pattern = "\\$", replacement = "\\\\\\\\$")
  str
}
#' Title
#'
#' @param ct_csv
#' @param tab_opts_raw
#' @param tab_counter
#' @param folder
#'
#' @return
#' @export
#'
#' @examples
gen_tabchunk = function(ct_csv, tab_opts_raw, tab_counter, folder =""){

  # table captions to return
  tab_capts = list()

  # table options, captuion, etc need to be in nexts paragraph
  tab_opts = c() # empty

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


  if(startsWith(trimws(tab_opts_raw$mrkdwn), "[[table")){

    tab_opts = parse_yaml_cmds(trimws(tab_opts_raw$mrkdwn))
  } else{

    warning("No table tag found")
  }


  # manual label overrriade?
  if('label' %in% names(tab_opts)){
    tab_chunk_label = tab_opts['label']

    if(!grepl(x = tab_chunk_label, pattern = "^[a-zA-Z0-9]{1,}$")){
      stop(paste0("table label '", tab_chunk_label, "' contains other chars than letters (A-z) or numbers"))
    }
  }

  # caption provided?
  tab_caption = ""
  if('caption' %in% names(tab_opts)){
    tab_caption = tab_opts['caption']

    # escape %, @, $
    tab_caption = escape_caption(tab_caption)

  }

  # whole page stretch or single columns?
  tab_wide = F
  if('wide' %in% tab_opts){
    tab_wide = TRUE
  }

  # longtable?
  tab_long = F
  if('long' %in% tab_opts){
    tab_long = TRUE
  }

  tab_colwidths = NULL
  if('colwidths' %in% names(tab_opts)){
    tab_colwidths = tab_opts['colwidths'][[1]] |> dput() |> capture.output()
  }

  tab_colaligns = NULL
  if('colaligns' %in% names(tab_opts)){
    tab_colaligns = tab_opts['colaligns'][[1]] |> dput() |> capture.output()
  }

  tab_fullgrid = "FALSE"
  if('fullgrid' %in% tab_opts){
    tab_fullgrid = "TRUE"
  }



  print(paste0("Writing table: ", tab_fname))
  data.table::fwrite(ct_csv, tab_fname, col.names = F) # do not write first row of column indexes

  # manually change spacing
  ctab_chunk = "

```{=latex}
```{r " %+%  tab_chunk_label  %+%",echo=F" %+% ", results = 'asis'}

tab_dat<-read.csv('" %+% tab_rmd_fname %+% "', check.names = F, header = F)
ncol =  NCOL(tab_dat)
  tex = pdfRotuce::rabulify(tab_dat, wide = " %+%  tab_wide %+% ", caption = '" %+% tab_caption %+% "',
              label = '" %+% tab_chunk_label %+%"' , long = '" %+% tab_long %+% "',
              colwidths = " %+%tab_colwidths%+% ", colaligns = " %+% tab_colaligns %+% ",
              fullgrid = " %+% tab_fullgrid %+% ")
cat(tex)
```
```"

}
