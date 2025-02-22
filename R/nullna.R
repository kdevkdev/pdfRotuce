
is.nullna = function(x) { is.null(x) || is.na(x) || length(x) == 0 }

putxml.nullna = function(x, tag = "", atts = NULL) {
  atts  = if(!is.null(atts)) paste0(" ", names(atts), "=",  "'", atts, "' ", collapse = " ") else ""
  if(!is.nullna(x)) paste0("<", tag,  atts,  ">",x|> xe(),"</",tag,">")  else ""
}
