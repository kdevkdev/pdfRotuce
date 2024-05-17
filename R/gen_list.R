gen_list = function(items, label = "\\textbullet", markers = 1:length(items), options = NULL){


  latex_items  =paste("\\item [", trimws(stringr::str_replace(string = label, pattern = "_", replacement =  as.character(markers))), "]", items)

  opts = ""
  if(!is.null(options)){
    opts = paste0("[",paste0(names(options), "=", options, collapse = ","),"]")
  }


  paste0("\\begin{itemize}",opts, "\n", #"\n\\DrawEnumitemLabel",
            paste0(latex_items, collapse = "\n"),
         "\n\\end{itemize}\n\n")

}
