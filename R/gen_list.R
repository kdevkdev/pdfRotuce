gen_list_latex = function(items, label = "\\textbullet", markers = 1:length(items), options = NULL){


  latex_items  =paste("\\item [", trimws(stringr::str_replace(string = label, pattern = "_", replacement =  as.character(markers))), "]", items)

  opts = ""
  if(!is.null(options)){
    opts = paste0("[",paste0(names(options), "=", options, collapse = ","),"]")
  }


  paste0("\\begin{itemize}",opts, "\n", #"\n\\DrawEnumitemLabel",
            paste0(latex_items, collapse = "\n"),
         "\n\\end{itemize}\n\n")

}
gen_list_html = function(items, tag = "li", markers = 1:length(items)){


  html_items  =paste("<", tag ," value='",markers,"'>",items, "</", tag, ">")


  paste0("<ol>\n",
         paste0(html_items, collapse = "\n"),
         "</ol>")

}
