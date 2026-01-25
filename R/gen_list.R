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


  html_items  =paste("<", tag ," value='",markers,"'>",items, "</", tag, ">", sep = "")
  # add some css style to make erornously inserted (by pandoc) </br> tags not have line braking effect
  paste0("<ol class='bibliography_list'>\n",
         paste0(html_items, collapse = "\n"),
         "</ol><style> .bibliography_list > ", tag, " > br { content:''; display:none; } </style>")


}
