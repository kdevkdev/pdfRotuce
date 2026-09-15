#' Parses YAML command tags in word documents
#'
#' @param c_comtext
#'
#' @return
#' @export
#'
#' @examples
parse_yaml_cmds = function(c_comtext, convert_straight_singlequote = T, escape_newlines_for_rmarkdown_propagation = T){
  c_comtext_o = c_comtext
  c_comtext = c_comtext |> gsub(pattern = "^\\[\\[", replacement = "") |> gsub(pattern = "\\]\\]$", replacement = "")

  if(convert_straight_singlequote){
    # if specified replace with straight single quotes to promote valid YAML
    # try to get only sensible single quotes after : and after ,
    c_comtext = gsub(x = c_comtext, pattern = "(\\b\\w+\\b:\\s?)[‘’]{1,1}", replacement = "\\1'") # replace non-straight single quotes after colons
    c_comtext = gsub(x = c_comtext, pattern = "[‘’]{1,1}(\\s?(?:,|$))", replacement = "'\\1") # replace non-straight single before , or at the end of string
  }
  if(escape_newlines_for_rmarkdown_propagation){
    c_comtext = gsub(x = c_comtext, pattern = "\n", replacement = "\\\\n") # convert newlines to preserve them in the command , especially useful in the 'text' value part.
  }


  # parse -> hopefully this is not too hacky. use yaml parser but trick with appending
  yaml_str = paste0("[",c_comtext, "]\n")
  #c_command = configr::str2config(yaml_str) |> unlist(recursive = F)
  c_command = tryCatch({yaml::read_yaml(text = yaml_str) |> unlist(recursive = F)}, # keys will be put as names
                       error = \(e){

                         print("YAML syntax yerror, check command - '" %+%  yaml_str %+% "'")
                         #browser()
                       })

  # unescape newlines
  # yaml , if single quote strings are used, does not unescape escaped newlines. It only does so for double quoted strings, cmp:
  # yaml::yaml.load(string = "[ text:   'hello\\nddd']")
  # yaml::yaml.load(string = "[ text:   \"hello\\nddd\"]")
  if(escape_newlines_for_rmarkdown_propagation && !is.null(c_command[['text']])){


    # this asumes that there is a text attribute
   c_command$text = gsub(x = c_command[['text']], pattern = "\\\\n", replacement = "  \\\n") # put back actual newlines, add two emtpy spaces for rmarkdown newlines
   # test: yaml::yaml.load(string = "[ text:   'hello\\nddd']")[[1]][['text']]  |> gsub(x = _, pattern = "\\\\n", replacement = "\\\n") |> cat()

  }

  c_command


}
`%+%` <- function(a, b) paste0(a, b)

