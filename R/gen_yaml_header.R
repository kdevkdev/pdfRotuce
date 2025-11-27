# hellper fun to always add quotes in yaml output
yml_qt = function(x) {
  x = as.character(x)
  attr(x, "quoted") = T
  x
}
gen_hardcoded_meta = function(reference_parsing ){

  md = list()

  # parameters relevant for parsing
  md$output                   = list('bookdown::pdf_document2' = list(template =  "template.tex"  |> yml_qt(),
                                                                      latex_engine   = "lualatex" |> yml_qt()),
                                     'bookdown::html_format2' = list(template = "template.html" |> yml_qt(),
                                                                     toc = TRUE,
                                                                     toc_depth = 3))


  # only put references.bib file as bibliography if appropriate - (ie if not in passthrough mode)
  if(reference_parsing != FALSE){
    md$bibliography              = 'references.bib'  |> yml_qt()
  }
  md$csl                      = 'plos-2020_square_nums.csl'  |> yml_qt()

  return(md)
}
gen_yaml_header = function(md, reference_parsing = T){


  yml_data = list()

  # copy over -> to it this way to have default values availabvlw
  yml_data$title_full                         = md$title
  yml_data$title                              = md$title # also put here to stop pandoc from complaining that its mandatory
  yml_data$copyright$year                     = md$copyright_year
  yml_data$copyright$text                     = md$copyright
  yml_data$license                            = md$license



  yml_data$journalinfo$title                  = md$journal_title
  yml_data$journalinfo$title_short            = md$journal_title_short
  yml_data$journalinfo$volume                 = md$volume
  yml_data$journalinfo$issue                  = md$issue
  yml_data$journalinfo$string_volumeissue     = md$string_volumeissue
  yml_data$journalinfo$doi                    = md$doi   # ideally if the doi is the article specific one this should be restructured
  yml_data$journalinfo$article_type           = md$article_type
  yml_data$journalinfo$issn                   = md$issn
  yml_data$journalinfo$publisher              = md$publisher

  yml_data$abstracts                          = md$abstracts
  yml_data$keywords                           = md$attributes$keywords
  yml_data$authors                            = md$authors
  yml_data$affiliations                       = md$affiliations
  yml_data$correspondingauthor$email          = md$attributes$corresponding_email
  yml_data$correspondingauthor$allstring      = md$attributes$corresponding_allstring
  yml_data$responsible_editor                 = md$attributes$responsible_editor
  yml_data$articledates                       = md$articledates
  yml_data$author_shortname                   = md$author_shortname
  yml_data$output                             = md$output
  yml_data$bibliography                       = md$bibliography
  yml_data$csl                                = md$csl
  yml_data$has_abstract                       = md$has_abstract
  yml_data$abstract_sidelangs_hint            = md$abstract_sidelangs_hint
  yml_data$string_corresponding               = md$string_corresponding
  yml_data$string_contact                     = md$string_contact
  yml_data$string_responsibleeditor           = md$string_responsibleeditor
  yml_data$string_articlehistory              = md$string_articlehistory
  yml_data$string_keywords                    = md$string_keywords
  yml_data$string_keywords_spanish            = md$string_keywords_spanish
  yml_data$string_multilang_abstract_title    = md$string_multilang_abstract_title
  yml_data$string_bibliography_title          = md$string_bibliography_title
  yml_data$string_declarations_title          = md$string_declarations_title





  if(!is.null(md$attributes$abstract_picture) && !is.na(md$attributes$abstract_picture) && is.character(md$attributes$abstract_picture) && nchar(md$attributes$abstract_picture) > 1)
  {
      yml_data$abstract_picture      = paste0("../", md$attributes$abstract_picture)
  } else {
    yml_data$abstract_picture        = ""
  }

  # check fore additional keywords
  sidelang_keywords_names = grep("keywords_", names(md$attributes), value = T)

  for(cn in sidelang_keywords_names){
    yml_data[[cn]] = md$attributes[[cn]]
  }

  # remove abstract again if 'has_abstract' has been specified or if mainlang not defined
  if((!is.null(yml_data$has_abstract) && yml_data$has_abstract == "no") || is.null(yml_data$abstracts$mainlang)){
    yml_data$abstracts = NULL
  }

  # put all characters as quoted
  return(paste0("---\n",yaml::as.yaml(yml_data, handlers = list("character" = \(x){ attr(x, "quoted") = T; x } )), "\n---\n"))
}
