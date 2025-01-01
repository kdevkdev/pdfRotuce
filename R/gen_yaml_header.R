# hellper fun to always add quotes in yaml output
yml_qt = function(x) {
  x = as.character(x)
  attr(x, "quoted") = T
  x
}
gen_hardcoded_meta = function(reference_parsing ){

  md = list()

  # few hardcoded values - should go into some conflig file when a good solutions is thought ofs
  md$copyright = "The Author(s). Published by Umeå university Library and owned by the Community Health Systems-Connect, a network of five universities from South Africa, Sweden Tanzania, Uganda and Zambia."
  md$license = "J Community Systems for Health is a Fair Open Access journal distributed under the terms of the Creative Commons Attribution License (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted use, distribution, and reproduction in any medium, provided the original work is properly cited."
  md$copyright_holder = "The Author(s)"
  md$journal_title = "JOURNAL OF COMMUNITY SYSTEMS FOR HEALTH"
  md$journal_title_short = "J Community Systems for Health"
  md$output                   = list('bookdown::pdf_document2' = list(template =  "template.tex"  |> yml_qt(),

                                                                      latex_engine   = "lualatex" |> yml_qt()))
  md$publisher = "Umeå University Library"
  md$journal_publisher_id = "JCSH"
  md$issn = "3035-692X"

  if(reference_parsing != FALSE){
    md$bibliography              = 'references.bib'  |> yml_qt()
  }
  md$csl                      = 'plos-2020_square_nums.csl'  |> yml_qt()

  return(md)
}
gen_yaml_header = function(md, reference_parsing = T){

  # md : metadata to fill in


  # first create empty structure
  yml_data = list(title_full = '',
                  copyright = list(year = '', text = ''),
                  license = '',
                  journalinfo = list(title = '', volume = '', issue = '', doi = '', article_type = '', string_volumeissue = ''),
                  abstracts = list(mainlang = list(parts = list(title = '', text = ''), title = '')),
                  keywords = c(),
                  authors = list(list(name = '', affiliation_ids = '', orcid = ''),
                                 list(name = '', affiliation_ids = '', orcid = '')),

                  affiliations = list(list(id = '', address = '', orcid = ''),
                                      list(id = '', address = '', orcid = '')),
                  correspondingauthor = list(address = '', email = ''),
                  articledates = list(received = '', decision = '', accepted = ''),
                  pageheader = list(even = '', odd = ''),
                  output = '',
                  bibliography = '',
                  csl = '',
                  abstract_sidelangs_hint = '',
                  abstract_picture = '',
                  string_corresponding = '',
                  publisher = '',
                  issn = '')


  # copy over -> to it this way to have default values availabvlw
  yml_data$title_full                         = md$title |> yml_qt()
  yml_data$copyright$year                     = md$copyright_year |> yml_qt()
  yml_data$copyright$text                     = md$copyright |> yml_qt()
  yml_data$license                            = md$license |> yml_qt()

  yml_data$journalinfo$title                  = md$journal_title |> yml_qt()
  yml_data$journalinfo$volume                 = md$volume |> yml_qt()
  yml_data$journalinfo$issue                  = md$issue |> yml_qt()
  yml_data$journalinfo$string_volumeissue     = md$string_volumeissue |> yml_qt()
  yml_data$journalinfo$doi                    = md$doi  |> yml_qt() # ideally if the doi is the article specific one this should be restructured
  yml_data$journalinfo$article_type           = md$article_type  |> yml_qt()
  yml_data$journalinfo$issn                   = md$issn |> yml_qt()
  yml_data$journalinfo$publisher              = md$publisher |> yml_qt()

  yml_data$abstracts                          = md$abstracts
  yml_data$keywords                           = md$attributes$keywords
  yml_data$authors                            = md$authors
  yml_data$affiliations                       = md$affiliations
  yml_data$correspondingauthor$email          = md$attributes$corresponding_email |> yml_qt()
  yml_data$articledates                       = md$articledates |> yml_qt()
  yml_data$pageheader$odd                     = md$journal_title_short |> yml_qt()
  yml_data$pageheader$even                    = md$pageheader |> yml_qt()
  yml_data$output                             = md$output
  yml_data$bibliography                       = md$bibliography |> yml_qt()
  yml_data$csl                                = md$csl |> yml_qt()
  yml_data$has_abstract                       = md$has_abstract |> yml_qt()
  yml_data$abstract_sidelangs_hint            = md$abstract_sidelangs_hint |> yml_qt()
  yml_data$string_corresponding               = md$string_corresponding |> yml_qt()


  if(!is.null(md$attributes$abstract_picture) && !is.na(md$attributes$abstract_picture) && is.character(md$attributes$abstract_picture) && nchar(md$attributes$abstract_picture) > 1)
  {
      yml_data$abstract_picture      = paste0("../", md$attributes$abstract_picture) |> yml_qt()
  } else {
    yml_data$abstract_picture        = "" |> yml_qt()
  }

  # check fore additional keywords
  sidelang_keywords_names = grep("keywords_", names(md$attributes), value = T)

  for(cn in sidelang_keywords_names){
    yml_data[[cn]] = md$attributes[[cn]] |> yml_qt()
  }

  # remove abstract again if 'has_abstract' has been specified or if mainlang not defined
  if(yml_data$has_abstract == "no" || is.null(yml_data$abstracts$mainlang)){
    yml_data$abstracts = NULL
  }

  return(paste0("---\n",yaml::as.yaml(yml_data), "\n---\n"))
}
