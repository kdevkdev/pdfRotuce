# hellper fun to always add quotes in yaml output
yml_qt = function(x) {
  x = as.character(x)
  attr(x, "quoted") = T
  x
}
gen_yaml_header = function(md){

  # md : metadata to fill in


  # few hardcoded values
  md$copyright = "The Author(s). Published by Umeå university Library and owned by the Community Health Systems-Connect, a network of five universities from South Africa, Sweden Tanzania, Uganda and Zambia. J Community Systems for Health is a Fair Open Access journal distributed under the terms of the Creative Commons Attribution License (http://creativecommons.org/licenses/by/4.0/), which permits unrestricted use, distribution, and reproduction in any medium, provided the original work is properly cited."
  md$journal_title = "JOURNAL OF COMMUNITY SYSTEMS FOR HEALTH"
  md$journal_title_short = "J Community Systems for Healt"
  md$output                   = list('bookdown::pdf_document2' = list(template =  "template.tex"  |> yml_qt(),
                                                                      latex_engine   = "lualatex" |> yml_qt()))
  md$bibliography              = 'references.bib'  |> yml_qt()
  md$csl                      = 'plos-2020.csl'  |> yml_qt()

  # yml_data = list(title_full = md$title,
  #                 copyright = list(year = md$copyright_year, text = md$copyright),
  #                 journalinfo = list(title = md$journal_title, volume = md$volume, doi = md$doi),
  #                 abstractparts = md$abstractparts,
  #                 keywords = md$keywords,
  #                 authors = md$authors,
  #                 affiliations = md$authors,
  #                 correspondingauthor = list(address = '', email = md$corresponding_email),
  #                 articledates =  list(received = '', decision = '', accepted = '') ,
  #                 pageheader = list(even = '', odd = ''),
  #                 output = '',
  #                 bibliography = '',
  #                 csl = '')





  # first create empty structure
  yml_data = list(title_full = '',
                  copyright = list(year = '', text = ''),
                  journalinfo = list(title = '', volume = '', doi = ''),
                  abstractparts = list(list(title = '', text = ''),
                                       list(title = '', text = '')),
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
                  csl = '')




  # copy over -> to it this way to have default values availabvlw
  yml_data$title_full                = md$title |> yml_qt()
  yml_data$copyright$year            = md$copyright_year |> yml_qt()
  yml_data$copyright$text            = md$copyright |> yml_qt()
  yml_data$journalinfo$title         = md$journal_title |> yml_qt()
  yml_data$journalinfo$volume        = md$volume |> yml_qt()
  yml_data$journalinfo$doi           = md$doi  |> yml_qt() # ideally if the doi is the article specific one this should be restructured
  yml_data$abstractparts             = md$abstractparts
  yml_data$keywords                  = md$keywords
  yml_data$authors                   = md$authors
  yml_data$affiliations              = md$affiliations
  yml_data$correspondingauthor$email = md$corresponding_email |> yml_qt()
  yml_data$articledates              = md$articledates |> yml_qt()
  yml_data$pageheader$odd            = md$journal_title_short |> yml_qt()
  yml_data$pageheader$even           = md$pageheader |> yml_qt()
  yml_data$output                    = md$output
  yml_data$bibliography              = md$bibliography |> yml_qt()
  yml_data$csl                       = md$csl |> yml_qt()

  return(paste0("---\n",yaml::as.yaml(yml_data), "\n---\n"))
}
