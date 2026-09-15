compile_metadata = function(doc_summar, doc_folder, meta_csv, reference_parsing){

  # get all l1 headings
  l1_inds = which(tolower(doc_summar$paragraph_stylename) == "heading 1")

  stopifnot("No level 1 heading style found" = length(l1_inds)>0 )

  title_page_inds = l1_inds[1]:(l1_inds[2]-1)

  # find and parse titele page - first heading 1 up to second heading 1
  parsed_meta = parse_title_page(doc_summar[title_page_inds,])

  # predefined metadata & metadata for
  predef_meta  = list()
  periodical_meta = list()

  # get article specific metadata from csv
  if(file.exists(paste0(doc_folder, "/metadata0_periodical.csv"))){

    # load periodical metadata
    pd_mp = read.csv(file = paste0(doc_folder, "/metadata0_periodical.csv"), header = F, fileEncoding = "UTF-8")
    pmvals = pd_mp[[2]]
    names(pmvals) = pd_mp[[1]]

    periodical_meta = as.list(pmvals)
  }


  # get article specific metadata from csv
  if(!is.null(meta_csv) & file.exists(meta_csv)){

    pd_tab = read.csv(file = meta_csv, header = F, fileEncoding = "UTF-8")

    # values to a vector and name to vector names
    tvals = pd_tab[[2]]
    names(tvals) = pd_tab[[1]]


    checkvec = c("volume", "issue", "string_volumeissue",
                 "doi", "has_abstract", "article_type", "author_shortname", "string_corresponding", "string_contact",
                 "string_responsibleeditor", "string_articleihstory", "string_keywords_heading",
                 "string_spanish_keywords", "string_declarations_title","string_keywords_spanish",
                 "string_multilang_abstract_title", "string_bibliography_title", "string_abstract_mainlang_title")



    # whitelist of values to copy
    for(cn in checkvec){

      if(!is.na(tvals[cn])){
        predef_meta[[cn]] = tvals[cn]
      }
    }

    # copy over provided values, uppercase for article typer
    # if(is.element("volume", names(tvals)))                    predef_meta$volume                    = tvals["volume"]                         else hgl_warn("'volume' missing in meta csv")
    # if(is.element("issue",  names(tvals)))                    predef_meta$issue                     = tvals["issue"]                          else hgl_warn("'issue' missing in meta csv")
    # if(is.element("string_volumeissue",  names(tvals)))       predef_meta$string_volumeissue        = tvals["string_volumeissue"]             else hgl_warn("'string_volumeissue' missing in meta csv")
    # if(is.element("doi", names(tvals)))                       predef_meta$doi                       = tvals["doi"]                            else hgl_warn("'doi' missing in meta csv")
    # if(is.element("has_abstract", names(tvals)))              predef_meta$has_abstract              = tvals["has_abstract"]                   else hgl_warn("'has_abstrat' missing in meta csv")

    # if(is.element("author_shortname", names(tvals)))          predef_meta$author_shortname          = tvals["author_shortname"]               else hgl_warn("'author_shortname' missing in meta csv")
    # if(is.element("string_corresponding", names(tvals)))      predef_meta$string_corresponding      = tvals["string_corresponding"]           else hgl_warn("'string_corresponding' missing in meta csv")
    # if(is.element("string_contact", names(tvals)))            predef_meta$string_contact            = tvals["string_contact"]                 else hgl_warn("'string_contact missing in meta csv")
    # if(is.element("string_responsibleeditor", names(tvals)))  predef_meta$string_responsibleeditor  = tvals["string_responsibleeditor"]       else hgl_warn("'string_responsibleeditor' missing in meta csv")
    # if(is.element("string_articlehistory", names(tvals)))     predef_meta$string_articlehistory     = tvals["string_articlehistory"]          else hgl_warn("'string_articlehistory' missing in meta csv")
    # if(is.element("string_keywords", names(tvals)))           predef_meta$string_keywords           = tvals["string_keywords"]                else hgl_warn("'string_keywords' missing in meta csv")
    # if(is.element("string_articlehistory", names(tvals)))     predef_meta$string_articlehistory     = tvals["string_articlehistory"]          else hgl_warn("'string_articlehistory' missing in meta csv")
    # if(is.element("string_keywords", names(tvals)))           predef_meta$string_keywords           = tvals["string_keywords"]                else hgl_warn("'string_keywords' missing in meta csv")


    # some more processing
    if(is.element("article_type", names(tvals)))              predef_meta$article_type              = toupper(tvals["article_type"])          else hgl_warn("'article_type' missing in meta csv")
    # split articledates by ';'
    if(is.element("articledates", names(tvals)))              predef_meta$articledates = stringr::str_split(simplify= T, pattern = ";", string =tvals["articledates"]) |> as.vector() |> trimws() else hgl_warn("'articledates' missing in meta csv")
    if(is.element("articledates_jats", names(tvals)))         predef_meta$articledates_jats = stringr::str_split(simplify= T, pattern = ";", string =tvals["articledates_jats"]) |> as.vector() |> trimws()

    if(startsWith(predef_meta$doi,"https://doi.org/") || startsWith(predef_meta$doi,"http://doi.org/")){

      # convert form url to doi if needed
      # for url start
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "^(https?://)?doi\\.org/", replacement = "")

      # for url end
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "/$", replacement = "")
    }
  }
  hardcoded_meta = gen_hardcoded_meta(reference_parsing = reference_parsing)


  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta, periodical_meta, hardcoded_meta)
  # also pack keywords from metadata$atrributes into the respective abstracts

  # find all attrributes starting with keywords
  kw_names = stringr::str_extract_all(string = names(metadata$attributes), pattern = "^keywords.*") |> unlist()

  for(ckwn in kw_names){

    ckws = metadata$attributes[[ckwn]]

    # detect langague suffix after '_', or mainlangaugeS
    if(ckwn == "keywords"){
      clan = 'mainlang'
      metadata$abstracts$mainlang$keywords = ckws
    } else{
      clan = stringr::str_replace_all(string = ckwn, pattern = "keywords_", replacement = "")

      # check if present - find index
      targ_ind = sapply(metadata$abstracts$sidelangs, \(x) { x$lang== clan})

      if(sum(targ_ind) ==1){

        metadata$abstracts$sidelangs[[which(targ_ind)]]$keywords = ckws

      } else{
        # if ever a template requires sidelang keywords without abstracts then att the sidelang abstract here and keywords after.
        hgl_warn("'" %+% ckwn %+% "' keywords provided but no or more than one abstract for language '" %+% clan %+% "' present.")
      }
    }
  }

  return(list(metadata =  metadata, doc_summar_wo_title_page = doc_summar[-title_page_inds,]
))
}
