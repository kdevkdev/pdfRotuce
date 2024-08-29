parse_title_page = function(docdat){
  # list to hol return values
  retlist = list()



  stopifnot("invalid docdata data frame" = NROW(docdat) > 0 )



  # check for level 2 heading METADATA and ABSTRACT
  stopifnot("Check that there is exactly one heading 'ABSTRACT' with style 'heading 2' on the manuscript titlepage " = sum( tolower(docdat$style_name) == "heading 2" & tolower(trimws(docdat$text)) == "abstract")==1)
  stopifnot("Check that there is exactly one heading 'METADATA' with style 'heading 2' on the manuscxript titlepage"  = sum(tolower(docdat$style_name) == "heading 2" & tolower(trimws(docdat$text)) == "metadata") == 1)

  # first heading 1 is title
  l1_title_ind = which(tolower(docdat$style_name) == "heading 1")

  # remove title
  docdat = docdat[-l1_title_ind,]

  retlist[["title"]] = trimws(docdat[l1_title_ind,]$text) # could happend that unwanted space added?


  # divide into parts
  docdat[, part_id := cumsum(tolower(style_name) == "heading 2")]

  # prereserver
  retlist[["abstracts"]] = list()

  # parse parts
  for(cpi in unique(docdat$part_id)){

    cdat = docdat[part_id == cpi]
    section_heading=tolower(trimws(cdat$text[1]))
    if(section_heading %in%  c("abstract", "abstract_es")) # first is main abstract, then follows additional languages
    {
      # detect which abstract language it is (need to list all allowed languages above)
      if(section_heading == "abstract"){
        langname = "mainlang" # main language (usually english)
      }
      else{

        langname = gsub("abstract_", replacement = "", x = section_heading)
      }


      abdat = cdat[-1,] # without first row


      # check if we have a title
      abstract_title = NA
      if(tolower(abdat$style_name[1]) == "heading 3"){
        abstract_title = abdat$text[1]
        abdat = abdat[-1,] # delete also abstract title
      }


      # one paragraph corresponds to 1 part
      # we boldify some words by by putting them in title
      #abdat$titles  = stringi::stri_match_first_regex(str = trimws(abdat$text), pattern = "^objective:|^methods:|^results:|^conclusion:", case_insensitive = T)
      abdat$titles  = stringi::stri_match_first_regex(str = trimws(abdat$text), pattern = "^\\w+:", case_insensitive = T)

      #abdat$text = trimws(stringi::stri_replace_first_regex(str = trimws(abdat$text), pattern = "^objective:|^methods:|^results:|^conclusion:", replacement = "", case_insensitive = T)) # call a second time to replace
      abdat$text = trimws(stringi::stri_replace_first_regex(str = trimws(abdat$text), pattern = "^\\w+:", replacement = "", case_insensitive = T)) # call a second time to replace

      # to avoid na
      abdat[is.na(text),   text   := ""]
      abdat[is.na(titles), titles := ""]

      tl = apply(MARGIN = 1, FUN =\(x) list(title = x[["titles"]] |> yml_qt(), text = x[['text']] |> yml_qt()), X = abdat, simplify = F)
      retlist[["abstracts"]][[langname]][["parts"]] = tl
      retlist[["abstracts"]][[langname]][["title"]] =abstract_title |> yml_qt()

    }
    else if(section_heading == "metadata"){

      # first tables
      cudoc_tabinds = unique(cdat[content_type == "table cell"]$doc_index)

      stopifnot("four tables in metadata section of titlepage needed: one for affiliations, one for authors, one for statements, and one for other attributes" =  length(cudoc_tabinds) == 4)
      stopifnot("metadata part empty" = NROW(cdat) > 1)

      # parse authors and affiliations in tables
      authors = NULL
      affiliations = NULL
      keywords = NULL
      corresponding_email = NULL
      #articledates = NULL

      for(cti in cudoc_tabinds){

        ct_dat = cdat[content_type == "table cell" & doc_index == cti]
        ct_tab = data.table::dcast(ct_dat, row_id ~ cell_id, value.var = "text")[,-1] # not first

        table_type = trimws(tolower(ct_tab[1,1]))
        cn = unlist(ct_tab[1,])
        ct_tab = ct_tab[-1, ]
        colnames(ct_tab) = trimws(tolower(cn))


        # parse tables - authors and affiliations
        if(table_type == "author"){

          # remove emptpy
          if(NROW(ct_tab[!is.null(author) & is.character(author) & nchar(author) == 0, ])>0){
            warning("Empty lines in authors table")
          }
          ct_tab = ct_tab[!is.null(author) & is.character(author) &nchar(author) > 0, ]

          retlist[["authors"]] = apply(X = ct_tab, MARGIN = 1, FUN = \(x){list(name = x[["author"]] |> yml_qt(), affiliation_ids = x[["affiliation_ids"]] |> yml_qt(), orcid = x[["orcid"]] |> yml_qt())})

        } else if(table_type == "affiliation"){

          retlist[["affiliations"]] = apply(X = ct_tab, MARGIN = 1, FUN = \(x){list(id = x[["id"]] |> yml_qt(), address = x[["affiliation"]] |> yml_qt())})
        } else if(table_type == "attribute"){
          l = apply(X = ct_tab, MARGIN = 1, FUN = \(x){x[["value"]]  |> yml_qt()}, simplify = F)
          names(l) = ct_tab$attribute
          retlist[["attributes"]] = l
        } else if(table_type == "statement"){
          l = apply(X = ct_tab, MARGIN = 1, FUN = \(x){x[["text"]]  |> yml_qt()}, simplify = F)
          names(l) = ct_tab$statement
          retlist[["statements"]] = l
        }
        else {
          stop("parseing metadata title page esction: unkown table type. Make sure that you provide the correct header row for all tables in the metadata section. f")
        }
      }
    }
    else{

        stop("unkown title pge section (should be metadata or ABSTRACT_XX, where XX corresponds ")
    }
  }
  return(retlist)
}

