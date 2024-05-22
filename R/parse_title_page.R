parse_title_page = function(docdat){
  # list to hol return values
  retlist = list()

  # (for historical purposes, paragraph can be deleted at a later stage) metadata alternatives
  # - clearly delinated titlepage vs free floating metadata commands
  # - all command boxes vs tables
  # - explicit command boxes vs minimally sytnaxiced paragraphs on delinated title page
  # - seperate (csv?) file

  stopifnot("invalid docdata data frame" = NROW(docdat) > 0 )



  # check for level 2 heading METADATA and ABSTRACT
  stopifnot(sum("not exactly 1 ABSTRACT level 2 heading" = tolower(docdat$style_name) == "heading 2" & tolower(trimws(docdat$text)) == "abstract")==1)
  stopifnot(sum("not ecactly 1 METADATA level 2 heading" = tolower(docdat$style_name) == "heading 2" & tolower(trimws(docdat$text)) == "metadata") == 1)

  # first heading 1 is title

  l1_title_ind = which(tolower(docdat$style_name) == "heading 1")

  retlist[["title"]] = trimws(docdat[l1_title_ind,]$text) # could happend that unwanted space added?


  # divide into parts
  docdat[, part_id := cumsum(tolower(style_name) == "heading 2")]



  # parse parts
  for(cpi in unique(docdat$part_id)){

    cdat = docdat[part_id == cpi]
    if(tolower(trimws(cdat$text[1])) == "abstract")# since
    {

      abdat = cdat[-1,] # without first row
      # one paragraph corresponds to 1 part
      # we boldify some words by by putting them in title
      #abdat$titles  = stringi::stri_match_first_regex(str = trimws(abdat$text), pattern = "^objective:|^methods:|^results:|^conclusion:", case_insensitive = T)
      abdat$titles  = stringi::stri_match_first_regex(str = trimws(abdat$text), pattern = "^\\w+:", case_insensitive = T)

      #abdat$text = trimws(stringi::stri_replace_first_regex(str = trimws(abdat$text), pattern = "^objective:|^methods:|^results:|^conclusion:", replacement = "", case_insensitive = T)) # call a second time to replace
      abdat$text = trimws(stringi::stri_replace_first_regex(str = trimws(abdat$text), pattern = "^\\w+:", replacement = "", case_insensitive = T)) # call a second time to replace

      # to avoid na
      abdat[is.na(text),   text   := ""]
      abdat[is.na(titles), titles := ""]

      retlist[["abstractparts"]] = apply(MARGIN = 1, FUN =\(x) list(title = x[["titles"]] |> yml_qt(), text = x[['text']] |> yml_qt()), X = abdat, simplify = F)
    }
    else if(tolower(trimws(cdat$text[1])) == "metadata"){

      # first tables
      cudoc_tabinds = unique(cdat[content_type == "table cell"]$doc_index)

      stopifnot("two tables in metadata section of titlepage needed, one for affiliations and one for authors" =  length(cudoc_tabinds) == 2)
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

        table_type = trimws(ct_tab[1,1])
        cn = unlist(ct_tab[1,])
        ct_tab = ct_tab[-1, ]
        colnames(ct_tab) = cn


        # parse tables - authors and affiliations
        if(table_type == "author"){

          # remove emptpy
          if(NROW(ct_tab[!is.null(author) | is.character(author) | nchar(author) == 0, ])>0){
            warning("Empty lines in authors table")
          }
          ct_tab = ct_tab[!is.null(author) & is.character(author) &nchar(author) > 0, ]

          retlist[["authors"]] = apply(X = ct_tab, MARGIN = 1, FUN = \(x){list(name = x[["author"]] |> yml_qt(), affiliation_ids = x[["affiliation_ids"]] |> yml_qt(), orcid = x[["orcid"]] |> yml_qt())})

        } else if(table_type == "affiliation"){

          retlist[["affiliations"]] = apply(X = ct_tab, MARGIN = 1, FUN = \(x){list(id = x[["id"]] |> yml_qt(), address = x[["affiliation"]] |> yml_qt())})
        }
        else {
          stop("parseing title page, metadata section: unkown table type")
        }

      }



      # tables already parsed
      cdat_notabs  = cdat[content_type != "table cell"]
      stopifnot("metadata part without tables empty" = NROW(cdat_notabs) > 1)
      # we parse all paragraphs
      for(cr in 2:NROW(cdat_notabs)){


        # command. Delinated by a colon.
        cmd = tolower(stringi::stri_match_first(cdat_notabs$text[cr], regex = "^[A-Za-z0-9_]{1,}\\s?(?=:)"))
        val = trimws(stringi::stri_replace_first(str = cdat_notabs$text[cr], regex = "^[A-Za-z0-9_]{1,}\\s?:", replacement = ""))

        switch(cmd,
               #articledates = { retlist[["articledates"]] = trimws(val) }, # could be split further, separated by semilcon
               keywords = { retlist[["keywords"]] =  trimws(strsplit(val, split = ",")[[1]] |> yml_qt())  },
               corresponding_email = { retlist[["corresponding_email"]] =  trimws(val) |> yml_qt()}
        )
      }
    }
  }
  return(retlist)
}

