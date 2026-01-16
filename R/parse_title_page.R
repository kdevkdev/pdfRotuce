parse_title_page = function(docdat){
  # list to hol return values
  retlist = list()



  stopifnot("invalid docdata data frame" = NROW(docdat) > 0 )



  # check for level 2 heading METADATA and ABSTRACT (for abstract at most one)
  stopifnot("Check that there is exactly one heading 'ABSTRACT' with style 'heading 2' on the manuscript titlepage " = sum( tolower(docdat$paragraph_stylename) == "heading 2" & tolower(trimws(docdat$text)) == "abstract")<=1)
  stopifnot("Check that there is exactly one heading 'METADATA' with style 'heading 2' on the manuscxript titlepage"  = sum(tolower(docdat$paragraph_stylename) == "heading 2" & tolower(trimws(docdat$text)) == "metadata") == 1)

  # first heading 1 is title
  l1_title_ind = which(tolower(docdat$paragraph_stylename) == "heading 1")


  retlist[["title"]] = trimws(docdat[l1_title_ind,]$text) # could happend that unwanted space added?

    # remove title
  docdat = docdat[-l1_title_ind,]

  # fix na not propagating in cumsum
  docdat[is.na(paragraph_stylename), paragraph_stylename := ""]

  # divide into parts
  docdat[text != "", part_id := cumsum(tolower(paragraph_stylename) == "heading 2")]

  # prereserver
  retlist[["abstracts"]] = list()

  # parse parts
  for(cpi in unique(na.omit(docdat$part_id))){

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

      if(NROW(abdat) > 0 && tolower(abdat$paragraph_stylename[1]) == "heading 3"){
        abstract_title = abdat$text[1]
        abdat = abdat[-1,] # delete also abstract title
      } else{
        abstract_title = ""
      }
      retlist[["abstracts"]][[langname]][["title"]] = abstract_title |> yml_qt()

      # anything left in abdat?
      if(NROW(abdat) > 0){

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

      }else{
        retlist[["abstracts"]][[langname]][["parts"]] = ""
      }
    }
    else if(section_heading == "metadata"){

      # first tables
      cudoc_tabinds = unique(cdat[content_type == "table cell"]$table_index)

      stopifnot("four tables in metadata section of titlepage needed: one for affiliations, one for authors, one for statements, and one for other attributes" =  length(cudoc_tabinds) == 4)
      stopifnot("metadata part empty" = NROW(cdat) > 1)

      # parse authors and affiliations in tables
      authors = NULL
      affiliations = NULL
      keywords = NULL
      #articledates = NULL

      for(cti in cudoc_tabinds){

        ct_dat = cdat[content_type == "table cell" & table_index == cti]
        ct_tab = data.table::dcast(ct_dat, row_id ~ cell_id, value.var = "text", fun.aggregate =  \(x){ paste(x, collapse = "\n\n")})[,-1] # not first


        if(NROW(ct_tab) <= 1){
          hgl_warn(paste0("empty table in metadata section: ",ct_tab[1,1]))
          next
        }

        table_type = trimws(tolower(ct_tab[1,1]))
        cn = unlist(ct_tab[1,])
        ct_tab = ct_tab[-1, ] # first row as colnames
        colnames(ct_tab) = trimws(tolower(cn))



        # parse tables - authors and affiliations
        if(table_type == "author"){

          # remove emptpy
          if(NROW(ct_tab[!is.null(author) & is.character(author) & nchar(author) == 0, ])>0){
            hgl_warn("Empty lines in authors table")
          }
          ct_tab = ct_tab[!is.null(author) & is.character(author) &nchar(author) > 0, ]


          ta = apply(X = ct_tab, MARGIN = 1, FUN = \(x){

              # detect corresponding author
              corresponding = FALSE
              if( stringr::str_detect(string = x[['affiliation_ids']], pattern = "\\*")){

                # delete star. Technically the asterixs is supposed to be in the end but we do not care for now
                x[['affiliation_ids']] = stringr::str_replace_all(string = x[['affiliation_ids']], pattern = "\\*", replacement = "")
                corresponding = TRUE
              }

              rl = list(name = x[["author"]] |> yml_qt(), affiliation_ids = x[["affiliation_ids"]] |> yml_qt(), orcid = x[["orcid"]] |> yml_qt(), corresponding = corresponding )

              # see if author has a , or ; (unescaped). If yes, create additional fields for surname and given name
              # this regex should work for with arbitrary number of escape chars
              name = x[['author']]

              # detect co- status (co-first, co-senior/co-last, or also called shared author ship)
              if(stringr::str_detect(name, pattern = "^&&")){

                # delete double ampersand from string
                name = trimws(stringr::str_replace(string = name, pattern = "^&&", replacement = ""))

                # set flag for co
                rl[['co_with_prior']] = yml_qt(T)
              }

              # we are interested in the end of the match (the first position always includes the previous char)
              cpos = stringr::str_locate(string = name, pattern = "(?:(?:[^\\\\](?:\\\\\\\\)*))(?:,|;)")[2]


             # if two parts
             if(!is.na(cpos) && cpos > 1 & cpos +1< nchar(name)){

                nameparts = vector("character", 2)
                nameparts[1] = trimws(stringr::str_sub(name, 1, cpos-1))
                nameparts[2] = trimws(stringr::str_sub(name, cpos+1, nchar(name))) # -1, +1 bec. we do not want to include the ,



                rl[['family_name']] = trimws(nameparts[1])
                rl[['first_name']] = trimws(nameparts[2])
                # also put the correct order into the whole string
                rl[['name']] = paste0(rl[['first_name']]," ", rl[['family_name']])

              } else {
                rl[['name']] = name
                hgl_warn(paste0("Name does not contain ',' or ';' (", name ,") - if applicable it is strongly recommended to separate family name and given name by a ',' or ';' - [family name],[given name]"))
              }

              # parse contributions
              if("contributions" %in% names(x) &&  !is.null(x[['contributions']]) && !is.na(x[['contributions']]) ){

                craw = x[['contributions']]
                # each comma seperated part holds a 'contribution rolen
                roles = trimws(stringr::str_split(string = craw, patter = ",")[[1]])

                rl[['contrib_roles']] = roles
              }



              rl
          })

          retlist[["authors"]] = ta
        } else if(table_type == "affiliation"){

          stopifnot("non-unique is in affiliation table"= sum(duplicated(ct_tab$id)) == 0)

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
          stop("parseing metadata title page esction: unkown table type ' " %+% table_type %+%" '. Make sure that you provide the correct header row for all tables in the metadata section. ")
        }
      }
    }
    else{

        stop("unkown title page section " %+% section_heading %+% "(should be metadata or ABSTRACT_XX, where XX corresponds to a supported language acronym ")
    }
  }
  # check if any additional language abstracts have ben specified. If yes, generate a hint text to be place below the main abstract
  abside_languages = setdiff(names(retlist$abstracts), "mainlang")

  lang_propernames = c(es = "Español", som = "Somali")

  # generate hint toext
  if(length(abside_languages) > 0){
    retlist$abstract_sidelangs_hint = paste("Abstract in ", paste(lang_propernames[abside_languages], sep = ", "), "at the end of the article")
  }

  return(retlist)
}

