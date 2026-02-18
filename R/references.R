parse_references = function(doc_summar, working_folder, reference_parsing,
                            reference_overwrite,
                            refparsing_inject,
                            grobid_consolidate,
                            grobid_consolidate_blacklist,
                            augment_global,
                            augment_whitelist ,
                            augment_blacklist,
                            url_parsing, doi_parsing, guess_refnumbers){



  # preparation

  # data table to temporariliy store xml intext citations
  d_xmlintext_cites = NULL

  if(!is.null(grobid_consolidate_blacklist)) grobid_consolidate_blacklist = stringr::str_split(grobid_consolidate_blacklist, pattern = ",")
  if(!is.null(augment_blacklist)) augment_blacklist = stringr::str_split(augment_blacklist, pattern = ",")


  d_augment_whitelist = data.table(internal_id = character(0), service = character(0),  DOI = character(0), PMID = character(0), external_id = character(0), service = character(0))
  if(!is.null(augment_whitelist)){

    # assign columns based on group of match to variables
    #t =  stringr::str_match(c("13=doi", "ab=2:", "dd20=doi:134", "gobo"), pattern = "(?:(.+?)(=|$))(?:(.+?)(:|$))?(?:(.+?)$)?")
    t =  stringr::str_match(string = augment_whitelist, pattern = "(?:(.+?)(=|$))(?:(.+?)(:|$))?(?:(.+?)$)?")

    d_augment_whitelist = data.table(service = t[, 4],
                                     external_id = t[, 6],
                                     internal_id = t[,2],
                                     DOI = NULL, # For convenience
                                     PMID = NULL)

    d_augment_whitelist[service == "doi", DOI := external_id]
    d_augment_whitelist[service == "pmid", PMID := external_id]

  }


  # find,parse,  and remove references
  l1_inds = which(tolower(doc_summar$paragraph_stylename) == "heading 1")
  refparind = which(tolower(doc_summar$text) %in% c('references', 'referencias') & tolower(doc_summar$paragraph_stylename) == "heading 1")
  ref_inds = NULL


  #rmd_references = ""
  yaml_references = ""
  if(length(refparind) == 0)   hgl_warn("No references found in manuscript!!!")
  else{

    # refinds are those paragraphs until end or next l1 heading
    ti = which(l1_inds > (refparind+1)) #+1 because we donot want to have the heading included


    if(length(ti) == 0) lri = NROW(doc_summar) # no subsequent l1 heading, we go until end
    else lri = l1_inds[ti] -1 #deduct 1 because we do not want to have the next level 1 heading included

    ref_inds = (refparind+1):lri # do not include the starging l1 heading , +1

    # create one large text value
    references =  doc_summar$text[ref_inds]

    # discard empty
    empty_inds = which(trimws(references) == "")

    # length 0 if none TRUE in which() above
    if(length(empty_inds) > 0){
      references = references[-empty_inds]
    }

  }

  path_temprefs_in  = paste0(working_folder, "/tempreftxts_in")
  path_temprefs_out = paste0(working_folder, "/tempreftxts_out")

  if(dir.exists(path_temprefs_in)) unlink(path_temprefs_in, recursive = T)
  if(dir.exists(path_temprefs_out)) unlink(path_temprefs_out, recursive = T)

  if(file.exists(paste0(working_folder, "/", "references.bib"))) unlink(paste0(working_folder, "/", "references.bib"))
  if(file.exists(paste0(working_folder, "/", "references_autoparsed.bib"))) unlink(paste0(working_folder, "/", "references_autoparsed.bib"))

  if(reference_parsing != FALSE && endsWith(reference_parsing, ".bib")){ # use provided bibfile

    rpath = ""
    if(file.exists(reference_parsing)){

      rpath = reference_parsing

    } else if(file.exists(paste0(working_folder, "/../", reference_parsing))){ # also check parent of working folder

      rpath = paste0(working_folder, "/../", reference_parsing)

    } else {
      hgl_error(paste0("'", reference_parsing, "' file provided as 'reference_parsing', but no file found at this location"))
    }

    # copy to workingu dir/references.bib
    dpath = paste0(working_folder, "/references.bib")
    file.copy(from = rpath, to = dpath, overwrite = T)
    hgl_note(paste0("Copied '", reference_parsing, "'  to '",paste0(working_folder, "/references.bib"), "', using for further procesing"))

    d_refs = data.table(bib2df::bib2df(file =dpath))

    d_refs$BIBLIOGRAPHY_NUMBER = NA

    # do not parse references inside figures and table tags
    ttext = trimws(doc_summar$text)
    content_inds = setdiff(1:NROW(doc_summar), which(startsWith(ttext, "[[table") | startsWith(ttext, "[[figure")))

    # current max of bibliography_number
    cbn_max = 1

    # resut list for xmlintext citations
    l_xmlintextcites = list()

    # add BIBLIOGRAPHY_NUMBER field to the data - parse all in text references and look up the number
    # also generate xml intext citations along the way
    doc_summar$xml_temp[content_inds] = stringr::str_replace_all(str = doc_summar$text[content_inds], pattern = "\\[((@[_\\-a-zA-Z0-9]+)(,? ?){0,1})+\\]", replacement = \(match) {


      # first get rid of encompassing [], then split into individual components 'ranges'
      t = stringr::str_replace_all(string = match, pattern = "\\[|\\]", replacement = "")
      intextcites = stringr::str_split(t, pattern = "[,;]", simplify = F)[[1]]
      locres = vector("character", length(intextcites))

      for(cri in 1:length(intextcites)){

        # look up in d_refs
        cic = trimws(intextcites[cri], whitespace = "[ @]")

        ccr = d_refs[BIBTEXKEY == cic,]
        cbn = NA
        bn = NA

        if(NROW(ccr) == 0){
          hgl_warn(paste0("In-text citation '",cic,"' not found in provided .bib references "))
        } else if(NROW(ccr) > 1){
          hgl_error(paste0("In-text citation '",cic,"' not unique provided .bib references, multiple matches found."))
        }else{
          # must be 1 match
          if(is.na(ccr$BIBLIOGRAPHY_NUMBER)){
            # not yet cited, set the match to current max, and increase the later by 1
            ccr$BIBLIOGRAPHY_NUMBER = cbn_max
            cbn_max = cbn_max +1
          } # otherwise a BIBLIOGRAPHY_NUMBER is already present, do nothing
          bn = ccr$BIBLIOGRAPHY_NUMBER
        }

        len = length(l_xmlintextcites)+1
        locres[cri] = paste0("========protectedintextcite", len, "========")
        l_xmlintextcites[[len]] <<- data.table(xml = paste0("<xref ref-type='bibr' rid='B", bn, "'>", bn, "</xref>"), index = len)
      }



      xml = paste0("[", paste0(locres, collapse = ";"), "]")
      xml
    })

    # generate citation data
    d_xmlintext_cites = rbindlist(l = l_xmlintextcites)

    # remove potentialbibliography parts from document
    if(!is.null(refparind) && !is.null(ref_inds)){


      doc_summar = doc_summar[-c(refparind, ref_inds), ]
    }

    # put placeholder for CLS references into the RMD text
    # rmd_references = paste0("# References\n\n",
    #                         "<div id=\"refs\"></div>")

    # https://pandoc.org/MANUAL.html
    # put into metadata field instead of div above, so that we can use teh $refs$ variable
    rmd_references = "---
refs: |
   ::: {#refs}
   :::
..."



    doc_summar$mrkdwn = doc_summar$text

    # protedct @ so that id does not get excapted
    doc_summar$mrkdwn[content_inds] = stringr::str_replace_all(str = doc_summar$mrkdwn[content_inds], pattern = "\\[((@[_\\-a-zA-Z0-9]+)(,?;? ?){0,1})+\\]", replacement = \(match) {

      stringr::str_replace_all(string = match, pattern = "@", replacement = "========protectedat========")
    })


  }
  else if(reference_parsing != FALSE && reference_parsing %in% c("anystyle", "grobid")){ # parsing references


    dir.create(path_temprefs_in, showWarnings = FALSE)
    dir.create(path_temprefs_out, showWarnings = FALSE)

    # remove number from beginning
    ref_itemnums = trimws(stringr::str_extract(string = references, pattern = "^[0-9]+[.]"))
    references = stringr::str_replace(string = references, pattern = "^[0-9]+[.]", replacement = "")

    # if all NA - take index of position
    if(any(is.na(ref_itemnums)) && guess_refnumbers ==T) ref_itemnums = 1:length(ref_itemnums)
    else if(any(is.na(ref_itemnums))) hgl_error("Could not parse bibliography references item numbers, please check")



    ############ reference injection ##########################
    d_refparser_inject = data.table()
    if(!is.null(refparsing_inject)){

      # would not need to match citekeyts
      t =  stringr::str_match(string = refparsing_inject, pattern = "(?:(.+?)(=|$))(?:(.+))?")

      d_refparser_inject$internal_id = trimws(t[, 2])
      d_refparser_inject$payload = t[,4]
      # find matching ref_itemnums - test using starts with
      inject_ids = sapply(d_refparser_inject$internal_id, FUN = \(x) { which(startsWith(x = ref_itemnums, prefix = x))})



      # overwrite bibliography entry befor writing to file
      if(length(inject_ids) > 0) references[inject_ids] = d_refparser_inject$payload



    }



    # save single .txt for each reference - do it this way to keep track of the ordering in of original bibliography as much as possible
    sapply(1:length(references), FUN = \(x) { writeLines(text = references[x], con = paste0(path_temprefs_in, "/", x, ".txt")) })


    # remove from document
    doc_summar = doc_summar[-c(refparind, ref_inds), ]
    # rmd_references = paste0("# References\n\n",
    #                         "<div id=\"refs\"></div>")

    # https://pandoc.org/MANUAL.html
    # put into metadata field instead of div above
    rmd_references = "refs: |
   ::: {#refs}
   :::"

    if(reference_parsing == "anystyle"){

      ##################### anystyle  parsing #################
      print("runing anystyle on extracted references ...")
      ras = system(paste0("anystyle -w -f bib,json parse ",  path_temprefs_in , " ", path_temprefs_out))
      stopifnot("anystyle failed" =  ras == 0 )

    } else if(reference_parsing == "grobid"){


      ##################### grobid parsing #################
      #Example: curl -X POST -H "Accept: application/x-bibtex" -d "citations=Graff, Expert. Opin. Ther. Targets (2002) 6(1): 103-113" localhost:8070/api/processCitation



      for(cri in 1:length(references)){

        resp_bib = ""
        resp_xml = ""
        resp_text  = ""

        cref = references[cri]


        # check consolidation
        consolidate= "0"
        if(!is.null(grobid_consolidate) && grobid_consolidate == "grobidlv1"&& !(cri %in% grobid_consolidate) ) {
          consolidate = "1"
        } else if (!is.null(grobid_consolidate) && grobid_consolidate == "grobidlv2"){
          consolidate = "2"
        }

        res = httr::POST("localhost:8070/api/processCitation", body = list(citations = cref, consolidateCitations = consolidate),
                         encode = "form", httr::accept("application/xml")) # application/x-bibtex

        if(res$status_code > 204)  stop("grobid service reference parsing failed with error")
        if(res$status_code == 204) {
          hgl_warn(paste0("could not parse reference: ", cref))

        } else{

          # if not - hopefully successfull
          resp_text = httr::content(res, as="text")
          resp_xml = xml2::read_xml(resp_text)
          resp_bib = tei_xml_to_bib(resp_xml, orderindex = cri)

          cat(resp_text, file=paste0(path_temprefs_out, "/", cri, ".xml"))
          cat(resp_bib, file=paste0(path_temprefs_out, "/", cri, ".bib"))
        }
      }
    }
    else {
      stop("Unkown reference parsing mode/backend")
    }


    ############################ binding reference parsing results  ####################
    # red back into R using bib2df  and join to big file, do numerical sort
    temp_bibfiles_in = stringr::str_sort(paste0(path_temprefs_out, "/", list.files( path = path_temprefs_out, pattern = "*.bib")), numeric = T)


    # prepare for binding rows
    x = list()
    for(cn in temp_bibfiles_in){

      print(cn)
      x[[cn]] <- bib2df::bib2df(cn)



      # parse bibnum and index (index always awailable, number might fail)
      bib_orderindex = gsub(x = basename(cn), replacement = "", pattern = ".bib")
      bib_num <- NA

      # if parsing failed bib2df returns 0 length data. framec
      if(NROW(x[[cn]]) == 0){
        x[[cn]] <- data.frame(bibliography_orderindex = bib_orderindex)
      } else {

        parsed_citnums = NA
        parsed_citnums = ref_itemnums[as.numeric(bib_orderindex)] # needs to correspond to original index since filename corresponds to row index


        bib_num <- gsub(x = parsed_citnums, pattern = "[^0-9]", replacement = "") # only allow alphanumeric
      }

      x[[cn]]$BIBLIOGRAPHY_NUMBER = bib_num

      # cehck for duplicated column names in the data r
      clnames = colnames(x[[cn]])

      # this shoudl finud R double name suffixes (x.1, x.2, ...)
      cn_dubinds = which(stringr::str_detect(string = clnames, pattern = "\\.[0-9]+$"))

      for(cdi in cn_dubinds){


        # find the other colums
        ccname = stringr::str_replace(string = clnames[cdi], pattern = "\\.[0-9]+$", replacement =  "")

        dubinds = which(startsWith(clnames, ccname))

        # find longest contennt
        charlenghts = sapply(dubinds, \(i) nchar(x[[cn]][[clnames[i]]]))
        maxi = which(charlenghts == max(charlenghts))[1] # if both the same, take first one

        keepcol  = clnames[dubinds[maxi]]
        keepcoli = dubinds[maxi]

        todel = setdiff(dubinds, keepcoli)

        x[[cn]][, todel] = NULL

      }
    }
    d_refs = rbindlist(l = x, fill = TRUE) # fill with NA by specifying fill = 'TRUE'
    d_refs$BIBLIOGRAPHY_NUMBER = as.numeric(d_refs$BIBLIOGRAPHY_NUMBER)
    setkey(d_refs, BIBLIOGRAPHY_NUMBER) # sort

    # if("CITATION.NUMBER" %in% names(d_refs)){
    #   setnames(d_refs, "CITATION.NUMBER", "CITATION_NUMBER")
    # }



    # get rid of trailing letter appended by anystyle, store in new variable
    d_refs$CITEKEY =  stringr::str_replace_all(pattern = "((?<=[0-9]{4})[a-z]{1})|(-[a-z]{1})", string= d_refs$BIBTEXKEY, replacement = "")

    # check for duplicatees
    dups = unique(d_refs$CITEKEY[duplicated(d_refs$CITEKEY)])

    # append letters to duplicates - hopefully not more than  24
    for(cd in dups){
      stopifnot("more than 24 duplicates for one CITEKEY -> fix code" = d_refs[CITEKEY == cd, .N] < 25)
      d_refs[CITEKEY == cd, CITEKEY:= paste0(CITEKEY, letters[1:NROW(.SD)])]
    }

    # copy back citeky to bibtexkey - needed for df2bib to work correctly (takes the value of this for the citation key)
    d_refs$BIBTEXKEY = d_refs$CITEKEY


    # Do not delete the CITEKEY, otherwise CSL parsing in pandoc/latex will stop to work: d_refs$CITEKEY = NULL

    # write references.bib
    bib2df::df2bib(x = dplyr::as_tibble(d_refs), file = paste0(working_folder, "/", "references_autoparsed.bib"))


    ################# reference augmentation #############################

    # agumentation: based on existing or argument-provided id (doi or pmid), look up the record in the respective database using RefManagaR
    for(cri in 1:NROW(d_refs)){

      cref = d_refs[cri, ]
      # any specific augmentation specifications?
      ws_spec = d_augment_whitelist[internal_id == cref$BIBTEXKEY || internal_id == cref$BIBLIOGRAPHY_NUMBER]


      if(NROW(ws_spec) > 1) hgl_error(paste0("augmentation: id for ", cref$BIBTEXKEY, " not unique in document tbibliography"))

      doi  = if(!is.nullna(ws_spec$DOI)) ws_spec$DOI else  if(!is.nullna(cref$DOI)) cref$DOI else NULL
      pmid = if(!is.nullna(ws_spec$PMID)) ws_spec$PMID else  if(!is.nullna(cref$PMID)) cref$PMID else NULL
      be = NULL

      # set service based on availability of ids, if not already specified, with doi as first priority
      if(NROW(ws_spec) > 0 && is.na(ws_spec$service)){
        if(!is.null(doi)) ws_spec$service = "doi"
        else if(!is.null(pmid)) ws_spec$service = "pmid"
        else hgl_warn(paste0("augmentation: augmentation specified, but no valid existing id (PMID nor DOI ) found for ", cref$BIBTEXKEY))
      }


      # check if either explicitly specified (see argument parsing at the beginning of the fuction) or globally specified and not blacklisted
      if((!is.null(augment_global) && augment_global == "doi" && NROW(ws_spec) == 0) || (NROW(ws_spec) > 0  && !is.na(ws_spec$service) && ws_spec$service == "doi") ){

        if(is.null(doi)){
          hgl_warn(paste0("augmentation: doi augmentation specified , but no valid DOI found for ", cref$BIBTEXKEY))
        }
        else {
          be = RefManageR::ReadCrossRef(filter = list(doi=doi))
        }
      }
      else if((!is.null(augment_global) && augment_global == "pmid" && NROW(ws_spec) == 0) || (NROW(ws_spec) > 0  && !is.na(ws_spec$service) &&  ws_spec$service == "pmid" )){

        if(is.null(pmid)) {
          hgl_warn(paste0("augmentation: pubmed augmentation specified , but no valid PMID found for ", cref$BIBTEXKEY))
        }
        else {
          be = RefManageR::GetPubMedByID(pmid)
        }
      } # no augmentation otherwise

      if(!is.null(be)){

        bibstr = RefManageR::toBiblatex(be)

        fn = paste0(path_temprefs_out, "/augment_", cref$BIBLIOGRAPHY_NUMBER, ".bib")

        RefManageR::WriteBib(file = fn, bib = be)

        # read back in with bib2df, replace citekey
        row = bib2df::bib2df(fn)
        row$BIBTEXKEY = d_refs$BIBTEXKEY[cri]

        d_refs = overwrite_bib2df_row(nrow = row, row_index = cri, df = d_refs, non_existing_fields = "append")
      }


    }

    #we can
    #- manually provide bibtex file.show
    #- provide pubmed ids and dois for fetching addditional information and merging it to the parsed one
    #- provide dois for using this as only source of information and discarding the bibliography information

    # correction, augmentation, merging,consolidation, replacement, overwriting

    # write references.bib
    bib2df::df2bib(x = dplyr::as_tibble(d_refs), file = paste0(working_folder, "/", "references_augmented.bib"))

    ################## refernce correction ##############################
    # overwrite reference entries if file provided
    if(!is.null(reference_overwrite)){

      # read in (faisl if invalid file)
      if(file.exists(reference_overwrite)){
        d_crefs = bib2df::bib2df(reference_overwrite)
      } else if(file.exists(paste0(working_folder, "/../", reference_overwrite))){
        d_crefs = bib2df::bib2df(paste0(working_folder, "/../", reference_overwrite))
      }

      for(k in 1:NROW(d_crefs)){

        ccr = d_crefs[k,]

        # teh correction .bib file needs to have a CORRECTION_BN entry if match by BIBLIOGRAPHY_NUMBER is desired
        if(!is.null(ccr$CORRECTION_BN)){

          cbibnum = as.numeric(ccr$CORRECTION_BN)
          ccr$CORRECTION_BN = NULL # delete correction number  as to not copy it into updated bibtex entry

          # check if integer
          if(!is.na(cbibnum) && is.numeric(cbibnum) && cbibnum %% 1  == 0){

            cbibnum = as.integer(cbibnum)

            # attempt to find in d_res
            if(NROW(d_refs[BIBLIOGRAPHY_NUMBER == cbibnum]) > 1){
              hgl_error("non-unique BIBLIOGRAPHY_NUMBER in d_refs")
            }
            else if(NROW(d_refs[BIBLIOGRAPHY_NUMBER == cbibnum]) == 0){
              hgl_warn(paste0("reference_overwrite: provided BIBLIOGRAPHY_NUMBER '", cbibnum, " 'for correction not found among parsed references"))
            }
            else if(NROW(d_refs[BIBLIOGRAPHY_NUMBER == cbibnum]) == 1){

              # get row index and update
              rri = which(d_refs$BIBLIOGRAPHY_NUMBER == cbibnum)
              d_refs = overwrite_bib2df_row(nrow = ccr, row_index = rri, df = d_refs, non_existing_fields = "fail")
            }


          } else {
            hgl_warn("reference correction: Non-integer value in BIBLIOGRAPHY_NUMBER field")
          }
        }
        else {

          # attempt to match by CITEKEY
          btxkey = ccr$BIBTEXKEY

          if(NROW(d_refs[BIBTEXKEY == btxkey]) > 1){
            hgl_error("non-unique BIBTEXKEY in d_refs")
          }
          else if(NROW(d_refs[BIBTEXKEY == btxkey]) == 0){
            hgl_warn(paste0("reference_overwrite: provided BIBTEXKEY '", btxkey, " 'for correct not found among parsed refernces"))
          } else if(NROW(d_refs[BIBTEXKEY == btxkey]) == 1){

            # get row index and update
            rri = which(d_refs$BIBTEXKEY == btxkey)
            d_refs = overwrite_bib2df_row(nrow = ccr, row_index = rri, df = d_refs, non_existing_fields = "fail")
          }
        }
      }
    }

    bib2df::df2bib(x = dplyr::as_tibble(d_refs), file = paste0(working_folder, "/", "references.bib"))

    # find all in text citatations - idea: three possible modes: text based detection using regex '(' or '[' brackets, field code based detection using xpath, or text based detection using citation keys (this would rquire providing a specializied citation style or hand- in of a bib file)
    # "========protectedcite{}protectedcite
    # do not parse references inside figures and table tags
    ttext = trimws(doc_summar$text)
    content_inds = setdiff(1:NROW(doc_summar), which(startsWith(ttext, "[[table") | startsWith(ttext, "[[figure")))

    # number based detection
    y = stringr::str_extract_all(str = doc_summar$text[content_inds], pattern = "\\[(([0-9]+([-–][0-9]+)?)(?:, ?)?)+\\]") # TODO: does not allow for spaces currently and is rather strict, possibly handle that in the future

    # save original intext citations, seperate by ', '
    doc_summar$orig_citations[content_inds] = lapply(y, FUN = paste, sep = ", ", collapse = ", ")


    doc_summar$text[content_inds] = intextrefnums_to_citekeys(doc_summar$text[content_inds], d_refs)

    # write to xml, and mrkdwn, but before also
    # postprocess JATS xml references
    doc_summar$mrkdwn = doc_summar$text
    tres = process_xml_intext_citations(text = doc_summar$text, d_refs =  d_refs)

    doc_summar$xml_temp = tres$xml_text
    d_xmlintext_cites = tres$d_intextcites
  }
  else {

    ############## passthrough mode ###########################
    # passthrough mode where word citations are not parsed and directly put into latex/rmd. NOt compatible with JATS generation
    # write to xml before latex-format-specific changes
    # operate on markdown  here

    # create column to contain markdown tex, and xml text
    doc_summar$mrkdwn = doc_summar$text
    doc_summar$xml_temp = doc_summar$text

    if(is.numeric(ref_inds[1])){

      refs <- references

      # extract marker 1., 2., .... using regex
      markers <- trimws(stringr::str_extract(string = refs, pattern = "^[0-9]+\\."))

      if(any(is.na(markers)) && guess_refnumbers == TRUE){
        markers = as.character(1:length(refs))
      }

      refs <- trimws(stringr::str_replace(string = refs, pattern = "^[0-9]+\\.", replacement = ""))

      # delete markers from refs
      stopifnot("number of found reference numbers in bibliography not identical to number of references, check list" = length(markers) == length(refs))


      # list for safeguarding url from doi parsing
      refdoi_safeguards_latex = list()
      refdoi_safeguards_html = list()

      if(doi_parsing){


        # pattern from qdapRegex
        refs = stringr::str_replace_all(string = refs,
                                        pattern = stringr::regex("\\b(?<!/)10[.]\\d{4,9}/[-._;()/:A-Z0-9]+\\b", ignore_case = T),
                                        replacement = function(m){

                                          r = gsub(pattern = "\\.$", replacement = "", x = m)
                                          rl = paste0("\\href{https://doi.org/", r,"}{", r, "}.")
                                          rh = paste0("<a href='https://doi.org/", r,"'>", r, "</a>.")


                                          refdoi_safeguards_latex <<- as.list(rl)
                                          refdoi_safeguards_html <<- as.list(rh)

                                          paste0("____________refdoi", 1:length(r), "refdoi____________")
                                           })#"\\\\url{\\0}"
      }

      # list for safeguarding url from url parsing
      refurl_safeguards_latex = list()
      refurl_safeguards_html = list()

      if(url_parsing){

        # pattern from qdapRegex, second one from stackoverflow
        refs = stringr::str_replace_all(string = refs,
                                        #pattern = stringr::regex("(((https?|ftps?)://)|(www\\.))(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?", ignore_case = T),
                                        pattern = "(((http|ftp|https):\\/\\/)?([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:\\/~+#-]*[\\w@?^=%&\\/~+#-]))+", # try another one from  https://stackoverflow.com/questions/6038061/regular-expression-to-find-urls-within-a-string
                                        replacement = function(m){

                                          r = gsub(pattern = "\\.$", replacement = "", x = m)
                                          rl  = paste0("\\url{", r,"}")
                                          rh  = paste0("<a href='", r,"'>", r,"</a>")


                                          refurl_safeguards_latex <<- as.list(rl)
                                          refurl_safeguards_html <<- as.list(rh)

                                          paste0("____________refurl", 1:length(r), "refurl____________")

                                        })#"\\\\url{\\0}"


      }

      refs_html = refs
      refs_latex = refs

      # put back reference urls
      if(length(refurl_safeguards_latex) > 0){
        for(i in 1:length(refurl_safeguards_latex)){
          # fill back in
          refs_latex = stringr::str_replace(string = refs_latex, pattern =stringr::fixed(paste0("____________refurl",i,"refurl____________")), replacement  = refurl_safeguards_latex[[i]])
          refs_html  = stringr::str_replace(string = refs_html, pattern =stringr::fixed(paste0("____________refurl",i,"refurl____________")), replacement  = refurl_safeguards_html[[i]])
        }
      }
      # put back doi urls
      if(length(refdoi_safeguards_latex) > 0){
        for(i in 1:length(refdoi_safeguards_latex)){
          # fill back in
          refs_latex = stringr::str_replace(string = refs_latex, pattern =stringr::fixed(paste0("____________refdoi",i,"refdoi____________")), replacement  = refdoi_safeguards_latex[[i]])
          refs_html  = stringr::str_replace(string = refs_html, pattern =stringr::fixed(paste0("____________refdoi",i,"refdoi____________")), replacement  = refdoi_safeguards_html[[i]])
        }
      }

      # latex: escape &, _ and [,], # . Some of these require double escaping
      refs_latex = stringr::str_replace_all(refs_latex, pattern = "&", replacement = "\\\\&")
      refs_latex = stringr::str_replace_all(refs_latex, pattern = "\\[", replacement = "{[}")
      refs_latex = stringr::str_replace_all(refs_latex, pattern = "\\]", replacement = "{]}")
      refs_latex = stringr::str_replace_all(refs_latex, pattern = "_", replacement = "\\\\_")
      refs_latex = stringr::str_replace_all(refs_latex, pattern = "\\#", replacement = "\\\\#")

      # html - escape chars

      # remove dots
      markers <- stringr::str_replace(string = markers, pattern = "\\.", replacement = "")

      reftex = "\\small" %+% gen_list_latex(items = refs_latex, label = "{[_]}", markers = markers,
                                                       options = c("labelindent" = "0em",
                                                                   "labelwidth" = "2.4em",
                                                                   "align" = "left" ,
                                                                   "leftmargin" = "2.7em")) %+% ""
      refhtml = gen_list_html(items = refs_html, markers = markers)



      #rmd_references =
      yaml_references = yaml::as.yaml(list(refs_latex = reftex, refs_html = refhtml))


      doc_summar = doc_summar[-c(refparind, ref_inds),]


    }
  }

  return(list(doc_summar = doc_summar, yaml_references = yaml_references, d_xmlintext_cites = d_xmlintext_cites))
}
