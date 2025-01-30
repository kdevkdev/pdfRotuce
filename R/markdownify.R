#' Converts word .docx files to .Rmd files that can be rendered to PDFs using latex
#'
#' @import data.table
#' @param src_docx
#' @param working_folder
#' @param meta_csv
#' @param reference_parsing Default `FALSE`. IF `FALSE`, copy references as is without automated parsing. 'anystyle' or 'grobid' use the respective backends. Can also take the name of a .bib file to use to genrate the bibliography. In that case, in-text citations are expected to follow the '[@citekey1, @citekey2..]' syntax with alphanumeric plus - and _ being valid chars for citekeys,a nd ',' and ';' being valid chars for seperating listed citekeys. In that case, a bibliography in the mansucript file will be disregarded.
#' @param reference_correction Only has effect if `reference_parsing` is not `FALSE`. Path to bibtex .bib file that contains manual overrides for references. Matching with entries in file: Recommended (Second priority) is automatically generated citeky, normally 'author'+ 'year', see 'working_path/references.bib' or files in the 'build/tempreftxts_out'. 1st priority is nonstandard bibtex field 'CORRECTION_BN', corresponding to the list item number in the refernces and in-text citation number. If a correction bibfile is provided, 'working_path/references.bib' will be altered compared to 'working_path/refernces_autoparses.bib'. If the file is not found at the specified path, the parent level of the working folder will also be checked (to cover the normal case where intermediate files are placed in the working path 'build' directory, and input files reside at the parent of the 'build' directory)
#' @return
#' @export
#'
#' @examples
markdownify = function(src_docx, doc_folder, working_folder = ".",
                       meta_csv = NULL,
                       rmd_outfile = NULL,
                       xml_outpath = NULL,
                       reference_parsing = F,
                       reference_correction = NULL,
                       refparser_inject = NULL, # format: (@citekey|BIBLIOGRAPHY_NUM)=text, similar to override (and some to augment), but before the parsing stage. Could be used to fuzzy match without doi, or parse bibliography items of reports ...
                       consolidate_grobid = "grobidlv1",# grobidlv1, grobidlv2
                       consolidate_blacklist = NULL, # only has effect if consolidation_global is provided
                       augment_global = NULL, # pmid, or doi
                       augment_whitelist = NULL, # format: (@citekey|BIBLIOGRAPHY_NUM)=(doi|pmid):id
                       augment_blacklist = NULL,
                       url_parsing = T, doi_parsing = T, guess_refnumbers = T){

  # a4: 210, 297, 15 mm left/right margin, 12.5 top/bottom
  type_width = 180
  type_height = 272

  fig_capts =
    tab_capts = c()


  if(!is.null(consolidate_blacklist)) consolidate_blacklist = stringr::str_split(consolidate_blacklist, pattern = ",")
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


  # read file and backup object
  docx = officer::read_docx(src_docx)
  doc_summar_o = doc_summar  = data.table::as.data.table(officer::docx_summary(docx,preserve = T,remove_fields = T))
  doc_summar[is.na(style_name), style_name := ""]
  doc_summar[, texto := text]

  # data table to temporariliy store xml intext citations
  d_xmlintext_cites = NULL


  # get all l1 headings
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  title_page_inds = l1_inds[1]:(l1_inds[2]-1)

  # find and parse titele page - first heading 1 up to second heading 1
  parsed_meta = parse_title_page(doc_summar[title_page_inds,])

  # predefined metadata
  predef_meta  = list()


  # get metadata from csv
  if(!is.null(meta_csv)){

    pd_tab = read.csv(file = meta_csv, header = F, fileEncoding = "UTF-8")

    # values to a vector and name to vector names
    tvals = pd_tab[[2]]
    names(tvals) = pd_tab[[1]]


    # copy over provided values, uppercase for article typer
    if(is.element("articledates", names(tvals)))             predef_meta$articledates              = tvals["articledates"]                   else hgl_warn("'articledates' missing in meta csv")
    if(is.element("volume", names(tvals)))                   predef_meta$volume                    = tvals["volume"]                         else hgl_warn("'volume' missing in meta csv")
    if(is.element("issue",  names(tvals)))                   predef_meta$issue                     = tvals["issue"]                          else hgl_warn("'issue' missing in meta csv")
    if(is.element("string_volumeissue",  names(tvals)))      predef_meta$string_volumeissue        = tvals["string_volumeissue"]             else hgl_warn("'string_volumeissue' missing in meta csv")
    if(is.element("copyright_year", names(tvals)))           predef_meta$copyright_year            = tvals["copyright_year"]                 else hgl_warn("'copyright_year' missing in meta csv")
    if(is.element("doi", names(tvals)))                      predef_meta$doi                       = tvals["doi"]                            else hgl_warn("'doi' missing in meta csv")
    if(is.element("pageheader", names(tvals)))               predef_meta$pageheader                = tvals["pageheader"]                     else hgl_warn("'pageheader' missing in meta csv")
    if(is.element("has_abstract", names(tvals)))             predef_meta$has_abstract              = tvals["has_abstract"]                   else hgl_warn("'has_abstrat' missing in meta csv")
    if(is.element("article_type", names(tvals)))             predef_meta$article_type              = toupper(tvals["article_type"])          else hgl_warn("'article_type' missing in meta csv")
    if(is.element("string_corresponding", names(tvals)))     predef_meta$string_corresponding      = tvals["string_corresponding"]           else hgl_warn("'string_corresponding' missing in meta csv")

    if(startsWith(predef_meta$doi,"https://doi.org/") || startsWith(predef_meta$doi,"http://doi.org/")){

      # convert form url to doi if needed
      # for url start
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "^(https?://)?doi\\.org/", replacement = "")

      # for url end
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "/$", replacement = "")
    }


    # if(is.null(parsed_meta$abstracts$mainlang) && predef_meta$has_abstract == "yes"){
    #   stop("abstract mandatory according to metadata.csv, but is not provided")
    # }
  }
  hardcoded_meta = gen_hardcoded_meta(reference_parsing = reference_parsing)



  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta, hardcoded_meta)

  yaml_preamble = gen_yaml_header(md =metadata, reference_parsing = reference_parsing)


  # delete  title page from document
  doc_summar = doc_summar[-title_page_inds,]

  # remove [empty] headings
  doc_summar = doc_summar[!(startsWith(tolower(style_name), "heading") & trimws(tolower(text)) == "[empty]"),]




  ## @@@@ doc_sumar will contain: text (original word contents), mrkdwn  (processed markedown), xml_temp (intermediate 'working' xml text ), xml_text (final xml conform text

  # create xml nodes list for JATS xml, special format in that it is embedded into the doc dataframe, needs to have same length as NROW
  # that is, each row as a cell/list entry for corresponding xml nodes
  doc_summar$xml = vector(mode = "list", length = NROW(doc_summar))
  doc_summar$xml_text = ""
  doc_summar$xml_type = character()
  doc_summar$xml_type = NA

  ################## reference parsing ##################################
  # find,parse,  and remove references
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  refparind = which(tolower(doc_summar$text) == 'references' & tolower(doc_summar$style_name) == "heading 1")
  ref_inds = NULL


  rmd_references = ""
  if(length(refparind) == 0)   hgl_warn("No references found in manuscript!!!")
  else{

    # refinds are those paragraphs until end or next l1 heading
    ti = which(l1_inds > (refparind+1)) #+1 because we donot want to have the heading included


    if(length(ti) == 0) lri = NROW(doc_summar) # no subsequent l1 heading, we go until end
    else lri = l1_inds[ti] -1 #deduct 1 because we do not want to have the next level 1 heading included

    ref_inds = (refparind+1):lri # do not include the starging l1 heading , +1

    # create one large text value
    #references_text = paste(doc_summar$text[ref_inds], collapse = "\n")
    references =  doc_summar$text[ref_inds]
  }

  path_temprefs_in  = paste0(working_folder, "/tempreftxts_in")
  path_temprefs_out = paste0(working_folder, "/tempreftxts_out")
  path_xml_filepack_dir = paste0(working_folder, "/jats_xml_filepacking")

  if(dir.exists(path_temprefs_in)) unlink(path_temprefs_in, recursive = T)
  if(dir.exists(path_temprefs_out)) unlink(path_temprefs_out, recursive = T)

  # reset xml filepackaging directory
  if(dir.exists(path_xml_filepack_dir)) unlink(path_xml_filepack_dir, recursive = T)
  dir.create(path_xml_filepack_dir)

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

      browser()
      doc_summar = doc_summar[-c(refparind, ref_inds), ]
    }

    # put placeholder for CLS references into the RMD text
    rmd_references = paste0("# References\n\n",
                            "<div id=\"refs\"></div>")


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
    ref_itemnums = stringr::str_extract(string = references, pattern = "^[0-9]+[.]")
    references = stringr::str_replace(string = references, pattern = "^[0-9]+[.]", replacement = "")

    if(!is.null(refparser_inject)){


    }


     # save single .txt for each reference - do it this way to keep track of the ordering in of original bibliography as much as possible
    sapply(1:length(references), FUN = \(x) { writeLines(text = references[x], con = paste0(path_temprefs_in, "/", x, ".txt")) })


    # remove from document
    doc_summar = doc_summar[-c(refparind, ref_inds), ]
    rmd_references = paste0("# References\n\n",
                            "<div id=\"refs\"></div>")

    if(reference_parsing == "anystyle"){

      ##################### anystyle  parsing #################
      print("runing anystyle on extracted references ...")
      ras = system(paste0("anystyle -w -f bib,json parse ",  path_temprefs_in , " ", path_temprefs_out))
      stopifnot("anystyle failed" =  ras == 0 )

    } else if(reference_parsing == "grobid"){

      ##################### grobid parsing #################
      #Examble: curl -X POST -H "Accept: application/x-bibtex" -d "citations=Graff, Expert. Opin. Ther. Targets (2002) 6(1): 103-113" localhost:8070/api/processCitation



      for(cri in 1:length(references)){

        resp_bib = ""
        resp_xml = ""
        resp_text  = ""

        cref = references[cri]


        # check consolidation
        consolidate= "0"
        if(!is.null(consolidate_grobid) && consolidate_grobid == "grobidlv1"&& !(cri %in% consolidate_blacklist) ) {
          consolidate = "1"
        } else if (!is.null(consolidate_grobid) && consolidate_grobid == "grobidlv2"){
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

    # write references.bib
    bib2df::df2bib(x = dplyr::as_tibble(d_refs), file = paste0(working_folder, "/", "references_augmented.bib"))

    ################## refernce correction ##############################
    # overwrite reference entries if file provided
    if(!is.null(reference_correction)){

      # read in (faisl if invalid file)
      if(file.exists(reference_correction)){
        d_crefs = bib2df::bib2df(reference_correction)
      } else if(file.exists(paste0(working_folder, "/../", reference_correction))){
        d_crefs = bib2df::bib2df(paste0(working_folder, "/../", reference_correction))
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
              hgl_warn(paste0("reference_correction: provided BIBLIOGRAPHY_NUMBER '", cbibnum, " 'for correction not found among parsed references"))
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
            hgl_warn(paste0("reference_correction: provided BIBTEXKEY '", btxkey, " 'for correct not found among parsed refernces"))
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
  else { # end condition refernce_parsing is not FALSE -> passthrough mode where word citatiosn are not parsed and directly put into latex/rmd. NOt compatible with JATS generation

    # write to xml before latex-format-specific changes
    # operate on markdown  here

    # create column to contain markdown tex, and xml text
    doc_summar$mrkdwn = doc_summar$text
    doc_summar$xml_temp = doc_summar$text

    # do not change in text citations
    if(is.numeric(ref_inds[1])){

      #doc_summar$mrkdwn[ref_inds] = paste0("\\setlength{\\parindent}{0em}\n", doc_summar$mrkdwn[ref_inds])
      refs <- doc_summar$mrkdwn[ref_inds]

      # remove empty paragraphs
      refs <- refs[!grepl(x = refs, pattern = "^\\s?$")]

      # extract marker 1., 2., .... using regex
      markers <- trimws(stringr::str_extract(string = refs, pattern = "^[0-9]+\\."))

      if(any(is.na(markers)) && guess_refnumbers == TRUE){
        markers = as.character(1:length(refs))
      }

      refs <- trimws(stringr::str_replace(string = refs, pattern = "^[0-9]+\\.", replacement = ""))

      # delete markers from refs
      stopifnot("number of found reference numbers in bibliography not identical to number of references, check list" = length(markers) == length(refs))

      # escape &, _ and [,]
      refs = stringr::str_replace_all(refs, pattern = "&", replacement = "\\\\&")
      refs = stringr::str_replace_all(refs, pattern = "\\[", replacement = "{[}")
      refs = stringr::str_replace_all(refs, pattern = "\\]", replacement = "{]}")
      refs = stringr::str_replace_all(refs, pattern = "_", replacement = "\\\\_")

      # remove dots
      markers <- stringr::str_replace(string = markers, pattern = "\\.", replacement = "")



      # [] need to be grouped in latex optional options bec of parsing (https://tex.stackexchange.com/questions/84595/latex-optional-arguments-with-square-brackets)
      #       doc_summar$mrkdwn[ref_inds[1]] = gen_list(items = refs, label = "{[_]}", markers = markers,
      #                                                 options = c("labelindent" = "0em", "labelwidth" = "2em", "align" = "left"))


      # https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
      #[-a-zA-Z0-9@:%._\+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b([-a-zA-Z0-9()@:%_\+.~#?&//=]*)

      #       refs = stringr::str_replace_all(string = refs,
      #                                pattern = "[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)",
      #                                replacement = "\\\\url{\\0}")

      if(url_parsing){

        # pattern from qdapRegex
        refs = stringr::str_replace_all(string = refs,
                                        pattern = stringr::regex("(((https?|ftps?)://)|(www\\.))(-\\.)?([^\\s/?\\.#-]+\\.?)+(/[^\\s]*)?", ignore_case = T),
                                        replacement = function(m){

                                          r = gsub(pattern = "\\.$", replacement = "", x = m)
                                          r  = paste0("\\url{", r,"}.")
                                          r
                                          #browser()
                                        })#"\\\\url{\\0}"

      }


      if(doi_parsing){


        # pattern from qdapRegex
        refs = stringr::str_replace_all(string = refs,
                                        pattern = stringr::regex("\\b(?<!/)10[.]\\d{4,9}/[-._;()/:A-Z0-9]+\\b", ignore_case = T),
                                        replacement = function(m){

                                          r = gsub(pattern = "\\.$", replacement = "", x = m)
                                          r  = paste0("\\href{https://doi.org/", r,"}{ ", r, "}.")
                                          r
                                          #browser()
                                        })#"\\\\url{\\0}"
      }

      rmd_references = "# References\n\n\\small" %+% gen_list(items = refs, label = "{[_]}", markers = markers,
                                                              options = c("labelindent" = "0em", "labelwidth" = "2.4em", "align" = "left" , "leftmargin" = "2.7em"))

      doc_summar = doc_summar[-c(refparind, ref_inds),]


      # remove from document
      #rem_inds = ref_inds[-1]
      # if(length(rem_inds) > 0){
      #   doc_summar = doc_summar[-c(refparind, rem_inds),]
      # }
    }
  }




  ############## headings ##############
  # there seems to be a problem with case sensitivity and style names between libreoffice and word, use tolowe (word uses capital, libreoffice not)
  # doc_summar[tolower(style_name) == "heading 1" & startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", substr(mrkdwn, 2, nchar(mrkdwn)), " {-}")] # do not put in TOC
  # convert headings to markdown up to level 5
  doc_summar[tolower(style_name) == "heading 1", mrkdwn := paste0("# ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 2", mrkdwn := paste0("##  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 3", mrkdwn := paste0("###  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 4", mrkdwn := paste0("####  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 5", mrkdwn := paste0("#####  ", mrkdwn)]
  doc_summar[startsWith(tolower(style_name),"heading"), xml_type:= "heading"]


  # parse headings to xml sections and store into doc_summar
  r = gen_xml_sections(doc_summar)
  doc_summar$xml_secopen = r$hxml_opentags
  doc_summar$xml_secend  = r$hxml_endtags

  # to debug
  # xml2::read_xml(paste0("<document>", paste0(hxmltags, collapse = ""), "</document>")) |> xml2::as_list()
  # cbind(headi, hxmltags)

  ############## process tables ##############
  cudoc_tabinds = unique(doc_summar[content_type == "table cell"]$doc_index)

  tab_counter = 1 # we keep a specific ounter, if ever cti would deviate because e.g. of failure

  for(cti in cudoc_tabinds){


    # detect empty paragraphs ahead
    empties = doc_summar[, doc_index > cti & !grepl(x = mrkdwn, pattern = "$\\s?^")]
    ind_next_nonempty = min(which(empties)) # first non-empty

    tab_opts_raw = doc_summar[ind_next_nonempty,]

    # check if table is acutally there
    if(tab_opts_raw$content_type != "paragraph" && !startsWith(trimws(tab_opts_raw$mrkdwn), "[[table")){
      hgl_warn("Table " %+% cti %+% " does not seem to have have a table tag")
      tab_opts_raw = NULL
      tab_opts = NULL
    } else{
      tab_opts  = parse_yaml_cmds(trimws(tab_opts_raw$mrkdwn))
    }

    ct_dat = doc_summar[content_type == "table cell" & doc_index == cti]

    # if we need we can take into account the header here (is_header column in doc_summar)
    ct_csv = data.table::dcast(ct_dat, row_id ~ cell_id, value.var = "mrkdwn")[,-1] # not first


    ctab_chunk = gen_tabchunk(ct_csv = ct_csv,
                              tab_opts = tab_opts,
                              tab_counter = tab_counter,
                              folder = working_folder)

    ctab_xml = gen_xml_table(ct_csv = ct_csv,
                             tab_opts = tab_opts,
                             tab_counter = tab_counter)


    # delete table rows and table options form document structure
    before = doc_summar[doc_index < cti]

    if(!is.null(tab_opts_raw)){

      after = doc_summar[doc_index > cti & doc_index != tab_opts_raw$doc_index]
    } else{
      after = doc_summar[doc_index > cti]
    }

    doc_summar = data.table::rbindlist(l = list(before,
                                           data.table::data.table(doc_index = cti, content_type = "paragraph", text = ctab_chunk,
                                                                  xml_text = ctab_xml,
                                                                  mrkdwn = ctab_chunk, is_heading1 = F, is_header = F, xml_type = "table"),
                                           after), fill = T)

    tab_counter = tab_counter +1

  }
  # write to file


  chunk_setup = "```{r SetupUpset,include=F}

#  source('rabul.R')

```"

  ########################################### command parsing ####################################################
  # list for inline math formulas
  list_inlinemath = list()


  # inline paragraph commands - do not need escaping
  # put protected dollar fo rinline mathc(
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", replacement = "========protecteddollar========\\1========protecteddollar========")]


  # not entirely R style, but use global assigment to easily populate list based on regex pattern recognition
  doc_summar[, xml_temp:= stringr::str_replace_all(string = xml_temp, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]",
                                                   replacement = function(x){
                                                   len = length(list_inlinemath)+1

                                                   # saldy need to run regex again - groups not provided
                                                   latex = stringr::str_extract(string = x, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", group = 1)

                                                   list_inlinemath[[len]] <<- data.table(latex = latex, index = len)
                                                   paste0("========protectedinlinemath", len, "========") })]

  d_inlinemath = rbindlist(l = list_inlinemath)

  # put protected at for refs
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\@ref\\((.+?)\\)", replacement = "\\========protectedat========ref(\\1)")]

  # TODO: implement global table and figure lits for JATS XML, see todo

  # noindent
  doc_summar[, mrkdwn:= gsub(x = trimws(mrkdwn), pattern = "^\\[\\[noindent\\]\\](.?)", replacement = "\\\\noindent \\1")]
  # JATS XML 1.3: indent likely not supported, but up to display tools

  # look for commands whole para tag commands [[]]
  command_inds =  which(startsWith(trimws(doc_summar$mrkdwn), "[[") & endsWith(trimws(doc_summar$mrkdwn),"]]"))


  # escape dollar etc in suitable paragraphs
  doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn :=rmd_char_escape(mrkdwn)]

  # doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\$", replacement = "\\\\$")]
  # doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\@", replacement = "\\\\@")]
  # doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\%", replacement = "\\\\%")]
  fig_counter = 1

  # store commands so we can detect the previous one
  command_list = list()
  cii = 0

  if(length(command_inds) > 0 ){

    for(c_comi in command_inds){

      # increment
      cii = cii +1

      # get current command
      c_comtext = doc_summar[c_comi]$mrkdwn

      c_command = parse_yaml_cmds(c_comtext)

      command_list[[cii]] = list(command = c_command, index = c_comi)

      #print(c_comtext |> substr(1,100))




      # very primitive command parsing first position contains command
      c_result = ""
      c_result_xml = ""
      c_xml_type = "command"


      if(length(c_command) > 0 & !inherits(c_command, "error")){

        switch(c_command[[1]],
               interbox={
                 #print("interbox detected")
                 title = c_command['title']
                 text = c_command['text']
                 iblabel = paste0( "Com", c_comi)
                 c_result = "
::: {.interbox data-latex=\"{"%+% title %+% "}{" %+% iblabel %+% "}\"}
" %+% text %+% "
:::

"
              c_result_xml = paste("<boxed-text position='anchor' content-type='infobox'>",
                                "<caption>",
                                 "<title>", title, "</title>",
                                "</caption>",
                                paste0("<p>",text,"</p>"),
                                paste0( "<xref ref-type='aff' rid='ibox_", iblabel  ,"'/>"),
                               "</boxed-text>", sep = "\n")
                 },
               figure={
                 #print("figure detected")
                 # src = c_command['src']
                 # cap = c_command['cap']


                 c_result = gen_figblock(fig_opts = c_command, fig_counter = fig_counter)
                 c_xml_type = "figure"
                 c_result_xml = gen_xml_figure(fig_opts = c_command, fig_counter = fig_counter, xml_filepack_dir = path_xml_filepack_dir, base_folder = doc_folder)
                 fig_counter = fig_counter +1
                 #
                 #                  c_result = "```{r "%+% flabel %+%",out.width='100%',echo=F, fig.align='center',fig.cap='(ref:cap" %+% flabel %+%")'}
                 # knitr::include_graphics(path='" %+% src %+% "')
                 # ```
                 # "

               },
               math={
                 #print("Math detected")
                 formula = c_command['form']
                 c_result = paste0("$$", trimws(formula), "$$")
                 c_result_xml = gen_xml_displaymath(latex = trimws(formula))
                 c_xml_type = "display_math"

               },
               quote={
                 #print("quote detected")

                 text = rmd_char_descape(trimws(c_command['text'])) # needs double escape to pass through rmarkdown preprocessing
                 source = if(!is.null(c_command[['src']]) && is.character(c_command[['src']])) trimws(c_command['src']) else ""

                 # determine spacing based on whether the last pargraph was also a quote
                 if(cii > 1 && command_list[[cii-1]]$command[[1]] == "quote" && command_list[[cii-1]]$index  == c_comi -1){
                   vskip = "\\vspace{-4mm}"
                 } else {
                   vskip = "\\vspace{-1mm}"
                 }

                 c_result = vskip %+% "
::: {.displayquote data-latex=\"{  }\"}
::: {.enquote data-latex=\"{\\textit{" %+% text %+% "}} " %+% source %+% "\"}
\\phantom{}
:::
:::
\\vspace{-5.5mm}
"
                 c_result_xml = paste("<disp-quote>",
                 "<preformat>",text,"</preformat>",
                 paste0("<attrib>", source, "</attrib>"),
                 "</disp-quote>", sep = "\n")

                 },
               table={
                  hgl_warn("Unparsed (superflous?) table tag without table")
               },
               columnbreak = {
                 c_result = "\\columnbreak"},
               pagebreak = {
                 c_result = paste("\n```{=latex}",
                                   "\\end{multicols}",
                                   "\\newpage",
                                   "\\begin{multicols}{2}", sep = "\n",
                                   #"\\raggedcolumns",
                                   "```\n")},
               {
                 # default
                   stop(paste0("unkown command '", c_comtext, "'"))
               })
      }
      else{

        print("element arguments: yaml parsing error:")
        #print(c_comtext)
      }

      # replace the paragraph with the result
      doc_summar[c_comi]$mrkdwn   = c_result
      doc_summar[c_comi]$xml_text = c_result_xml # this needs to be finalized xml
      doc_summar[c_comi]$xml_type = c_xml_type

    }
  }


  # parse to xml  nodes that have not already been prossesd
  doc_summar[is.na(xml_type), `:=`(xml_type = "paragraph", xml_text = gen_xml_paragraphs(xml_temp,d_inlinemath ))]



  ############ statements and declarations ############
  rmd_statements = ''

  tll = list()
  for(ca in metadata$authors){

    if(!is.null(ca$contrib_roles)){

        tll[[length(tll)+1]] =paste0(ca$name, ": ", paste0(ca$contrib_roles, collapse = ", "), ".")
    }
  }

  indiv_author_contribs = ""
  if(length(tll) > 0 ){
    indiv_author_contribs = paste0(tll, collapse = " ")
  }

  # any statements? populate corresponding article part, each with its own subheading
  consumed_indiv_author_contribs = FALSE
  if(length(metadata$statements) > 0){

    rmd_statements = "\n\n# Declarations\n\n"
    for(cn in names(metadata$statements)){

      cstat <- metadata$statements[[cn]]
      rmd_statements = rmd_statements %+% "## " %+%cn %+% "\n" %+% "\\noindent " %+% cstat

      if(tolower(trimws(cn)) == "contributions" || tolower(trimws(cn)) == "author contributions" || tolower(trimws(cn)) == "author's contributions"|| tolower(trimws(cn)) == "authors' contributions"){
         consumed_indiv_author_contribs = TRUE
         rmd_statements = paste0(rmd_statements, "\n", indiv_author_contribs)

      }
      rmd_statements = rmd_statements %+% "\n\n"
    }

  }

  # author speciffic contributions not yet put into rmd
  if(!consumed_indiv_author_contribs) {

    rmd_statements = rmd_statements %+% "## Author contributions \n\n" %+% indiv_author_contribs %+% "\n\n"
  }



  ############ other language abstracts ############
  rmd_multilang_abstracts = ""
  if(any(names(metadata$abstracts)!="mainlang")){

    # figure out what additional languages there are (remove mainlang, usually english)
    side_langs = setdiff(names(metadata$abstracts),"mainlang")

    side_langs_abstracts = metadata$abstracts[side_langs]




    rmd_multilang_abstracts = "\n\\vspace{2mm}"
    first <- TRUE

    for(can in names(side_langs_abstracts)){

      # all abstracgt parts
      ca_parts = side_langs_abstracts[[can]]$parts

      # get article title in current language to add to abstract
      article_lang_title = side_langs_abstracts[[can]]$title


      cab_tit = toupper(switch(can, "", "es" = "RESUMEN")) # Resumén. Generate abstract term for each language, default empty

      if(first){ # but horizontal rule
        #rmd_multilang_abstracts = rmd_multilang_abstracts %+% "{\\noindent\\color{jchsheadercolor}\\rule{\\textwidth}{1.6pt}}\n"
        rmd_multilang_abstracts = rmd_multilang_abstracts %+% "\\begin{tcolorbox}[colframe=jchslightorange, colback=jchslightorange, sharp corners,boxsep=4mm,top=3.5mm,left=3.8mm,right=2.0mm]\\sffamily\n"
        first <- FALSE
      }



      if(!is.null(article_lang_title) && !is.na(article_lang_title) && article_lang_title != ""){

        rmd_multilang_abstracts = rmd_multilang_abstracts %+% "{\\bfseries \\vskip 0mm" %+% article_lang_title %+% "}\\vskip 3mm"
      }
      rmd_multilang_abstracts = rmd_multilang_abstracts %+% "{\\raggedright\\bfseries " %+% cab_tit %+% "}\\vskip 1mm"


      # go through paragraphs/parts
      for(cp in ca_parts){

        rmd_multilang_abstracts = rmd_multilang_abstracts %+% "{\\bfseries " %+% cp$title %+% "} " %+% cp$text %+% "\n\n"
      }
    }

    csidelang = gsub("abstract_", "", x = can)

    if(!is.null(metadata$attributes[[paste0("keywords_", csidelang)]])){

      keywordstitle = ""
      if(csidelang == "es"){
        keywordtitle = "{\\raggedright\\bfseries Palabras clave: }"
      }
      rmd_multilang_abstracts = rmd_multilang_abstracts %+% "\\vskip 3mm" %+% keywordtitle
      rmd_multilang_abstracts = rmd_multilang_abstracts %+% metadata$attributes[[paste0("keywords_", csidelang)]]
    }

    rmd_multilang_abstracts = rmd_multilang_abstracts %+% "\\end{tcolorbox}\\vspace{3mm}\n\n"

  }



  ########################################### postprocessing  & writing file ####################################################
  # replace back protected dollars , @, etc
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "========protecteddollar========", replacement = "$")]
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "========protectedat========", replacement = "@")]

  doc_summar$sep = "\n\n"

  outmrkdwn = doc_summar[, paste0(rbind(mrkdwn, sep), collapse = "")]


  # put orcid section
  author_orcinds <- sapply(X = metadata$authors, FUN = \(x){

    grepl(x = gsub(x = x$orcid, pattern = "[^0-9X]", replacement= ""), pattern = "[0-9X]{16}")
  })


  # if any orcids present, put all authors with oricds in a separate section before the references
  rmd_orcinds = ""
  if(!is.null(author_orcinds) & length(author_orcinds) > 0){

    td <- metadata$authors[author_orcinds] |> rbindlist(fill = TRUE)

    plural = ""
    if(NROW(td)> 1){
      plural = "s"
    }

    rmd_orcinds =                 "## ORCID" %+% plural %+%"\n\n"
    rmd_orcinds = rmd_orcinds %+% "```{=latex}\n{\\noindent\\raggedright\n"
    rmd_orcinds = rmd_orcinds %+% paste0( paste0(td$name , " \\orcidaffil{", td$orcid, "} \\href{https://orcid.org/",
                                                 td$orcid, "}{",
                                                 td$orcid,"}"), collapse = " \\\\\n") %+% "}\n\n"

    rmd_orcinds = rmd_orcinds %+% "\n```"
  }


  rmd_text = c(yaml_preamble, chunk_setup, fig_capts, tab_capts,
               "```{=latex}", # maybe sometimes a restart of the column environemnt might be helpful_
               "\\begin{multicols}{2}",
               #"\\raggedcolumns",
               #"\\interlinepenalty=10000",
               "```",
               outmrkdwn,
               "```{=latex}",
               "```",
               rmd_statements,
               rmd_orcinds,
               "```{=latex}",
               "\\end{multicols}",
               "```",
               "\\raggedbottom",
               rmd_multilang_abstracts,
               "```{=latex}",
               "\\begin{multicols}{2}",
               #"\\raggedcolumns",
               #"\\interlinepenalty=10000",
               "```",
               rmd_references,

               "```{=latex}",
               #"\\bibliography{$bibliography$}",
               "\\end{multicols}",
               "```")

  #[[figure,src: 'figures/Fig4.png', wide, caption: 'Promotores de salud capacitados.', label: 'fig4']]



  # write rmd file if filename provided
  if(!is.null(rmd_outfile)){
    write(rmd_text, file = rmd_outfile) # overwrites if existing
  }


  ############### JATS XML output ######################
  xml_metadata  = xml_reorder_metadata(metadata)

  xml_front = gen_xml_header(xml_metadata, base_folder = doc_folder, xml_filepack_dir = path_xml_filepack_dir)

  xml_refs = NULL
  if(exists("d_refs")){
    xml_refs = gen_xml_references(d_refs)
  }
  else{
    hgl_warn("No parsed references found, but necessary for JATS xml")
  }

  # write xml file if filename provided
  if(!is.null(xml_outpath)){

    # delete if already exists and create
    if(dir.exists(xml_outpath)) unlink(xml_outpath, recursive = T)
    dir.create(xml_outpath)

    # check and generatei statements such as Data availability ...
    if(length(xml_metadata$statements) > 0){
      xml_statements = gen_xml_statements(xml_metadata$statements)
    }

    xml_text = paste0(gen_xml_file(doc_summar, article_type = xml_metadata$article_type,
                                   xml_meta  = xml_front, xml_references = xml_refs,
                                   d_xmlintext_cites = d_xmlintext_cites, xml_statements = xml_statements))

    if(file.exists(paste0(path_xml_filepack_dir, "/document.xml"))) hgl_error(paste0("Error in writing out JATS xml:  document.xml already exists in '", path_xml_filepack_dir,"'"))
    else write(xml_text, file = paste0(path_xml_filepack_dir, "/document.xml")) # overwrites if existing

    # copy all to output dir - trick - base R does not allow copying of *contents* of directories - copy as single directory to parent folder, and then rename
    file.copy(from = list.files(path = paste0(path_xml_filepack_dir), full.names = T), to = paste0(xml_outpath), recursive = T)

    # attempt to create zip of contents
    zip::zip(zipfile = paste0(xml_outpath, "/document.zip"), files = list.files(xml_outpath, full.names= TRUE), mode = "cherry-pick")
  }

  return(rmd_text)
}



