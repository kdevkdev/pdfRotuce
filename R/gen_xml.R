getLE = function(l, names){

  # hacker funktion return and test element of nested lists
  # NULL if not foundS
  ns = names(unlist(l))

  for(cn in names){
    tofind = gsub(pattern = "\\$", replacement = ".", x = cn)

    if(any(startsWith(ns,tofind))){
      # evaluate the access expression in the local environment of the function
            return(eval(parse(text=paste0("l$",cn))))
    }
  }
  return(FALSE)
}
# xml escape characters
xe = function(s){

  s1 = stringi::stri_replace_all_fixed(str = s,
                                  pattern = c('&'),
                                  replacement = c("&amp;"),
                                  vectorise_all = F)

# to on two steps do avoid replacing ampersandof already escaped chars with '&amp'
  stringi::stri_replace_all_fixed(str = s1,
                                  pattern = c('"', "'", '<', '>'),
                                  replacement = c("&quot;", "&apos;", "&lt;", "&gt;"),
                                  vectorise_all = F)
}
pack_xml_file = function(src, base_folder, xml_filepack_dir){

  # try to copy file into temporary xml file pack directory and configure the right paths
  cpydest = paste0(xml_filepack_dir, "/", basename(src))
  cpysrc = paste0(base_folder, "/",src)

  if(file.exists(cpydest) && rlang::hash_file(cpydest) != rlang::hash_file(cpysrc))  hgl_error(paste0("pack_xml_file: trying to move xml figure file into xml packaging diretory, but a DIFFERENT file '",cpydest, "' already exists. Files need to be identical, otherwise files for JATS xml with non-unique names will overwrite each other."))
  if(!file.exists(cpysrc))  hgl_error(paste0("pack_xml_file: trying to move xml figure file into xml packaging directoy, but could not find '",cpysrc, "' at location"))

  file.copy(from = cpysrc, to = cpydest, overwrite = T)

}
# is valid
v = function(x){

  # if multiple elements in x, only check first one
  # length also triggers for x=NULL
  if(length(x) == 0 || is.na(x[1]) || isFALSE(x[1]) || is.nan(x[1])) return(FALSE)
  TRUE
}

xml_reorder_metadata = function(metadata){

  xml_meta = metadata
  fstatement <- metadata |> getLE(c("statements$funding","statements$Funding"))
  if(v(fstatement)){

    # move fudning statement because in JATS XML it belongs into the metadata header
    xml_meta$funding = list(statement = fstatement)
    metadata$statements$funding = NULL
  }

  if(exists("attributes", where = metadata)){

    # put all keywords into its own list and delete from attributes
    kwinds = names(metadata$attributes)[stringr::str_detect(string = names(metadata$attributes), pattern = "^keywords" )]
    if(length(kwinds) > 0 ){

      xml_meta$keywords = list()

      for(ckl in kwinds){

        if(ckl == "keywords"){
            xml_meta$keywords$keywords_mainlang = xml_meta$attributes[[ckl]]
        }
        else{

          lang = stringr::str_extract(string = ckl, pattern = "^keywords_(.+)$", group = 1)

          if(lang %in% c("es")){
            xml_meta$keywords[[ckl]] = xml_meta$attributes[[ckl]]
          }

        }
        xml_meta$attributes[[ckl]] = NULL
      }
    }

  }
  abspic <- metadata |> getLE(c("attributes$abstract_picture"))
  if(v(abspic)){

     xml_meta$abstract_picture = abspic
     xml_meta$attributes$abstract_picture = NULL
  }


  # all keyword langauges into 'kewywords'

  xml_meta
}
process_xml_intext_citations = function(text, d_refs){

  # function to generate <xref bibtype='bibr' rid='xx'> xml in-text reference tag

  # first find all citation keys

  l_intextcites = list()

  xmlres = stringr::str_replace_all(str = text, pattern = "\\[(========protectedat========\\w+;?)+\\]", replacement = \(match) {

    # get rid of square brackets and split into single citation keys
    t = stringr::str_replace_all(string = match, pattern = "\\[|\\]", replacement = "")
    ranges = stringr::str_split(string = t, pattern = ";", simplify = T)

    lres = vector("character", length(ranges))
    for(cri in 1:length(ranges)){

      # delete '@' text from citation key
      ccitekey = stringr::str_replace(string = ranges[cri], pattern = "========protectedat========", replacement = "")

      # find ref_number
      bn = d_refs[CITEKEY == ccitekey, BIBLIOGRAPHY_NUMBER]

      if(length(bn) !=1) {
          hgl_error(paste0("No valid bibliography number for citation key: ", ccitekey))
      } else {
        # use the same id generation procedure here as later in the xml biography
        #lres[cri] = paste0("<xref ref-type='bibr' rid='B", bn, "'>", bn, "</xref>")
        len = length(l_intextcites)+1
        l_intextcites[[len]] <<- data.table(xml = paste0("<xref ref-type='bibr' rid='B", bn, "'>", bn, "</xref>"), index = len)
        lres[cri] = paste0("========protectedintextcite", len, "========")
      }
    }
    # ptu back as string
    paste0("[", paste0(lres, collapse = ","), "]")
  })


  return(list(xml_text = xmlres, d_intextcites =  rbindlist(l = l_intextcites)))
}
insert_xml_intext_citations = function(xml_text, d_xmlintext_cites){

  # fill back in latex formulas
  xml_text = stringr::str_replace_all(xml_text, pattern = "========protectedintextcite[:digit:]+========", replacement = \(x) {

    # get index of inline math to get the corresponding row - unfortunate we have to run the regex again but neither stirngr nor stringi seem to provdie group match access in replacement functions
    ind = as.numeric(stringr::str_extract(x, "========protectedintextcite([:digit:]+)========", group = 1))

    xml = d_xmlintext_cites[index == ind, xml]
    xml
  })
  xml_text
}
gen_xml_date = function(datetime, tag = "date", attributes = NULL){

  isodate = lubridate::format_ISO8601(datetime, precision = "ymd", usetz = F)

  attstr = ""
  if(is.vector(attributes) && length(attributes) > 0){

    attstr  = paste0(names(attributes), "='", attributes, "'", collapse = " ")
  }

  # datetime should be a (lubridate) datetime object
  paste("<",tag," iso-8601-date='", isodate, "' ", attstr, ">\n",
        "<day>",lubridate::mday(datetime) |> xe(),"</day>\n",
        "<month>",lubridate::month(datetime)|> xe(),"</month>\n",
        "<year>",lubridate::year(datetime)|> xe(),"</year>\n",
        "</",tag,">", sep = "")

}
gen_xml_abstracts = function(metadata_abs, base_folder = NULL, xml_filepack_dir = NULL, abstract_picture = NULL){

  # TODO: dont forget handing of picture later
  res = list()
  for(cn in names(metadata_abs)){

    ca = metadata_abs[[cn]]
    tag = ""
    lan = ""
    picture = ""
    if(cn == "mainlang"){
      # WARNING JATS implicitly assumes this is english. should be properly explicitly or is it ok?
      tag = "abstract"

      # also check if picture provided
      if(!is.null(abstract_picture)){

        # only possible in 1.4 -> wait until implemented
        #pack_xml_file(src = abstract_picture, base_folder =  base_folder,  xml_filepack_dir = xml_filepack_dir)
        #picture = paste0("<graphic xlink:href='", abstract_picture,"' position='anchor'/>")
      }

    } else {
      tag = "trans-abstract"
      lan = paste0("xml:lang='", cn, "'")
    }

    resparts = list()
    for(ccpi in 1:length(ca[['parts']])){

      cctext = ca[['parts']][[ccpi]][['text']]
      cctitle = ca[['parts']][[ccpi]][['title']]

      tit = ""
      text = ""
      if(is.character(cctitle)){
        tit = paste0("<title>",cctitle|> xe(),"</title>\n")
      }
      if(is.character(cctext)){
        text = paste0("<p>", cctext|> xe(), "</p>")
      } else {
        stop("Invalid abstrat part, no text in gen_xml_abstracts")
      }
      resparts[[length(resparts)+1]] = paste0(tit, text)
    }

    if(length(resparts) > 1){

      text = paste("<sec>", resparts,  "</sec>", collapse = "\n", sep = "\n")

    } else if(length(resparts)  == 1){

      text = resparts[[1]]
    }

    res[[cn]] = paste0("<", tag, " ", lan, ">",text, "\n", picture,
           "\n</", tag, ">")
  }

  ret = paste0(res, collapse = "\n")
  ret
}
gen_xml_keywords = function(metadata_kwds){

  res = list()

  for(cn in names(metadata_kwds)){
    xml_lang = ""
    if(cn == "keywords_mainlang"){
      xml_lang = ""
    } else{
        lang = stringr::str_extract(string = cn, pattern = "_(.+)$", group = 1)
        xml_lang = paste0("xml:lang='es'")
    }
    kwds = trimws(stringr::str_split(string = metadata_kwds[[cn]], pattern = ",")[[1]])

    xkwds = paste0("<kwd>", kwds |> xe(), "</kwd>")

    res[[cn]] = paste0("<kwd-group ", xml_lang, " kwd-group-type='author'>\n",
                       paste(xkwds, collapse = "\n"),
                       "\n</kwd-group>")
  }
  ret = paste0(res, collapse = "\n")
  ret
}
gen_xml_header = function(metadata, base_folder,  xml_filepack_dir){


  # https://credit.niso.org/ credit taxonomoy terms to recogmnize author roles
  credit_roles = c("Conceptualization",
    "Data curation",
    "Formal analysis",
    "Funding acquisition",
    "Investigation",
    "Methodology",
    "Project administration",
    "Resources",
    "Software",
    "Supervision",
    "Validation",
    "Visualization",
    "Writing – original draft",
    "Writing – review & editing")

  # match by index
  credit_urls = c("http://credit.niso.org/contributor-roles/conceptualization/",
                  "http://credit.niso.org/contributor-roles/data-curation/",
                  "http://credit.niso.org/contributor-roles/formal-analysis/",
                  "http://credit.niso.org/contributor-roles/fundin-acquisition/",
                  "http://credit.niso.org/contributor-roles/investigation/",
                  "http://credit.niso.org/contributor-roles/methodology/",
                  "http://credit.niso.org/contributor-roles/project-administration/",
                  "http://credit.niso.org/contributor-roles/resources/",
                  "http://credit.niso.org/contributor-roles/software/",
                  "http://credit.niso.org/contributor-roles/supervision/",
                  "http://credit.niso.org/contributor-roles/validation/",
                  "http://credit.niso.org/contributor-roles/visualization/",
                  "http://credit.niso.org/contributor-roles/writing-original-draft/",
                  "http://credit.niso.org/contributor-roles/writing-review-editing/")


  processing_meta = paste0("<processing-meta ",
                  "tagset-family='jats' ",
                  "base-tagset='publishing' ",
                  "mathml-version='2.0' ",
                  "table-model='xhtml'/>")

  # front
  journal_meta = paste0("<journal-meta>",
                        "<journal-id journal-id-type='publisher'>",metadata$journal_publisher_id|> xe(),"</journal-id>",
                        "<journal-title-group>",
                        "<journal-title>",metadata$journal_title|> xe(), "</journal-title>",
                        "<abbrev-journal-title>",metadata$journal_title_short|> xe(),"</abbrev-journal-title>",
                        "</journal-title-group>",
                        "<issn publication-format='electronic'>",metadata$issn|> xe(),"</issn>",
                        "<publisher>", #PMC  ids for publisher and pubmed shorthand need likely to be added backupon indexation
                        "<publisher-name>",metadata$publisher|> xe(),"</publisher-name>",
                        "</publisher>",
                        "</journal-meta>")


  author_vec = vector(mode = "character", length = length(metadata$authors))
  i = 1
  for(ca in metadata$authors){
      # check here if two name parts or if only one  name

       names = ""
       if(!is.null(ca$family_name) && !is.null(ca$first_name)){
         names = paste("<surname>",ca$family_name|> xe(),"</surname>\n",
                       "<given-names>",ca$first_name|> xe(),"</given-names>",sep = "")
       }
       else{
         names = paste("<surname>",ca$name|> xe(),"</surname>", sep = "") # online validator fails if string-name element is used, but should be valid according to tag library
       }

       # we need to handle multiple affiliations per author, hence the loop
       affil_ids = stringr::str_split_1(string = ca$affiliation_ids, pattern = ",")
       txr = vector("character", length(affil_ids))[[1]]
       for(cafri  in 1:length(affil_ids)){

          txr[cafri] = paste0( "<xref ref-type='aff' rid='aff_", trimws(affil_ids[[cafri]])  ,"'/>")
       }

       # detect if corresponding
       corresp_xref = corresp_att = ""
       if(ca$corresponding){
          corresp_xref = "<xref ref-type='corresp' rid='corr_1'/>" # hardcode to 1 for now
          corresp_att = "corresp='yes'"
       }

       # roles
       v_roles = vector("character", length(ca$contrib_roles))
       if(!is.null(ca$contrib_roles)){

         for(cri in 1:length(ca$contrib_roles)){

            crole = ca$contrib_roles[cri]
            v_roles[cri] = ""
             # is it a credit taxonomy temr_
            mi = match(tolower(trimws(crole)), tolower(credit_roles))
            if(!is.na(mi)){

              v_roles[cri] = paste0("<role vocab='credit' vocab-identifier='https://credit.niso.org/' ",
                             "vocab-term-identifier='", credit_urls[mi], "' ",
                             "vocab-term='", tolower(crole), "'>", crole|> xe(), "</role>")
            }
            else {

              v_roles[cri] = paste0("<role>",crole|> xe(),"</role>\n")
            }

          }
       }

       affilxrefs = paste(txr, collapse  ="\n")
       author_vec[i] =
         paste(paste0("<contrib contrib-type='author' ", corresp_att, " >"),
                  "<name>",names,
                  "</name>",
                  affilxrefs,
                  corresp_xref,
                  paste(v_roles, collapse = "\n"),
                "</contrib>", sep = "\n")
       i = i+1

  }
  authors = paste0(author_vec, collapse = "\n")


  affils_vec = vector("character", length(metadata$affiliations))
  for(cafi in 1:length(metadata$affiliations)){

    affils_vec[cafi] = paste0("<aff id='aff_", metadata$affiliations[[cafi]]$id,  "'>",metadata$affiliations[[cafi]]$address |> xe(),"</aff>")
  }
  affiliations = paste0(affils_vec, collapse = "\n")


  corresp_note = ""
  if(is.character(metadata$attributes$corresponding_email)){

    email = metadata$attributes$corresponding_email
    if(stringr::str_detect(string = email, pattern = ".+@.+\\..+")){ # primitive check if email to put email tag
      email = paste0("<email>", email|> xe() ,"</email>")
    }
    corresp_note = paste0("<corresp id='corr_1'>*", metadata$string_corresponding |> xe(), ": ", email, "</corresp>")
  }




  # todo: abstract and keyworsd
  adates = stringr::str_split_1(string = metadata$articledates, pattern = ";")

  # parse dataes - two  orders allowed: 30 september 2024, 2024 september 30 , september 30 2024
  parsed_adates = lubridate::parse_date_time(x = adates, orders = c("dmy", "ymd", "mdy"))

  parsed_adatetypes = stringr::str_match(string = stringr::str_to_lower(adates), pattern = "received|published|accepted")[,1]

  # mandatory published missing?
  pubdate_i = which(parsed_adatetypes == "published")
  if(length(pubdate_i) != 1){
    stop("multiple or no 'published' dates in metadata.csv provided")
  }

  pubpdate = ""
  hist_list = list()

  for(j in 1:length(adates)){

    if((is.na(parsed_adates[j]) && !(j == pubdate_i && stringr::str_detect(string = stringr::str_to_lower(adates[j]), pattern = "unpublished")))){
      stop(paste0("invalid date:", adates[j]))
    }
    if(is.na(parsed_adatetypes[j])){
      stop(paste0("invalid date type (needs accepted, published or received):", adates[j]))
    }

    if(tolower(parsed_adatetypes[j]) == "published"){

      ctate = parsed_adates[j]
      if(is.na(ctate)){
        ctate = lubridate::parse_date_time("1700",  orders = "y")
      }


      # publication-date needs date-type 'pub' https://jats.nlm.nih.gov/publishing/tag-library/1.3/attribute/date-type.html
      pubdate = gen_xml_date(ctate, tag = "pub-date", attributes = c('date-type' = "pub", 'publication-format'='electronic'))
    }
    else {
      # put in history
      hist_list[[length(hist_list)+1]] = gen_xml_date(parsed_adates[j], attributes = c('date-type' = parsed_adatetypes[j])) # can put directly, both accepted and received part of JATS standard
    }

  }
  history = paste("<history>", paste(hist_list, collapse = "\n"), "</history>", sep = "\n")
  volumeissue = paste("<volume>", metadata$volume|> xe(),"</volume>\n",
                      "<issue>", metadata$issue|> xe(), "</issue>",
                      "<elocation-id>1</elocation-id>\n",
                      sep = "")



  permissions = paste("<permissions>\n",
                      "<copyright-statement>©",metadata$copyright_year, metadata$copyright|> xe(),"</copyright-statement>\n",
                      "<copyright-year>",metadata$copyright_year,"</copyright-year>\n",
                      "<copyright-holder>", metadata$copyright_holder|> xe(),"</copyright-holder>\n",
                      "<license license-type='open-access'>",
                      "<license-p>", metadata$license |> xe(),"</license-p>",
                      "</license>",
                      "</permissions>", sep = "")

  abstracts = gen_xml_abstracts(metadata$abstracts,abstract_picture =  metadata$abstract_picture, base_folder = base_folder, xml_filepack_dir = xml_filepack_dir)

  keywords = ""
  if(exists("keywords", where = metadata)){
    keywords =  gen_xml_keywords(metadata$keywords)
  }

  funding = ""
  fstatement <- metadata |> getLE("funding$statement")
  if(v(fstatement)){

    paste0("<funding-group>",
           "<funding-statement>", fstatement|> xe(), "</funding-statement>",
      "</funding-group>")
  }

  article_meta = paste("<article-meta>",
                          #"<article-id pub-id-type='publisher-id'>181325198</article-id>", # needed - Ask miguel?$
                          paste0("<article-id pub-id-type='doi'>",metadata$doi|> xe(),"</article-id>"),
                          "<article-version vocab='JAV' vocab-identifier='http://www.niso.org/publications/rp/RP-8-2008.pdf' vocab-term='Version of Record' article-version-type='VoR'>Version of Record</article-version>", # ask miguel
                          "<article-categories>",
                          "<subj-group>",
                          "<subject>Articles</subject>", # use two level groupinsg to be somewhat future proof
                          "<subj-group>",
                          "<subject>",metadata$article_type|> xe(),"</subject>",
                          "</subj-group>",
                          "</subj-group>",
                          "</article-categories>",

                          # article title
                          paste0("<title-group>\n<article-title>", metadata$title|> xe(), "</article-title>\n</title-group>"),

                          # authors - ask miguel if CREDIT statment will be used? if yes, check pnas original again
                          "<contrib-group content-type='authors'>",
                          authors,
                          affiliations,
                          "</contrib-group>",
                          "<author-notes>",
                            corresp_note,
                          "</author-notes>",
                          pubdate,
                          volumeissue,
                          history,
                          permissions,
                          abstracts,
                          keywords,
                          funding,
                          "</article-meta>", sep = "\n")


  header = paste(processing_meta, "<front>", journal_meta, article_meta, "</front>", sep = "\n")
  header
}
gen_xml_authors = function(v_authors){



  xmlauthors = ""

  if(!is.null(v_authors)){
    v_authors = v_authors[!is.na(v_authors)] #filter to tonlyi get NA


    if(length(v_authors) > 0){

      authoritems = vector("character", length(v_authors))
      for(j in 1:length(v_authors)){


        # parse family and first name
        anparts = strsplit(v_authors[j], split = ",")[[1]]

        # check if we have first and surname
        if(length(anparts) == 1){

          authoritems[j] = paste0("<name>\n<surname>", # also remove curly brackets
                                  trimws(anparts[1], whitespace = "[ \t\r\n}{]")|> xe(),
                                  "</surname>\n</name>")


        } else if (length(anparts) == 2){

          authoritems[j] = paste0("<name>\n<surname>",
                                  trimws(anparts[1]|> xe(), whitespace = "[ \t\r\n}{]"),"</surname>\n",
                                  "<given-names>",trimws(anparts[2], whitespace = "[ \t\r\n}{]")|> xe(),
                                  "</given-names>\n</name>")

        }
        else {
          hgl_error("invalid number of author name parts ")
        }

      }
      xmlauthors = paste0(authoritems, collapse = "\n")

    } else{
      hgl_warn("gen_xml_authors: No valid entries in vector")
    }
  }
  xmlauthors
}
gen_xml_references = function(d_refs){


  # JATS: element-citation -> only well structured, biographic data contained
  # JATS: mixecd-element -> can also contain text

  # generate JATS xml references here
  refitems = vector("character", NROW(d_refs))

  # empty rows might occur)
  dr = d_refs[!is.na(CITEKEY)]


  for(i in 1:NROW(dr)){

    cid = paste0("B", i)
    clab = paste0(i,".")
    # if a list, use [[]]

    # parse authors
    xml_authors = gen_xml_authors(dr$AUTHOR[[i]])
    pgauthors = ""
    if(nchar(xml_authors) > 0 ){
      pgauthors = paste("<person-group person-group-type='author'>", xml_authors, "</person-group>", sep = "\n")
    }

    # and editors
    xml_editors = gen_xml_authors(dr$EDITOR[[i]])
    pgeditors = ""

    if(nchar(xml_editors) > 0 ){
      pgeditors = paste("<person-group person-group-type='editor'>", xml_editors, "</person-group>", sep= "\n")
    }




    volume          = putxml.nullna(dr$VOLUME[i],                                                           "volume")
    issue           = putxml.nullna(dr$NUMBER[i],                                                           "issue")
    articletitle    = putxml.nullna(trimws(dr$TITLE[i], whitespace = "[ \t\r\n}{]"),                        "article-title")
    source          = putxml.nullna(trimws(dr$BOOKTITLE[i], whitespace = "[ \t\r\n}{]"),                    "source")
    source          = if(source == "") putxml.nullna(trimws(dr$JOURNAL[i], whitespace = "[ \t\r\n}{]"),     "source") # give higher priority to journal title
    edition         = putxml.nullna(dr$EDITION[i],                                                          "edition")


    publisherloc    = putxml.nullna(dr$ADDRESS[i],                                          "publisher-loc")
    publishername   = putxml.nullna(dr$PUBLISHER[i],                                        "publisher-name")
    extlink         = putxml.nullna(dr$URL[i],                                              "ext-link", c("ext-link-type"="url"))
    pubiddoi        = putxml.nullna(dr$DOI[i],                                              "pub-id", c("pub-id-type"="doi"))


    pages = ""
    if(!is.na(dr$PAGES[i])){

      tspl = stringr::str_split(string = dr$PAGES[i], pattern = "((--)|-)")[[1]]
      if(length(tspl)> 1){
        pages = paste0("<fpage>",tspl[1]|> xe(),"</fpage>\n<lpage>",tspl[2]|> xe(),"</lpage>")
      } else{
        pages = paste0("<fpage>",tspl[1]|> xe(),"</fpage>")
      }
    }

    # publicatoin-type
    pt = switch(dr$CATEGORY[i], ARTICLE="journal", BOOK="book", TECHREPORT="report", CONFERENCE="conf-paper", MISC="webpage", "")
    if(nchar(pt) > 0 ) publicationtype = paste0("publication-type='", pt,"'") else publicationtype = ""

    # , publication-format='print' ?
    publicationformat = ""
    if(nchar(pubiddoi) > 0 || nchar(extlink) > 0 ) publicationformat = paste0("publication-format='electronic'")
    else publicationformat = paste0("publication-format='print'")


    # only extract years -> the recogniztion by anystyle seems to be too unreliable for more
    tdate = stringr::str_extract(dr$YEAR[i], pattern = "[0-9]{4}")
    year = ""
    if(!is.na(tdate)  && length(tdate) == 1 && nchar(tdate) == 4){
      year = paste0("<year iso-8601-date='", tdate, "'>",tdate, "</year>")
    }


    refitems[i]   = paste(
      paste0("<ref id='", cid , "' >"),
      paste0("<label>", clab|> xe(), "</label>"),
      paste0("<element-citation ",  publicationtype, " ", publicationformat, " >"),
      pgauthors,
      pgeditors,
      year,
      articletitle,
      source,
      volume,
      issue,
      edition,
      pages,
      publisherloc,
      publishername,
      pubiddoi,
      extlink,
      "</element-citation>",
      "</ref>",sep = "\n")


    # remove blank lines
    refitems[i] <- stringr::str_replace_all(string = refitems[i], pattern = stringr::regex("[\n\r]{2,}", multiline = T), replacement = "\n")
  }




  xml_refs = paste("<ref-list>",
                    paste0(refitems, collapse="\n"),
                    "</ref-list>", sep = "\n")
  xml_refs

}
gen_xml_displaymath = function(latex, counter){


  mid = paste0("id='DM_", counter, "'")
  paste0("<disp-formula ",mid, ">",
         "<tex-math><![CDATA[\\documentclass[12pt]{minimal} \\usepackage{wasysym} \\usepackage[substack]{amsmath} \\usepackage{amsfonts} \\usepackage{amssymb} \\usepackage{amsbsy} \\usepackage[mathscr]{eucal} \\usepackage{mathrsfs} \\DeclareFontFamily{T1}{linotext}{} \\DeclareFontShape{T1}{linotext}{m}{n} { &#x003C;-&#x003E; linotext }{} \\DeclareSymbolFont{linotext}{T1}{linotext}{m}{n} \\DeclareSymbolFontAlphabet{\\mathLINOTEXT}{linotext} \\begin{document} $$",
         latex,
         "$$ \\end{document} ]]></tex-math>",
         "</disp-formula>", sep = "\n")



}
gen_xml_inlinemath = function(latex, counter = NULL){


  paste0("<inline-formula>",
         "<tex-math><![CDATA[\\documentclass[12pt]{minimal} \\usepackage{wasysym} \\usepackage[substack]{amsmath} \\usepackage{amsfonts} \\usepackage{amssymb} \\usepackage{amsbsy} \\usepackage[mathscr]{eucal} \\usepackage{mathrsfs} \\DeclareFontFamily{T1}{linotext}{} \\DeclareFontShape{T1}{linotext}{m}{n} { &#x003C;-&#x003E; linotext }{} \\DeclareSymbolFont{linotext}{T1}{linotext}{m}{n} \\DeclareSymbolFontAlphabet{\\mathLINOTEXT}{linotext} \\begin{document} $$",
         latex,
         "$$ \\end{document} ]]></tex-math>",
         "</inline-formula>", sep = "\n")
}
gen_xml_paragraphs = function(ptext,d_inlinemath){

  res = vector("character", length = length(ptext))

  # parse rmarkdown to xml- but keep in vector order, hence the loop, and check for invalid entries to be set to ''
  for(i in 1:length(ptext)){

    if(is.null(ptext[i]) || is.na(ptext[i]) || !is.character(ptext[i])  || nchar(ptext[i]) ==0){
        res[i] = ""
    }else{

        # xml parsetree as text
        xml = commonmark::markdown_xml(ptext[i])

        # to R xml
        xml_nodes = xml2::read_xml(xml)

        if(xml2::xml_length(xml_nodes) >0 ){

          xml2::xml_ns_strip(xml_nodes)

          # to develop https://xsltfiddle-beta.liberty-development.net/
          xlst =  xml2::read_xml(system.file("commonmark_xml_to_jats_xml.xml", package="pdfRotuce"))

          # define xslt transformation
          xml_jats = xslt::xml_xslt(xml_nodes,xlst)

          if(is.na(xml_jats)){
            stop("commonmark to JATS xslt transformation failed")
          }


          docnode = xml2::xml_find_first(xml_jats, "/*")

          if(is.na(xml_jats)){
            stop("no valid root node in transfomred JATS xml")
          }

          xml_jatsstr = as.character(docnode)

          # fill back in latex formulas
          xml_jatsstr = stringr::str_replace_all(xml_jatsstr, pattern = "========protectedinlinemath[:digit:]+========", replacement = \(x) {

              # get index of inline math to get the corresponding row - unfortunate we have to run the regex again but neither stirngr nor stringi seem to provdie group match access in replacement functions
              ind = as.numeric(stringr::str_extract(x, "========protectedinlinemath([:digit:]+)========", group = 1))

              latex = d_inlinemath[index == ind, latex]
              gen_xml_inlinemath(latex)
          })

          res[i] =xml_jatsstr
        } else{

          res[i] = "" # empty docment

        }

    }
  }


  return(res)
}

gen_xml_sections = function(doc_summar){

  # for JATS we need more elaborate parsing because we ned to know the level to set the number of taghs correctly
  headi = matrix(data = 0, nrow = NROW(doc_summar), ncol = 5)

  # in the matrix set the responding column for the respective row to 1
  headi[,1] = doc_summar[,tolower(style_name) == "heading 1"]
  headi[,2] = doc_summar[,tolower(style_name) == "heading 2"]
  headi[,3] = doc_summar[,tolower(style_name) == "heading 3"]
  headi[,4] = doc_summar[,tolower(style_name) == "heading 4"]
  headi[,5] = doc_summar[,tolower(style_name) == "heading 5"]

  # get row indices with 1 entries
  hinds = which(headi |> rowSums() > 0 )

  # 1. fill everything right of a 1-cell with 2 to mark an interruption through an upper level
  for(ci in hinds) headi[ci, ] = cumsum(headi[ci, ])+shift(x = cumsum(headi[ci, ]), n = 1, fill = 0)


  # 2. insert xml section start and endtags
  hxml_pretags = rep("", times = NROW(headi))
  hxml_postags = rep("", times = NROW(headi))
  for(crow in hinds) {

    # current heading start
    ccol = which(headi[crow,] == 1)


    # put section type for some level 1 headings (https://jats.nlm.nih.gov/archiving/tag-library/1.4/attribute/sec-type.html)
    sectype = ""
    txt = doc_summar$text[crow]
    if(ccol  ==1 ){


      t = switch(tolower(trimws(txt)),
              conclusions = "conclusions",
              discussion = "discussion",
              introduction = "intro",
              background = "intro",
              methods = "methods",
              results = "results",
             "")

      if(nchar(t) > 0){
        sectype = paste0("sec-type='", t,"'")
      }
    }


    hxml_postags[crow] = paste0(hxml_postags[crow],"<sec ", sectype, "><title>",txt|> xe(), "</title>")

    # find section end -> next row with 1 or 2 in the same column
    lastrow = integer(0)
    if(crow < NROW(headi)) {
      sstart = crow+1

      lastrow = which(headi[sstart:NROW(headi), ccol] > 0)

      # was there no 1 or 2 below the current row in this column? then sedtion ends at the last row
      if(length(lastrow) > 0) hxml_pretags[lastrow[1]+crow] = paste0("</sec>", hxml_pretags[lastrow[1]+crow])


    }

    # if the content is at the end, then the closing that needs to go behind
    if(length(lastrow) == 0){
      # at the target row at the end
      hxml_postags[NROW(headi)] = paste0("</sec>", hxml_postags[NROW(headi)])
    }

    #cat("cr: ", crow, "lr: ", lastrow, "\n")

  }

  return(list(hxml_pretags = hxml_pretags, hxml_postags = hxml_postags))
}
gen_xml_table = function(ct_csv, tab_opts, tab_counter){

  # label. to autogenerate
  # caption title  - is it needed?
  ct_csv_o = copy(ct_csv)

  label = tab_opts['label']
  if(is.null(tab_opts['label']) || is.na(label) || !is.character(label) ){

    label = paste0("tableautolabel", tab_counter)
  }

  # caption provided?
  tab_caption = ""
  if('caption' %in% names(tab_opts)){
    tab_caption = tab_opts['caption']

    # escape %, @, $
    tab_caption = escape_caption(tab_caption)
  }

  header_inds = which(startsWith(ct_csv[[1]] |> trimws(), "#"))


  if(length(header_inds) == 0){
    # do o not work, behaviour is ok: hgl_warn(paste0("Table #",  tab_counter ,  " - ", label ,": no header row provided (=first cell in column starting with '#'), assuming first row is header row. XML Jats needs at least 1 header row."))
    header_inds = 1
  }



  # remove '#'
  ct_csv[[1]] = gsub(pattern = "#", replacement = " ",  x =ct_csv[[1]])

  # parse cells
  ct_csv = apply(ct_csv, MARGIN = c(1,2), FUN = \(x) {

    # cell md
    x = parse_cellmd_xml(x)

    # 3 empty spaces
    x = gsub(x = x,pattern = "[ ]{3}", replacement = "<preformat>   </preformat>") # 3 empty spaces
  })

  colgroup = ""
  colwidths = NULL
  colaligns = NULL


  #todo: fix missing conversion to reasonable values (perentage and left,right,center)
  if(!is.null(tab_opts['colwidths'])){

    tcw = as.numeric(tab_opts['colwidths'][[1]])
    if(!all(is.numeric(tcw))) stop("nonnumeric table colwidth provided")
    tcw = tcw / sum(tcw)
    colwidths = paste0("width='", tcw, "%'")
  }




  id = paste0( label, "_", tab_counter)

  label = paste0("Table ", tab_counter)

  if(!is.null(tab_opts['colaligns']) && !is.na(tab_opts['colaligns']))  {

    tca = tab_opts['colaligns'][[1]]

    if(!all(tca %in% c("l", "r", "c"))) stop("only 'c','l','r' allowed in the list of table column align specifiers")

    tcav = ifelse(tca == "r", "'right'", ifelse(tca == "c", "'center'", "'left'"))

    colaligns = paste0("align=", tcav)
  }

  if(!is.null(colwidths) || !is.null(colaligns)){
    colgroup = paste0("<col ", colwidths, " ", colaligns,"/>", collapse = "\n")
  }

  rf_str = "rules='none' frame='hsides'"

  if('fullgrid' %in% names(tab_opts)){
    rf_str = "rules='all' frame='box'"
  }

  # need to implement: header rows (style attribute with gray background,
  # colwidths, colaligns, fullgrid), table id

  celltags = rep("td", times = NROW(ct_csv))
  celltags[header_inds] = "th"

  row_strings = vector(mode = "character", length = NROW(ct_csv))
  for(i in 1:NROW(ct_csv)){
    row_strings[i] = paste0("<tr>", paste0("<",celltags[i], " " , colaligns, ">", ct_csv[i,] |> unlist(), "</", celltags[i], ">", collapse = ""), "</tr>")
  }
  tbodystring = paste0(row_strings, collapse = "\n")

  table_xml = paste0("<table-wrap id='", id ,"'>",
   paste("<label>", label|> xe(), "</label>"),
   "<caption>",
    #"<title>Patient Care at End of Follow Up</title>", caption title if ever
     paste0("<p>", tab_caption|> xe(), "</p>"),
   "</caption>",
   "<table ",rf_str," cellpadding='3'>",
    "<colgroup>", colgroup, "</colgroup>",
    "<tbody>",
        tbodystring,
    "</tbody>
   </table>",
   # <table-wrap-foot>
   #  <fn-group>
   #   <fn id='TF1-150'><p>Data not available for 1 trial.</p></fn>
   #   <fn id='TF1-151'><p>P&#x003C;0.05 (random effects model).</p></fn>
   #  </fn-group>
   # </table-wrap-foot>
  "</table-wrap>", sep = "\n")

  table_xml
}
gen_xml_figure = function(fig_opts, fig_counter, xml_filepack_dir, base_folder){

  id = paste0( fig_opts['label'], "_", fig_counter)
  label = paste0("Figure ", fig_counter)

  # caption provided?
  tab_caption = ""
  if('caption' %in% names(fig_opts)){
    fig_caption = fig_opts['caption']

    # escape %, @, $
    fig_caption = escape_caption(fig_caption)
  }


  if('src' %in% names(fig_opts)){


    fig_src = fig_opts[['src']] # add one level up because its not in the build dir but one level above
    pack_xml_file(src = fig_src, base_folder = base_folder, xml_filepack_dir = xml_filepack_dir)

  }
  else{
    stop("Figure src not provided")
  }
  figure_xml = paste(paste0("<fig id='", id, "'>"),
                    paste0("<label>", label|> xe(), "</label>"),
                    "<caption>",
                    paste0("<p>", fig_caption|> xe(),"</p>"),
                    "</caption>",
                    paste0("<graphic xmlns:xlink='http://www.w3.org/1999/xlink' xlink:href='", fig_src, "'>"),
                    "</graphic>
                    </fig>", sep = "\n")

  figure_xml
}
gen_xml_statements = function(statement_list){

  # generate statements to be put in bACK

  if(length(statement_list) > 0){

    ret = vector("character", length(statement_list))
    # list with statement titles as names, statement texts as text
    for(ci in 1:length(statement_list)){

      title = names(statement_list)[ci]
      if(!is.null(title)) title = paste0("<title>", title|> xe(), "</title>")

      # type detection
      sectype = ""
      tag = "sec"
      if(!is.null(title)){

        if(tolower(title) == "data availability"){
          sectype = "data-availability"
        } else if(tolower(title) == "acknowledgement" || tolower(title) == "acknowledgements") {
          tag = "ack"
        } else if(stringr::str_detect(string = tolower(title), pattern = "( conflict| competing).+interest")){
          sectype = "coi-statement"
        }else if(tolower(title) == "contributions"){
          sectype = "author-contributions" # according to : https://jats.taylorandfrancis.com/jats-guide/topics/author-contributions-credit/#author-contributions-statement
        }
        # do nothing specific otherwise

      }

      # Funding: https://jats.nlm.nih.gov/archiving/tag-library/1.4/element/funding-group.html -> either very structured or text funding statement in article metadata
      # COntributions: single statment and/or specific to listed authors and addtional parties in article metadata contrib-group / contrib/ role
# disclosure: according to https://jats.taylorandfrancis.com/jats-guide/topics/disclosure-statement/ , as a sec of type COI-statement inside of back


      ret[ci] = paste0("<",tag,">",
             title,
             paste0("<p>",statement_list[[ci]]|> xe(), "</p>"),
             "</", tag, ">", sep = "\n")

      return(paste0(ret, collapse = "\n"))
    }


  } else {
    return("")
  }
}

gen_xml_file = function(doc_summar, article_type, xml_meta, xml_references, d_xmlintext_cites, xml_statements){

  xml_article_type = ""

  if(tolower(article_type) == "original research"){

    xml_article_tuype = "research-article"

  } else if(tolower(article_type) == "review"){

    xml_article_type = "review-article"

  } else if(tolower(article_type) == "comment"){

    xml_article_type = "article-commentary"
  }
  else{
    hgl_warn("unkown article type " %+% article_type  %+% " for JATS conversion, might not confirm to suggested best practises in JATS standards")
    xml_article_type = tolower(article_type)
  }

  header = paste("<?xml version='1.0' encoding='UTF-8'?>",
                  "<!DOCTYPE article PUBLIC",
                 "'-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.3 20210610//EN'",
                 "'JATS-journalpublishing1-3.dtd'>",
                 # "'-//NLM//DTD JATS (Z39.96) Article Authoring DTD v1.4 20241031//EN'",
                 # "'JATS-articleauthoring1-4.dtd'>",
                  paste0("<article article-type='",xml_article_type, "'"),
  #"dtd-version='1.4'",
  "dtd-version='1.3'",
  "xml:lang='en'",
  "xmlns:mml='http://www.w3.org/1998/Math/MathML'",
  "xmlns:xlink='http://www.w3.org/1999/xlink'",
  "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' >",
                 xml_meta, sep = "\n")

  # get rid of NA entires - could be unsave though in case NAs do not match over rows
  secpre = doc_summar$xml_pretags
  text    = doc_summar$xml_text
  secpos  = doc_summar$xml_postags



  secpre[is.na(secpre)] = ""
  text[is.na(text)] = ""
  secpos[is.na(secpos)] = ""

  xmltext_rows = paste(trimws(secpre), trimws(text), trimws(secpos))



  mid = paste0(xmltext_rows[nchar(xmltext_rows) > 0], collapse = "\n")

  # put xml intext citations back
  mid = insert_xml_intext_citations(xml_text = mid, d_xmlintext_cites = d_xmlintext_cites)

  if(is.null(xml_references)) xml_references = ""

  back = paste0("<back>",xml_statements, xml_references,"</back>", sep="\n")

  end = "</article>"

  res = paste(header, "<body>", mid, "</body>", back, end, sep = "\n")

}
