gen_xml_date = function(datetime, tag = "date", attributes = NULL){

  isodate = lubridate::format_ISO8601(datetime, precision = "ymd", usetz = F)

  attstr = ""
  if(is.vector(attributes) && length(attributes) > 0){

    attstr  = paste0(names(attributes), "='", attributes, "'", collapse = " ")
  }

  # datetime should be a (lubridate) datetime object
  paste("<",tag," iso-8601-date='", isodate, "' ", attstr, ">\n",
        "<day>",lubridate::mday(datetime),"</day>\n",
        "<month>",lubridate::month(datetime),"</month>\n",
        "<year>",lubridate::year(datetime),"</year>\n",
        "</",tag,">", sep = "")

}
gen_xml_abstracts = function(metadata_abs){

  # TODO: dont forget handing of picture later
  res = list()
  for(cn in names(metadata_abs)){

    ca = metadata_abs[[cn]]
    tag = ""
    lan = ""
    if(cn == "mainlang"){
      # WARNING JATS implicitly assumes this is english. should be properly explicitly or is it ok?
      tag = "abstract"

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
        tit = paste0("<title>",cctitle,"</title>\n")
      }
      if(is.character(cctext)){
        text = paste0("<p>", cctext, "</p>")
      } else {
        stop("Invalid abstrat part, no text in gen_xml_abstracts")
      }
      resparts[[length(resparts)+1]] = paste0(tit, text)
    }

    if(length(resparts) > 1){

      text = paste("<sec>", resparts, "</sec>", collapse = "\n", sep = "\n")

    } else if(length(resparts)  == 1){

      text = resparts[[1]]
    }

    res[[cn]] = paste0("<", tag, " ", lan, ">",text,
           "\n</", tag, ">")
  }

  ret = paste0(res, collapse = "\n")
  ret
}
gen_xml_header = function(metadata){


  processing_meta = paste0("<processing-meta ",
                  "tagset-family='jats' ",
                  "base-tagset='publishing' ",
                  "mathml-version='2.0' ",
                  "table-model='xhtml'/>")

  # front
  journal_meta = paste0("<journal-meta>",
                        "<journal-id journal-id-type='publisher'>",metadata$journal_publisher_id,"</journal-id>",
                        "<journal-title-group>",
                        "<journal-title>",metadata$journal_title, "</journal-title>",
                        "<abbrev-journal-title>",metadata$journal_title_short,"</abbrev-journal-title>",
                        "</journal-title-group>",
                        "<issn publication-format='electronic'>",metadata$issn,"</issn>",
                        "<publisher>", #PMC  ids for publisher and pubmed shorthand need likely to be added backupon indexation
                        "<publisher-name>",metadata$publisher,"</publisher-name>",
                        "</publisher>",
                        "</journal-meta>")


  author_vec = vector(mode = "character", length = length(metadata$authors))
  i = 1
  for(ca in metadata$authors){
      # check here if two name parts or if only one  name

       names = ""
       if(!is.null(ca$family_name) && !is.null(ca$first_name)){
         names = paste("<surname>",ca$family_name,"</surname>\n",
                       "<given-names>",ca$first_name,"</given-names>",sep = "")
       }
       else{
         names = paste("<surname>",ca$name,"</surname>", sep = "") # online validator fails if string-name element is used, but should be valid according to tag library
       }

       # we need to handle multiple affiliations per author, hence the loop
       affil_ids = stringr::str_split_1(string = ca$affiliation_ids, pattern = ",")
       txr = vector("character", length(affil_ids))[[1]]
       for(cafri  in 1:length(affil_ids)){

          txr[cafri] = paste0( "<xref ref-type='aff' rid='aff_", affil_ids[[cafri]]  ,"'/>")
       }

       # detect if corresponding
       corresp_xref = corresp_att = ""
       if(ca$corresponding){
          corresp_xref = "<xref ref-type='corresp' rid='corr_1'/>" # hardcode to 1 for now
          corresp_att = "corresp='yes'"
       }

       affilxrefs = paste(txr, collapse  ="\n")
       author_vec[i] =
         paste(paste0("<contrib contrib-type='author' ", corresp_att, " >"),
                  "<name>",names,
                  "</name>",
                  affilxrefs,
                  corresp_xref,
                "</contrib>", sep = "\n")
       i = i+1

  }
  authors = paste0(author_vec, collapse = "\n")


  affils_vec = vector("character", length(metadata$affiliations))
  for(cafi in 1:length(metadata$affiliations)){

    affils_vec[cafi] = paste0("<aff id='aff_", metadata$affiliations[[cafi]]$id,  "'>",metadata$affiliations[[cafi]]$address,"</aff>")
  }
  affiliations = paste0(affils_vec, collapse = "\n")


  corresp_note = ""
  if(is.character(metadata$attributes$corresponding_email)){

    email = metadata$attributes$corresponding_email
    if(stringr::str_detect(string = email, pattern = ".+@.+\\..+")){ # primitive check if email to put email tag
      email = paste0("<email>", email ,"</email>")
    }
    corresp_note = paste0("<corresp id='corr_1'>*", metadata$string_corresponding, ": ", email, "</corresp>")
  }




  # todo: abstract and keyworsd
  adates = stringr::str_split_1(string = metadata$articledates, pattern = ";")

  # parse dataes - two  orders allowed: 30 september 2024, 2024 september 30 , september 30 2024
  parsed_adates = lubridate::parse_date_time(x = adates, orders = c("dmy", "ymd", "mdy"))

  parsed_adatetypes = stringr::str_match(string = stringr::str_to_lower(adates), pattern = "received|published|accepted")[,1]

  # mandatory published missing?
  pubdate_i = which(parsed_adatetypes == "published")
  if(length(pubdate_i) != 1){
    stop("multiple or no published dates in metadata.csv provided")
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

    if(parsed_adatetypes[j] == "published"){

      # publication-date needs date-type 'pub' https://jats.nlm.nih.gov/publishing/tag-library/1.3/attribute/date-type.html
      pubdate = gen_xml_date(parsed_adates[j], tag = "pub-date", attributes = c('date-type' = "pub", 'publication-format'='electronic'))
    }
    else {
      # put in history
      hist_list[[length(hist_list)+1]] = gen_xml_date(parsed_adates[j], attributes = c('date-type' = parsed_adatetypes[j])) # can put directly, both accepted and received part of JATS standard
    }

  }
  history = paste("<history>", paste(hist_list, collapse = "\n"), "</history>", sep = "\n")
  volumeissue = paste("<volume>", metadata$volume,"</volume>\n",
                      "<issue>", metadata$issue, "</issue>",
                      "<elocation-id>1</elocation-id>\n",
                      sep = "")



  permissions = paste("<permissions>\n",
                      "<copyright-statement>©",metadata$copyright_year, metadata$copyright,"</copyright-statement>\n",
                      "<copyright-year>",metadata$copyright_year,"</copyright-year>\n",
                      "<copyright-holder>", metadata$copyright_holder,"</copyright-holder>\n",
                      "<license license-type='open-access'>",
                      "<license-p>", metadata$license,"</license-p>",
                      "</license>",
                      "</permissions>", sep = "")

  abstracts = gen_xml_abstracts(metadata$abstracts)

  article_meta = paste("<article-meta>",
                          #"<article-id pub-id-type='publisher-id'>181325198</article-id>", # needed - Ask miguel?$
                          paste0("<article-id pub-id-type='doi'>",metadata$doi,"</article-id>"),
                          "<article-version vocab='JAV' vocab-identifier='http://www.niso.org/publications/rp/RP-8-2008.pdf' vocab-term='Version of Record' article-version-type='VoR'>Version of Record</article-version>", # ask miguel
                          # ask miguel for how they plan around this?
                          "<article-categories>",
                          "<subj-group>",
                          "<subject>X</subject>",
                          "<subj-group>",
                          "<subject>Y</subject>",
                          "</subj-group>",
                          "</subj-group>",
                          "<subj-group>",
                          "<subject>Z</subject>",
                          "<subj-group>",
                          "<subject>W</subject>",
                          "</subj-group>",
                          "</subj-group>",
                          "</article-categories>",

                          # article title
                          paste0("<title-group>\n<article-title>", metadata$title, "</article-title>\n</title-group>"),

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
                                  trimws(anparts[1], whitespace = "[ \t\r\n}{]"),
                                  "</surname>\n</name>")


        } else if (length(anparts) == 2){

          authoritems[j] = paste0("<name>\n<surname>",
                                  trimws(anparts[1], whitespace = "[ \t\r\n}{]"),"</surname>\n",
                                  "<given-names>",trimws(anparts[2], whitespace = "[ \t\r\n}{]"),
                                  "</given-names>\n</name>")

        }
        else {
          hgl_error("invalid number of author name parts ")
        }

      }
      xmlauthors = paste0(authoritems, collapse = "\n")

    } else{
      hgl_warn("ors: No valid author entries in vector")
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
  dr = d_refs[!is.na(citekey)]


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


    volume =        if(!is.na(dr$VOLUME[i]))     paste0("<volume>", dr$VOLUME[i], "</volume>")                     else ""
    issue =         if(!is.na(dr$NUMBER[i]))     paste0("<issue>", dr$NUMBER[i], "</isssue>")                      else ""


    articletitle  = if(!is.na(dr$TITLE[i]))      paste0("<article-title>",
                                                        trimws(dr$TITLE[i], whitespace = "[ \t\r\n}{]"),
                                                        "</article-title>")                                        else ""

    source =        if(!is.na(dr$BOOKTITLE[i]))  paste0("<source>",
                                                        trimws(dr$BOOKTITLE[i], whitespace = "[ \t\r\n}{]"),
                                                        "</source>")                                               else ""

    publisherloc  = if(!is.na(dr$ADDRESS[i]))    paste0("<publisher-loc>", dr$ADDRESS[i],"</publisher-loc>")       else ""
    publishername = if(!is.na(dr$PUBLISHER[i]))  paste0("<publisher-name>", dr$PUBLISHER[i],"</publisher-name>")   else ""
    extlink =       if(!is.na(dr$URL[i]))        paste0("<ext-link ext-link-type='url'>", dr$URL[i],"</ext-link>") else ""
    pubiddoi  =     if(!is.na(dr$DOI[i]))        paste0("<pub-id pub-id-type='doi'>", dr$DOI[i],"</pub-id>")       else ""



    edition = ""
    pages = ""
    if(!is.na(dr$PAGES[i])){

      tspl = stringr::str_split(string = "40-30", pattern = "((--)|-)")[[1]]
      if(length(tspl)> 1){
        pages = paste0("<fpage>",tspl[1],"</fpage>\n<lpage>",tspl[2],"</lpage>")
      } else{
        pages = paste0("<fpage>",tspl[1])
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
      paste0("<label>", clab, "</label>"),
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
gen_xml_displaymath = function(latex, counter = NULL){

  paste0("<disp-formula id='E1'>",
         "<tex-math id='M1'>\\documentclass[12pt]{minimal} \\usepackage{wasysym} \\usepackage[substack]{amsmath} \\usepackage{amsfonts} \\usepackage{amssymb} \\usepackage{amsbsy} \\usepackage[mathscr]{eucal} \\usepackage{mathrsfs} \\DeclareFontFamily{T1}{linotext}{} \\DeclareFontShape{T1}{linotext}{m}{n} { &#x003C;-&#x003E; linotext }{} \\DeclareSymbolFont{linotext}{T1}{linotext}{m}{n} \\DeclareSymbolFontAlphabet{\\mathLINOTEXT}{linotext} \\begin{document} $$",
         latex,
         "$$ \\end{document} </tex-math>",
         "</disp-formula>", sep = "\n")



}
gen_xml_inlinemath = function(latex){


  paste0("<inline-formula>",
         "<tex-math id='M1'>\\documentclass[12pt]{minimal} \\usepackage{wasysym} \\usepackage[substack]{amsmath} \\usepackage{amsfonts} \\usepackage{amssymb} \\usepackage{amsbsy} \\usepackage[mathscr]{eucal} \\usepackage{mathrsfs} \\DeclareFontFamily{T1}{linotext}{} \\DeclareFontShape{T1}{linotext}{m}{n} { &#x003C;-&#x003E; linotext }{} \\DeclareSymbolFont{linotext}{T1}{linotext}{m}{n} \\DeclareSymbolFontAlphabet{\\mathLINOTEXT}{linotext} \\begin{document} $$",
         latex,
         "$$ \\end{document} </tex-math>",
         "</inline-formula>", sep = "\n")
}
gen_xml_paragraphs = function(ptext,d_inlinemath){

  res = vector("character", length = length(ptext))

  # parse rmarkdown to xml- but keep in vector hence the loop
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

  # for JATS we need more elaborate parsing
  headi = matrix(data = 0, nrow = NROW(doc_summar), ncol = 5)
  headi[,1] = doc_summar[,tolower(style_name) == "heading 1"]
  headi[,2] = doc_summar[,tolower(style_name) == "heading 2"]
  headi[,3] = doc_summar[,tolower(style_name) == "heading 3"]
  headi[,4] = doc_summar[,tolower(style_name) == "heading 4"]
  headi[,5] = doc_summar[,tolower(style_name) == "heading 5"]

  # get row indices with 1 entries
  hinds = which(headi |> rowSums() > 0 )

  # 1. fill everything right of a level 1 cell with 2 to mark an interruption through an upper level
  for(ci in hinds) headi[ci, ] = cumsum(headi[ci, ])+shift(x = cumsum(headi[ci, ]), n = 1, fill = 0)


  # 2. insert xml section start and endtags
  hxml_opentags = rep("", times = NROW(headi))
  hxml_endtags = rep("", times = NROW(headi))
  for(crow in hinds) {

    # current heading start
    ccol = which(headi[crow,] == 1)

    hxml_opentags[crow] = paste0(hxml_opentags[crow],"<sec><title>",doc_summar$text[crow], "</title>")

    # find section end -> next row with 1 or 2 in the same column
    if(crow < NROW(headi)) {
      sstart = crow+1

      lastrow = which(headi[sstart:NROW(headi), ccol] > 0)

      # was there no 1 or 2 below the current row in this column? then sedtion ends at the last row
      if(length(lastrow) == 0) lastrow = NROW(headi) else lastrow = lastrow[1]+sstart -1

    } else  lastrow = NROW(headi)


    hxml_endtags[lastrow] = paste0("</sec>", hxml_endtags[lastrow])

    #cat("cr: ", crow, "lr: ", lastrow, "\n")

  }
  return(list(hxml_opentags = hxml_opentags, hxml_endtags = hxml_endtags))
}
gen_xml_file = function(doc_summar, article_type, xml_meta, xml_references){

  xml_article_type = ""

  if(tolower(article_type) == "original research"){

    xml_article_tuype = "research-article"

  } else if(olower(article_type) == "review"){

    xml_article_type = "review-article"

  } else if(olower(article_type) == "comment"){

    xml_article_type = "article-commentary"
  }
  else{
    stop("unkown article typer for JATS conversion")
  }

  header = paste("<?xml version='1.0' encoding='UTF-8'?>",
                  "<!DOCTYPE article PUBLIC",
                  "'-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.3 20210610//EN'",
                  "'JATS-journalpublishing1-3.dtd'>",
                  paste0("<article article-type='",xml_article_type, "'"),
  "dtd-version='1.3'",
  "xml:lang='en'",
  "xmlns:mml='http://www.w3.org/1998/Math/MathML'",
  "xmlns:xlink='http://www.w3.org/1999/xlink'",
  "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' >",
                 xml_meta, sep = "\n")

  # get rid of NA entires - could be unsave though in case NAs do not match over rows
  secopen = doc_summar$xml_secopen
  text    = doc_summar$xml_text
  secend  = doc_summar$xml_secend

  secopen[is.na(secopen)] = ""
  text[is.na(text)] = ""
  secend[is.na(secend)] = ""

  xmltext_rows = paste(trimws(secend), trimws(text), trimws(secopen))

  mid = paste0(xmltext_rows[nchar(xmltext_rows) > 0], collapse = "\n")

  back = paste0("<back>",xml_references,"</back>", sep="\n")

  end = "</article>"

  res = paste(header, "<body>", mid, "</body>", back, end, sep = "\n")

}
gen_xml_table = function(ct_csv, tab_opts, tab_counter){

  # label. to autogenerate
  # caption title  - is it needed?
  ct_csv_o = ct_csv

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
    hgl_warn(paste0("Table #",  tab_counter ,  " - ", label ,": no header row provided (=first cell in column starting with '#'), assuming first row is header row. XML Jats needs at least 1 header row."))
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
    row_strings[i] = paste0("<tr>", paste0("<",celltags[i],">", ct_csv[i,] |> unlist(), "</", celltags[i], ">", collapse = ""), "</tr>")
  }
  tbodystring = paste0(row_strings, collapse = "\n")

  table_xml = paste0("<table-wrap id='", id ,"'>",
   paste("<label>", label, "</label>"),
   "<caption>",
    #"<title>Patient Care at End of Follow Up</title>", caption title if ever
     paste0("<p>", tab_caption, "</p>"),
   "</caption>",
   "<table ",rf_str," cellpadding='3'>",
    colgroup,
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
gen_xml_figure = function(fig_opts, fig_counter){

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
    fig_src = paste0("../",fig_opts['src']) # add one level up because its not in the build dir but one level above
  }
  else{
    stop("Figure src not provided")
  }
  figure_xml = paste(paste0("<fig id='", id, "'>"),
                    paste0("<label>", label, "</label>"),
                    "<caption>",
                    paste0("<p>", fig_caption,"</p>"),
                    "</caption>",
                    paste0("<graphic xmlns:xlink='http://www.w3.org/1999/xlink' xlink:href='", fig_src, "'>"),
                    "</graphic>
                    </fig>", sep = "\n")

  figure_xml
}
