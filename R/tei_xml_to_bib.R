# https://tei-c.org/release/doc/tei-p5-doc/en/html/CO.html#COBICOL
tei_xml_to_bib = function(xml_obj, orderindex = ""){

  xslt =  xml2::read_xml(system.file("teixml_to_bib.xsl", package="pdfRotuce"))
  xslt_params = list(citekey = '', year = '', edition = '') # all need to be predefined (xslt:if throws error otherwise)

  # xslt in R only currently possible with libxslt which supports (mostly) only xslt 1.0 without regex, hence preparse the citekey here to pass it as a parameter
  # (deduplication happens later)
  first_name = tolower(xml2::xml_text(xml2::xml_find_first( xml_obj, "//persName/surname|author|editor")))
  first_year = xml2::xml_text(xml2::xml_find_first(xml_obj, "//date/@when|//date")) |> stringr::str_extract(pattern = "[0-9]{4}")

  # edition may not be put in the correct tag by grobid, but in the 'note' tags
  notes = xml2::xml_find_all(xml_obj, "//note")

  # try to detect edition strings using regex, if match remove according node
  for(cn in notes) {
    if(stringr::str_detect(xml2::xml_text(cn), pattern = stringr::regex("[0-9]+([.a-zA-Z]* )ed", ignore_case = T))) { # hopefully nothing else is inside this node

      xslt_params$edition = xml2::xml_text(cn)
      xml2::xml_remove(cn)
    }
  }


  if(is.na(first_year)) first_year = orderindex
  if(is.na(first_name)) first_name = "undefauthor"

  xslt_params$citekey = paste0(first_name, first_year)
  xslt_params$year = as.character(first_year)

  bib = xslt::xml_xslt(xml_obj, xslt, params = xslt_params)

  bib
  # find all biblstruct (looks like we do not get <bibl> from grobid)
  # l_biblstruct = xml2::xml_find_all(xml_obj, "//biblStruct")
  #
  #
  # # one biblstruct  an contain analytic, monograph, and series, corresponding to information
  # # analytic: one entry in a monograph (eg journal article, book chapter)
  # # monograph: independent publishable entity (journal , book)
  # # series: collection of articlses  or books
  #
  # # write loop even if only one item expected
  # for(bi in 1:length(l_biblstruct)){
  #
  #   # xml nodeset retunred by xml_find_all  is like a list -> we use R length() and list indexing [[]]
  #   cbiblstruct = l_biblstruct[[bi]]
  #
  #   # find first analytic (according to schema tei schema multiple possible ('*') - but not sure what multiple analtyic elements would signify)
  #   node_analytic = xml2::xml_find_first(cbiblstruct, "./analytic")
  #   node_title_analytic = xml2::xml_find_first(node_analytic, "./title")
  #
  #   # also find first monograhg - multiple possible if published in multiple places, but do not handle this case. Is the only element that must occur at least once
  #   node_monogr = xml2::xml_find_first(cbiblstruct, "./monogr")
  #   node_title_monogr = xml2::xml_find_first(node_monogr, "./title")
  #   titletype_monogr = xml2::xml_attr(node_title_monogr, attr = "level")
  #
  #   if(is.na(titletype_monogr)) titletype_monogr = "m" # implicit 'default' value according to TEI xml standard
  #
  #   # same for series
  #   node_series = xml2::xml_find_first(cbiblstruct, "./series")
  #   node_title_series = xml2::xml_find_first(node_series, "./title")
  #
  #   # detect type -> NA if not found
  #   if(!is.na(node_analytic) && !is.na(node_monogr)){
  #
  #     if(titletype_monogr == "m"){
  #
  #       # analytic in a monograph -> e.g. book chapter?
  #
  #       # bibtexfields: author, editor, title, pages, booktitle, publisher, address, edition, year, (issn|isbn doi)
  #
  #     } else if(titletype_monogr == "j"){
  #
  #       # journal article
  #
  #       # bibtexfields: author, title, journal, number, volume, pages, year, (doi)
  #       # -> 'number' refers to issuenumber
  #     }
  #   }
  #   else if(is.na(node_analytic) && !is.na(node_monogr)){
  #
  #     # book / rapport / grej litterature of single publication
  #
  #     # book bibtexfields: title, pages, booktitle, publisher, address, edition, year (issn|isbn, doi)
  #     # rapport bibtexfields: title, pages, institution|publisher, address, edition, year (issn|isbn, doi, url)
  #
  #
  #     # look for 'imporint' information (publisher, pace, time)
  #     # maybe contains edition in note
  #   }
  #
  # }
  #
  # r_btx = list()
  #
  # # fetch all the bibtex fields we can -> see list below
  # node_pubplace = xml2::xml_find_first(cbiblstruct, "./monogr/imprint/pubPlace")
  # nodes_author  = xml2::xml_find_all(cbiblstruct, "./analytic/author")
  #
  # authornames = vector("character", length(nodes_author))
  # for(cia in 1:length(nodes_author)){
  #
  #   cauth = nodes_author[[cia]]
  #
  #
  #   if(length(xml2::xml_children(cauth)) > 0 ){
  #
  #     cacont = cauth |> xml2::xml_child(1)
  #
  #     # try to see if there is more children that make up this author node
  #     authornames[cia] = switch(xml2::xml_name(cacont),
  #            "persName" = {
  #
  #               surname = ""
  #               ts = xml2::xml_find_all(cauth, "./persName/surname")
  #               if(!is.na(ts)) surname = paste0(unlist(xml2::as_list(ts)), collapse = " ") # concat all surnames
  #
  #               forename = ""
  #               ts = xml2::xml_find_all(cauth, "./persName/forename")
  #               if(!is.na(ts)) forename = paste0(unlist(xml2::as_list(ts)), collapse = " ") # concat all forenames
  #
  #
  #               # concat surnames and fornenames
  #               paste0(surname, ", ", forename)
  #            },
  #            {
  #               hgl_warn_S("tei_xml_to_bib: unhandled author subtype - possible loss of authorship information")
  #               paste0(unlist(xml2::as_list(cauth)), collapse = " ")
  #
  #            })
  #   } else{
  #
  #     authornames[cia] = xml2::xml_text(cauth)
  #   }
  #
  # }
  # # concat all authors and put 'and' in between
  # r_btx$author = paste0(authornames, collapse = " and " )


  # generate citekey




  # mapping to bibtex types - according to my (preliminary) understanding:
  # one analytic  and one monograph -> journal article, monogrph has title type 'j'
    # -> bibtex type 'article'
  # one monograph -> book or report, monograph has title type 'm' or or is undefined
    # -> bibtex type 'book'  (or 'techreprort' but maybe not distinguishable? )
  # chapter in book: one analytic , one monograph ->  monograph has title type 'm' or is undefined
    # -> bibtex type 'incollecition' ('collection' for the entirety of an edited book)  # https://github.com/retorquere/zotero-better-bibtex/issues/877


  # bibtex types:
  # article: any article published in a periodical like a journal article or magazine article
  # book: a book
  # booklet: like a book but without a designated publisher
  # conference: a conference paper
  # inbook: a section or chapter in a book
  # incollection: an article in a collection
  # inproceedings: a conference paper (same as the conference entry type)
  # manual: a technical manual
  # masterthesis: a Masters thesis
  # misc: used if nothing else fits
  # phdthesis: a PhD thesis
  # proceedings: the whole conference proceedings
  # techreport: a technical report, government report or white paper
  # unpublished: a work that has not yet been officially published



  ## Standard field types in bibtex and where to get the info in tei XML
  #
  # address: address of the publisher or the institution                                        -> monogr/imprint/pubPlace
  # annote: an annotation                                                                       -> ?
  # author: list of authors of the work                                                         -> (analytic|monogr)/author
  # booktitle: title of the book                                                                -> (analytic|monogr)/title
  # chapter: number of a chapter in a book                                                      -> monogr/biblScope ?
  # edition: edition number of a book                                                           -> monogr/edition (but grobid puts it as biblstruct/note)
  # editor: list of editors of a book                                                           -> (analytic|monogr|series)/editor
  # howpublished: a publication notice for unusual publications                                 -> ?
  # institution: name of the institution that published and/or sponsored the report             -> monogr/imprint/publisher
  # journal: name of the journal or magazine the article was published in                       -> monogr/title
  # month: the month during the work was published                                              -> monogr/imprint/date | analytic/date | biblscope/date
  # note: notes about the reference                                                             -> monogr/note | monogr/imprint/note
  # number: number of the report or the issue number for a journal article                      -> (monogr|imprint)/biblscope[unit=issue]
  # organization: name of the organizing/sponsoring/publishing institution                      -> unclear, maybe (imprint|monogr|series)/respStatement, or distributor funder ...
  # pages: page numbers or a page range                                                         -> (monogr|imprint)biblscope[from= , to= ]
  # publisher: name of the publisher                                                            -> monogr/imprint/publisher
  # school: name of the university or degree awarding institution                               -> ?
  # series: name of the series or set of books                                                  -> series/title
  # title: title of the work                                                                    -> (analytic|monogr)/title
  # type: type of the technical report or thesis                                                -> ? detucted?
  # volume: volume number                                                                       -> (monogr|imprint)/biblscope[unit=volume]
  # year: year the work was published                                                           -> see month above
  #
  ## Non-standard field types
  #
  # These fields are frequently used, but are not supported by all BibTeX styles.
  #
  # doi: DOI number (like 10.1038/d41586-018-07848-2)                                           -> (analytic|monogr)/idno[type = 'DOI']
  # issn: ISSN number (like 1476-4687)                                                          -> .,,idno...
  # isbn: ISBN number (like 9780201896831)                                                      -> .,,idno...
  # url: URL of a web page                                                                      -> .,,idno...

}

