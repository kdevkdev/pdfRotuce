#' Converts word .docx files to .Rmd files that can be rendered to PDFs using latex
#'
#' @import data.table
#' @param src_docx
#' @param working_folder
#' @param meta_csv
#' @param reference_parsing Default `TRUE`. IF `FALSE`, copy references as is without automated parsing using anystyle.
#'
#' @return
#' @export
#'
#' @examples
markdownify = function(src_docx, working_folder = ".", meta_csv = NULL, rmd_outpath = NULL, reference_parsing = F, url_parsing = T, doi_parsing = T, guess_refnumbers = T){

  # a4: 210, 297, 15 mm left/right margin, 12.5 top/bottom
  type_width = 180
  type_height = 272

  fig_capts =
    tab_capts = c()

  # read file and backup object
  docx = officer::read_docx(src_docx)
  doc_summar_o = doc_summar  = data.table::as.data.table(officer::docx_summary(docx,preserve = T,remove_fields = T))
  doc_summar[is.na(style_name), style_name := ""]

  # get all l1 headings
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  title_page_inds = l1_inds[1]:(l1_inds[2]-1)

  # find and parse titele page - first heading 1 up to second heading 1
  parsed_meta = parse_title_page(doc_summar[title_page_inds,])





  # predefined metadata
  predef_meta  = list(articledates = NULL, volume = NULL, copyright_year = NULL,
                      doi = NULL, journal_title = NULL, pageheader = NULL,
                      article_type =NULL, has_abstract = NULL)


  # get metadata from csv
  if(!is.null(meta_csv)){

    pd_tab = read.csv(file = meta_csv, header = F, fileEncoding = "UTF-8")

    # values to a vector and name to vector names
    tvals = pd_tab[[2]]
    names(tvals) = pd_tab[[1]]


    # copy over provided values, uppercase for article typer
    if(is.element("articledates", names(tvals)))     predef_meta$articledates              = tvals["articledates"]           else warning("'articledates' missing in meta csv")
    if(is.element("volume", names(tvals)))           predef_meta$volume                    = tvals["volume"]                 else warning("'volume' missing in meta csv")
    if(is.element("copyright_year", names(tvals)))   predef_meta$copyright_year            = tvals["copyright_year"]         else warning("'copyright_year' missing in meta csv")
    if(is.element("doi", names(tvals)))              predef_meta$doi                       = tvals["doi"]                    else warning("'doi' missing in meta csv")
    if(is.element("pageheader", names(tvals)))       predef_meta$pageheader                = tvals["pageheader"]             else warning("'pageheader' missing in meta csv")
    if(is.element("has_abstract", names(tvals)))     predef_meta$has_abstract              = tvals["has_abstract"]           else warning("'has_abstrat' missing in meta csv")
    if(is.element("article_type", names(tvals)))     predef_meta$article_type              = toupper(tvals["article_type"])  else warning("'article_type' missing in meta csv")

    # if(is.null(parsed_meta$abstracts$mainlang) && predef_meta$has_abstract == "yes"){
    #   stop("abstract mandatory according to metadata.csv, but is not provided")
    # }
  }



  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta)

  preamble_yaml = gen_yaml_header(md =metadata, reference_parsing = reference_parsing)

  # delete  title page from document
  doc_summar = doc_summar[-title_page_inds,]

  # create column to contain markdown text
  doc_summar$mrkdwn = doc_summar$text


  # find,parse,  and remove references
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  refparind = which(tolower(doc_summar$mrkdwn) == 'references' & tolower(doc_summar$style_name) == "heading 1")


  rmd_references = ""
  if(length(refparind) == 0)   warning("No references found!!!")
  else{

    # refinds are those paragraphs until end or next l1 heading
    ti = which(l1_inds > (refparind+1)) #+1 because we donot want to have the heading included



    if(length(ti) == 0) lri = NROW(doc_summar) # no subsequent l1 heading, we go until end
    else lri = l1_inds[ti] -1 #deduct 1 because we do not want to have the next level 1 heading included

    ref_inds = (refparind+1):lri # do not include the starging l1 heading , +1

    # create one large text value
    #references_text = paste(doc_summar$mrkdwn[ref_inds], collapse = "\n")
    references = doc_summar$mrkdwn[ref_inds]

    if(reference_parsing){

      path_temprefs_in  = paste0(working_folder, "/tempreftxts_in")
      path_temprefs_out = paste0(working_folder, "/tempreftxts_out")
      dir.create(path_temprefs_in, showWarnings = FALSE)
      dir.create(path_temprefs_out, showWarnings = FALSE)

      # save single .txt for each reference - do it this way to keep track of the ordering in of original bibliography as much as possible
      sapply(1:length(references), FUN = \(x) { writeLines(text = references[x], con = paste0(path_temprefs_in, "/", x, ".txt")) })

      # save to extracted file
      # fname = paste(working_folder, "references_extracted.txt", sep = "/")
      # print(paste0("Writing extracted refrerences file file: ", fname))
      # writeLines(text = references_text, con = fname)

      # anystyle -f bib parse references_extracted.txt
      print("runing anystyle on extracted references ...")
      #ras = system(paste0("anystyle -f bib,json parse ",  fname , " ", working_folder))
      ras = system(paste0("anystyle -w -f bib,json parse ",  path_temprefs_in , " ", path_temprefs_out))
      stopifnot("anystyle failed" =  ras == 0 )


      # remove from document
      doc_summar = doc_summar[-c(refparind, ref_inds), ]
      rmd_references = "# References\n\n  \n"



      # red back into R using bib2df  and join to big file
      temp_bibfiles_in = sort(paste0(path_temprefs_out, "/", list.files( path = path_temprefs_out, pattern = "*.bib")))

      x = list()
      for(cn in temp_bibfiles_in){
        x[[cn]] <- bib2df::bib2df(cn)


        # parse bibnum and index (index always awailable, number might fail)
        bib_orderindex = gsub(x = basename(cn), replacement = "", pattern = ".bib")
        bib_num <- NA

        # if parsing failed bib2df returns 0 length data. framec
        if(NROW(x[[cn]]) == 0){
          x[[cn]] <- data.frame(bibliography_orderindex = bib_orderindex)
        }else{

          parsed_citnums = NA

          if("NUMBER" %in% colnames(x[[cn]]) && !is.na(x[[cn]]$NUMBER)){
            parsed_citnums = x[[cn]]$NUMBER
            x[[cn]]$NUMBER = NA
          }
          # overwrite if citation.number is given
          if ("CITATION.NUMBER" %in% colnames(x[[cn]]) && !is.na(x[[cn]]$CITATION.NUMBER)){
            parsed_citnums = x[[cn]]$CITATION.NUMBER
            x[[cn]]$CITATION.NUMBER = NA
          }

          bib_num <- gsub(x = parsed_citnums, pattern = "[^0-9]", replacement = "") # only allow alphanumeric
          x[[cn]]$NUMBER = bib_num
        }

        x[[cn]]$bibliography_number = bib_num
        x[[cn]]$bibliography_orderindex = bib_orderindex
      }



      d_refs = rbindlist(l = x, fill = TRUE) # fill with NA by specifying fill = 'TRUE'
      d_refs$bibliography_number = as.numeric(d_refs$bibliography_number)
      setkey(d_refs, bibliography_number) # sort


      # if("CITATION.NUMBER" %in% names(d_refs)){
      #   setnames(d_refs, "CITATION.NUMBER", "CITATION_NUMBER")
      # }

      # get rid of trailing letter appended by anystyle, store in new variable
      d_refs$citekey =  stringr::str_replace_all(pattern = "((?<=[0-9]{4})[a-z]{1})|(-[a-z]{1})", string= d_refs$BIBTEXKEY, replacement = "")


      # check for duplicatees
      dups = unique(d_refs$citekey[duplicated(d_refs$citekey)])

      # append letters to duplicates - hopefully not more than  24
      for(cd in dups){
        stopifnot("more than 24 duplicates for one citekey -> fix code" = d_refs[citekey == cd, .N] < 25)
        d_refs[citekey == cd, citekey:= paste0(citekey, letters[1:NROW(.SD)])]
      }

      # write references.bib
      t = d_refs
      t$BIBTEXKEY = t$citekey
      t$citekey = NULL
      t = dplyr::as_tibble(t)

      bib2df::df2bib(x = t, file = paste0(working_folder, "/", "references.bib")) # bug in the package , use another package
      #RefManageR::WriteBib(RefManageR::as.BibEntry(t), file =paste0(working_folder, "/", "references.bib") )

      # find all in text citatations - idea: three possible modes: text based detection using regex '(' or '[' brackets, field code based detection using xpath, or text based detection using citation keys (this would rquire providing a specializied citation style or hand- in of a bib file)
      # "========protectedcite{}protectedcite
      ttext = trimws(doc_summar$mrkdwn)
      content_inds = setdiff(1:NROW(doc_summar), which(startsWith(ttext, "[[table") | startsWith(ttext, "[[figure")))

      y = stringr::str_extract_all(str = doc_summar$mrkdwn[content_inds], pattern = "\\[(([0-9]+([-–][0-9]+)?)(?:, ?))+\\]") # TODO: does not allow for spaces currently and is rather strict, possibly handle that in the future

      # save original intext citations, seperate by ', '
      doc_summar$orig_citations[content_inds] = lapply(y, FUN = paste, sep = ", ", collapse = ", ")

      # replace intext-citations with citekeys
      doc_summar$mrkdwn[content_inds] = stringr::str_replace_all(str = doc_summar$mrkdwn[content_inds], pattern = "\\[(([0-9]+([-–][0-9]+)?)(?:, ?)?)+\\]", replacement = \(match) {

        # first get rid of encompassing [], then split into individual components 'ranges'
        t = stringr::str_replace_all(string = match, pattern = "\\[|\\]", replacement = "")
        ranges = stringr::str_split(string = t, pattern = ",", simplify = T)


        # go through ranges, if necessary expand and retrieve and add citation key to list
        locrefs_nums = list()
        locrefs_keys = list()
        for(crange in ranges){

          # remove possible trailing whitespace
          crange = trimws(crange)

          if(grepl(x = crange, pattern = "[-–]")){

            # range, extract start and end, and then create sequence (regex using positive lookahead and lookbehind)
            start = stringr::str_extract(string= crange, pattern = "^[0-9]{1,}(?=[-–])")
            end = stringr::str_extract(string= crange, pattern = "(?<=[-–])[0-9]{1,}$")
            trefnums = start:end
          }
          else{
            trefnums = as.numeric(crange)
          }
          # store results
          locrefs_nums[[crange]] = trefnums
          locrefs_keys[[crange]] = d_refs[bibliography_number %in% trefnums]$citekey

        }
        # construct string to return
        replacement = paste0("[", paste0("========protectedat========", unlist(locrefs_keys),collapse = ";") , "]")
        replacement
      })

    }
    else { # end condition refernce_parsing

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
                                          pattern = stringr::regex("\\b(?<!/)10.\\d{4,9}/[-._;()/:A-Z0-9]+\\b", ignore_case = T),
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
  }



  # there seems to be a problem with case sensitivity and style names between libreoffice and word, use tolowe (word uses capital, libreoffice not)
  # doc_summar[tolower(style_name) == "heading 1" & startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", substr(mrkdwn, 2, nchar(mrkdwn)), " {-}")] # do not put in TOC



  # convert headings to markdown up to level 5
  doc_summar[tolower(style_name) == "heading 1", mrkdwn := paste0("# ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 2", mrkdwn := paste0("##  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 3", mrkdwn := paste0("###  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 4", mrkdwn := paste0("####  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 5", mrkdwn := paste0("#####  ", mrkdwn)]
  cpart = doc_summar

  # process tables
  cudoc_tabinds = unique(cpart[content_type == "table cell"]$doc_index)

  tab_counter = 1 # we keep a specific ounter, if ever cti would deviate because e.g. of failure


  for(cti in cudoc_tabinds){


    # detect empty paragraphs ahead
    empties = cpart[, doc_index > cti & !grepl(x = mrkdwn, pattern = "$\\s?^")]
    ind_next_nonempty = min(which(empties)) # first non-empty

    tab_opts_raw = cpart[ind_next_nonempty,]

    if(tab_opts_raw$content_type != "paragraph" && !startsWith(trimws(tab_opts_raw$mrkdwn, "[[table"))){
      warning("Table " %+% cti %+% " does not seem to have have a table tag")
    }else{


    }


    ct_dat = cpart[content_type == "table cell" & doc_index == cti]

    # if we need we can take into acount the header here (is_header column in doc_summar)
    ct_csv = data.table::dcast(ct_dat, row_id ~ cell_id, value.var = "mrkdwn")[,-1] # not first

    ctab_chunk = gen_tabchunk(ct_csv, tab_opts_raw, tab_counter, folder = working_folder)

    # delete table rows and table options form document structure
    cpart = data.table::rbindlist(l = list(cpart[doc_index < cti],
                                           data.table::data.table(doc_index = cti, content_type = "paragraph", text = ctab_chunk, mrkdwn = ctab_chunk, is_heading1 = F, is_header = F),
                                           cpart[doc_index > cti &doc_index != tab_opts_raw$doc_index]), fill = T)

    tab_counter = tab_counter +1

  }
  # write to file


  chunk_setup = "```{r SetupUpset,include=F}

#  source('rabul.R')

```"

  ########################################### command parsing ####################################################

  # inline paragraph commands
  # put protected dollar fo rinline mathc(
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", replacement = "========protecteddollar========\\1========protecteddollar========")]

  # put protected at for refs
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\@ref\\((.+?)\\)", replacement = "\\========protectedat========ref(\\1)")]

  # noindent
  cpart[, mrkdwn:= gsub(x = trimws(mrkdwn), pattern = "^\\[\\[noindent\\]\\](.?)", replacement = "\\\\noindent \\1")]


  # look for commands whole para tag commands [[]]
  command_inds =  which(startsWith(trimws(cpart$mrkdwn), "[[") & endsWith(trimws(cpart$mrkdwn),"]]"))

  # escape dollar in suitable paragraphs
  cpart[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\$", replacement = "\\\\$")]
  cpart[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\@", replacement = "\\\\@")]
  cpart[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\%", replacement = "\\\\%")]
  fig_counter = 1


  if(length(command_inds) > 0 ){

    for(c_comi in command_inds){


      # get current command
      c_comtext = cpart[c_comi]$mrkdwn

      c_command = parse_yaml_cmds(c_comtext)

      print(c_comtext |> substr(1,100))




      # very primitive command parsing first position contains command
      c_result = ""

      if(length(c_command) > 0 & !inherits(c_command, "error")){

        switch(c_command[[1]],
               interbox={
                 print("interbox detected")
                 title = c_command['title']
                 text = c_command['text']
                 iblabel = paste0("Part",  cpart_id, "Com", c_comi)
                 c_result = "
::: {.interbox data-latex=\"{"%+% title %+% "}{" %+% iblabel %+% "}\"}
" %+% text %+% "
:::

"
               },
               figure={
                 print("figure detected")
                 # src = c_command['src']
                 # cap = c_command['cap']


                 c_result = gen_figblock(fig_opts = c_command, fig_counter = fig_counter)
                 #
                 #                  c_result = "```{r "%+% flabel %+%",out.width='100%',echo=F, fig.align='center',fig.cap='(ref:cap" %+% flabel %+%")'}
                 # knitr::include_graphics(path='" %+% src %+% "')
                 # ```
                 # "

               },
               math={
                 print("Math detected")
                 form = c_command['form']
                 c_result = paste0("$$", trimws(form), "$$")
               },
               quote={
                 print("quote detected")
                 c_result = paste0("\\hyphenblockcquote{", trimws(c_command['text']), "}")
                 c_result = "\\vskip 2mm
::: {.displayquote data-latex=\"{  }\"}

::: {.enquote data-latex=\"{ " %+% trimws(c_command['text']) %+% " }\"}
\\phantom{}
:::

:::

"},
               columnbreak = {
                 c_result = "\\columnbreak"})
      }
      else{

        print("element arguments: yaml parsing error:")
        #print(c_comtext)
      }

      # replace the paragraph with the result
      cpart[c_comi]$mrkdwn = c_result

    }
  }


  # statements and declarations
  rmd_statements = ''

  # any statements? populate corresponding article part, each with its own subheading
  if(length(metadata$statements) > 0){

    rmd_statements = "\n\n# Declarations\n\n"
    for(cn in names(metadata$statements)){

      cstat <- metadata$statements[[cn]]
      rmd_statements = rmd_statements %+% "## " %+%cn %+% "\n" %+% "\\noindent " %+% cstat %+% "\n\n"
    }
  }



  # other language abstracts
  rmd_multilang_abstracts = ""
  if(any(names(metadata$abstracts)!="mainlang")){

    # figure out what additional languages there are (remove mainlang, usually english)
    side_langs = setdiff(names(metadata$abstracts),"mainlang")

    side_langs_abstracts = metadata$abstracts[side_langs]




    rmd_multilang_abstracts = "\n```{=latex}\n\\end{multicols}\n\\vspace{2mm}"
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

    rmd_multilang_abstracts = rmd_multilang_abstracts %+% "\\end{tcolorbox}\\vspace{3mm}\\begin{multicols}{2}\n```\n\n"

  }


  ########################################### postprocessing  & writing file ####################################################
  # replace back protected dollars , @, etc
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "========protecteddollar========", replacement = "$")]
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "========protectedat========", replacement = "@")]

  cpart$sep = "\n\n"

  outmrkdwn = cpart[, paste0(rbind(mrkdwn, sep), collapse = "")]


  # put orcid section
  author_orcinds <- sapply(X = metadata$authors, FUN = \(x){

    grepl(x = gsub(x = x$orcid, pattern = "[^0-9X]", replacement= ""), pattern = "[0-9X]{16}")
  })


  # if any orcids present, put all authors with oricds in a separate section before the references
  rmd_orcinds = ""
  if(!is.null(author_orcinds) & length(author_orcinds) > 0){

    td <- metadata$authors[author_orcinds] |> rbindlist()

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


  rmd_text = c(preamble_yaml, chunk_setup, fig_capts, tab_capts, outmrkdwn,
               rmd_statements,
               rmd_orcinds,
               rmd_multilang_abstracts,
               rmd_references)


  # write rmd file if filename provided
  if(!is.null(rmd_outpath)){
    write(rmd_text, file = rmd_outpath) # overwrites if existing
  }

  return(rmd_text)
}



