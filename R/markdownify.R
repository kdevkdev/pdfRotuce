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
markdownify = function(src_docx, working_folder = ".", meta_csv = NULL, rmd_outpath = NULL, xml_outpath = NULL,reference_parsing = F, url_parsing = T, doi_parsing = T, guess_refnumbers = T){

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


    # if(is.null(parsed_meta$abstracts$mainlang) && predef_meta$has_abstract == "yes"){
    #   stop("abstract mandatory according to metadata.csv, but is not provided")
    # }
  }
  hardcoded_meta = gen_hardcoded_meta(reference_parsing = reference_parsing)



  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta, hardcoded_meta)

  yaml_preamble = gen_yaml_header(md =metadata, reference_parsing = reference_parsing)


  xml_front = gen_xml_header(metadata)


  # delete  title page from document
  doc_summar = doc_summar[-title_page_inds,]

  # create column to contain markdown text
  doc_summar$mrkdwn = doc_summar$text

  # create xml nodes list for JATS xml, special format in that it is embedded into the doc dataframe, needs to have same length as NROW
  # that is, each row as a cell/list entry for corresponding xml nodes
  doc_summar$xml = vector(mode = "list", length = NROW(doc_summar))
  doc_summar$xml_text = ""
  doc_summar$xml_temp = doc_summar$text
  doc_summar$xml_type = character()
  doc_summar$xml_type = NA



  ## @@@@ doc_sumar will contain: text (original word contents), mrkdwn  (processed markedown), xml_temp (intermediate 'working' xml text ), xml_text (final xml conform text


  # remove [empty] headings
  doc_summar = doc_summar[!(startsWith(tolower(style_name), "heading") & trimws(tolower(mrkdwn)) == "[empty]"),]

  # find,parse,  and remove references
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  refparind = which(tolower(doc_summar$mrkdwn) == 'references' & tolower(doc_summar$style_name) == "heading 1")


  rmd_references = ""
  if(length(refparind) == 0)   hgl_warn("No references found!!!")
  else{

    # refinds are those paragraphs until end or next l1 heading
    ti = which(l1_inds > (refparind+1)) #+1 because we donot want to have the heading included



    if(length(ti) == 0) lri = NROW(doc_summar) # no subsequent l1 heading, we go until end
    else lri = l1_inds[ti] -1 #deduct 1 because we do not want to have the next level 1 heading included

    ################## reference parsing ##################################
    ref_inds = (refparind+1):lri # do not include the starging l1 heading , +1

    # create one large text value
    #references_text = paste(doc_summar$mrkdwn[ref_inds], collapse = "\n")
    references =  doc_summar$mrkdwn[ref_inds]

    if(reference_parsing){

      path_temprefs_in  = paste0(working_folder, "/tempreftxts_in")
      path_temprefs_out = paste0(working_folder, "/tempreftxts_out")
      dir.create(path_temprefs_in, showWarnings = FALSE)
      dir.create(path_temprefs_out, showWarnings = FALSE)

      # remove number from beginning
      ref_itemnums = stringr::str_extract(string = references, pattern = "^[0-9]+[.]")
      references = stringr::str_replace(string = references, pattern = "^[0-9]+[.]", replacement = "")

      # save single .txt for each reference - do it this way to keep track of the ordering in of original bibliography as much as possible
      sapply(1:length(references), FUN = \(x) { writeLines(text = references[x], con = paste0(path_temprefs_in, "/", x, ".txt")) })

      # remove from document
      doc_summar = doc_summar[-c(refparind, ref_inds), ]
      rmd_references = "# References\n\n  \n"

      # run anystyle
      print("runing anystyle on extracted references ...")
      ras = system(paste0("anystyle -w -f bib,json parse ",  path_temprefs_in , " ", path_temprefs_out))
      stopifnot("anystyle failed" =  ras == 0 )



      # red back into R using bib2df  and join to big file, do numerical sort
      temp_bibfiles_in = stringr::str_sort(paste0(path_temprefs_out, "/", list.files( path = path_temprefs_out, pattern = "*.bib")), numeric = T)




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
# #
#           if("NUMBER" %in% colnames(x[[cn]]) && !is.na(x[[cn]]$NUMBER)){
#             parsed_citnums = x[[cn]]$NUMBER
#             x[[cn]]$NUMBER = NA
#           }
#           # overwrite if citation.number is given
#           if ("CITATION.NUMBER" %in% colnames(x[[cn]]) && !is.na(x[[cn]]$CITATION.NUMBER)){
#             parsed_citnums = x[[cn]]$CITATION.NUMBER
#             x[[cn]]$CITATION.NUMBER = NA
#          }

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

      y = stringr::str_extract_all(str = doc_summar$mrkdwn[content_inds], pattern = "\\[(([0-9]+([-–][0-9]+)?)(?:, ?)?)+\\]") # TODO: does not allow for spaces currently and is rather strict, possibly handle that in the future

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
  }


  # there seems to be a problem with case sensitivity and style names between libreoffice and word, use tolowe (word uses capital, libreoffice not)
  # doc_summar[tolower(style_name) == "heading 1" & startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", substr(mrkdwn, 2, nchar(mrkdwn)), " {-}")] # do not put in TOC


  ############## headings ##############
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
  doc_summar[, xml_temp:= stringr::str_replace_all(string = text, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]",
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
                 c_result_xml = gen_xml_figure(fig_opts = c_command, fig_counter = fig_counter)
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

  # any statements? populate corresponding article part, each with its own subheading
  if(length(metadata$statements) > 0){

    rmd_statements = "\n\n# Declarations\n\n"
    for(cn in names(metadata$statements)){

      cstat <- metadata$statements[[cn]]
      rmd_statements = rmd_statements %+% "## " %+%cn %+% "\n" %+% "\\noindent " %+% cstat %+% "\n\n"
    }
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
               "\\end{multicols}",
               "```")

  #[[figure,src: 'figures/Fig4.png', wide, caption: 'Promotores de salud capacitados.', label: 'fig4']]



  # write rmd file if filename provided
  if(!is.null(rmd_outpath)){
    write(rmd_text, file = rmd_outpath) # overwrites if existing
  }

  # write xml file if filename provided
  if(!is.null(rmd_outpath)){
    xml_text = paste0(gen_xml_file(doc_summar, article_type = metadata$article_type, xml_meta  = xml_front))
    #write(xml_text, file = xml_outpath) # overwrites if existing
  }

  return(rmd_text)
}



