#' Converts word .docx files to .Rmd files that can be rendered to PDFs using latex
#'
#' @import data.table
#' @param src_docx
#' @param working_folder
#' @param meta_csv
#'
#' @return
#' @export
#'
#' @examples
markdownify = function(src_docx, working_folder = ".", meta_csv = NULL, rmd_outpath = NULL, reference_parsing = T){

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
  predef_meta  = list(articledates = NULL, volume = NULL, copyright_year = NULL, doi = NULL, journal_title = NULL, pageheader = NULL)


  # get metadata from csv
  if(!is.null(meta_csv)){

    pd_tab = fread(meta_csv, header = F)

    # values to a vector and name to vector names
    tvals = pd_tab[[2]]
    names(tvals) = pd_tab[[1]]


    # copy over provided values
    if(is.element("articledates", names(tvals)))     predef_meta$articledates     = tvals["articledates"]         else warning("'articledates' missing in meta csv")
    if(is.element("volume", names(tvals)))           predef_meta$volume           = tvals["volume"]               else warning("'volume' missing in meta csv")
    if(is.element("copyright_year", names(tvals)))   predef_meta$copyright_year   = tvals["copyright_year"]       else warning("'copyright_year' missing in meta csv")
    if(is.element("doi", names(tvals)))              predef_meta$doi              = tvals["doi"]                  else warning("'doi' missing in meta csv")
    if(is.element("pageheader", names(tvals)))       predef_meta$pageheader       = tvals["pageheader"]           else warning("'pageheader' missing in meta csv")
  }



  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta)

  preamble_yaml = gen_yaml_header(md =metadata)

  # delete  title page from document
  doc_summar = doc_summar[-title_page_inds,]

  # create column to contain markdown text
  doc_summar$mrkdwn = doc_summar$text


  # find,parse,  and remove references
  l1_inds = which(tolower(doc_summar$style_name) == "heading 1")
  refparind = which(tolower(doc_summar$mrkdwn) == 'references' & tolower(doc_summar$style_name) == "heading 1")


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
      doc_summar = doc_summar[- ref_inds, ]



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
        replacement = paste0("[", paste0("@", unlist(locrefs_keys),collapse = ";") , "]")
        replacement
      })

    }
   else { # end condition refernce_parsing

      # do not change in text citations

   }
  }



  # there seems to be a problem with case sensitivity and style names between libreoffice and word, use tolowe (word uses capital, libreoffice not)
  doc_summar[tolower(style_name) == "heading 1" & !startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 1" & startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", substr(mrkdwn, 2, nchar(mrkdwn)), " {-}")]



  # convert headings to markdown up to level 5
  doc_summar[tolower(style_name) == "heading 2", mrkdwn := paste0("##  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 3", mrkdwn := paste0("###  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 4", mrkdwn := paste0("####  ", mrkdwn)]
  doc_summar[tolower(style_name) == "heading 5", mrkdwn := paste0("#####  ", mrkdwn)]
  cpart = doc_summar

  # process tables
  cudoc_tabinds = unique(cpart[content_type == "table cell"]$doc_index)

  tab_counter = 1 # we keep a specific ounter, if ever cti would deviate because e.g. of failure


  for(cti in cudoc_tabinds){



    tab_opts_raw = cpart[content_type == "paragraph" & doc_index == (cti+1)]

    ct_dat = cpart[content_type == "table cell" & doc_index == cti]

    # if we need we can take into acount the header here (is_header column in doc_summar)
    ct_csv = data.table::dcast(ct_dat, row_id ~ cell_id, value.var = "mrkdwn")[,-1] # not first

    ctab_chunk = gen_tabchunk(ct_csv, tab_opts_raw, tab_counter, folder = working_folder)

    cpart = data.table::rbindlist(l = list(cpart[doc_index < cti],
                                         data.table::data.table(doc_index = cti, content_type = "paragraph", text = ctab_chunk, mrkdwn = ctab_chunk, is_heading1 = F, is_header = F),
                                         cpart[doc_index > cti]), fill = T)

    tab_counter = tab_counter +1

  }
  # write to file


  chunk_setup = "```{r SetupUpset,include=F}

#  source('rabul.R')

```"

  ########################################### command parsing ####################################################

  #inline paragraph commands
  # inline mathc(
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", replacement = "========protecteddollar========\\1========protecteddollar========")]


  # look for commands [[]]
  command_inds =  which(startsWith(trimws(cpart$mrkdwn), "[[") & endsWith(trimws(cpart$mrkdwn),"]]"))

  # escape dollar in suitable paragraphs
  cpart[!startsWith(mrkdwn, "$") & !startsWith(mrkdwn, "```{r") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\$", replacement = "\\\\$")]
  cpart[!startsWith(mrkdwn, "$") & !startsWith(mrkdwn, "```{r") ,mrkdwn := stringr::str_replace_all(mrkdwn, pattern = "\\%", replacement = "\\\\%")]
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


                 c_result = gen_figchunk(fig_opts = c_command, fig_counter = fig_counter)
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
               })
           }
           else{

             print("element arguments: yaml parsing error:")
             #print(c_comtext)
           }

           # replace the paragraph with the result
           cpart[c_comi]$mrkdwn = c_result

      }
  }


  ########################################### postprocessing  & writing file ####################################################
  # replace back protected dollars
  cpart[, mrkdwn:= gsub(x = mrkdwn, pattern = "========protecteddollar========", replacement = "$")]

  cpart$sep = "\n\n"

  outmrkdwn = cpart[, paste0(rbind(mrkdwn, sep), collapse = "")]


  rmd_text = c(preamble_yaml, chunk_setup, fig_capts, tab_capts, outmrkdwn)


  # write rmd file if filename provided
  if(!is.null(rmd_outpath)){
    write(rmd_text, file = rmd_outpath) # overwrites if existing
  }

  return(rmd_text)
}


