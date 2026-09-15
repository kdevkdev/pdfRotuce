#' Converts word .docx files to .Rmd files that can be rendered to PDFs using latex
#'
#' @import data.table
#' @param src_docx
#' @param working_folder
#' @param meta_csv
#' @param reference_parsing Default `FALSE`. IF `FALSE`, copy references as is without automated parsing. 'anystyle' or 'grobid' use the respective backends. Can also take the name of a .bib file to use to genrate the bibliography. In that case, in-text citations are expected to follow the '[@citekey1, @citekey2..]' syntax with alphanumeric plus - and _ being valid chars for citekeys,a nd ',' and ';' being valid chars for seperating listed citekeys. In that case, a bibliography in the mansucript file will be disregarded.
#' @param reference_overwrite Only has effect if `reference_parsing` is not `FALSE`. Path to bibtex .bib file that contains manual overrides for references. Matching with entries in file: Recommended (Second priority) is automatically generated citeky, normally 'author'+ 'year', see 'working_path/references.bib' or files in the 'build/tempreftxts_out'. 1st priority is nonstandard bibtex field 'CORRECTION_BN', corresponding to the list item number in the refernces and in-text citation number. If a correction bibfile is provided, 'working_path/references.bib' will be altered compared to 'working_path/refernces_autoparses.bib'. If the file is not found at the specified path, the parent level of the working folder will also be checked (to cover the normal case where intermediate files are placed in the working path 'build' directory, and input files reside at the parent of the 'build' directory)
#' @return
#' @export
#'
#' @examples
#'
#'
markdownify = function(src_docx, doc_folder, working_folder = ".",
                       meta_csv = NULL,
                       rmd_outfile = NULL,
                       xml_outpath = NULL,
                       reference_parsing = F,
                       reference_overwrite = NULL,
                       refparsing_inject = NULL, # format: (@citekey|BIBLIOGRAPHY_NUM)=text, similar to override (and some to augment), but before the parsing stage. Could be used to fuzzy match without doi, or parse bibliography items of reports ...
                       grobid_consolidate = "grobidlv1",# grobidlv1, grobidlv2
                       grobid_consolidate_blacklist = NULL, # only has effect if consolidation_global is provided
                       augment_global = NULL, # pmid, or doi
                       augment_whitelist = NULL, # format: (@citekey|BIBLIOGRAPHY_NUM)=(doi|pmid):id
                       augment_blacklist = NULL,
                       url_parsing = T, doi_parsing = T, guess_refnumbers = T,
                       compat_cell_md_parsing = F){

  # a4: 210, 297, 15 mm left/right margin, 12.5 top/bottom
  #type_width = 180
  #type_height = 272

  fig_capts = tab_capts = c()

  # read file and backup original object
  docx = officer::read_docx(src_docx)
  df = officer::docx_summary(docx,preserve = T,remove_fields = T, detailed = T)


  doc_summar_o = doc_summar  = data.table::as.data.table(df)


  # combine word .docx runs here
  doc_summar = combine_runs(doc_summar)

  # save orginal text after run combination
  doc_summar[, texto := text]


  ################## compile metadata #########################
  metadata_compilation = compile_metadata(doc_summar, doc_folder, meta_csv, reference_parsing)
  metadata = metadata_compilation$metadata

  yaml_preamble = gen_yaml_header(md =metadata, reference_parsing = reference_parsing)

  # remove title page rows from contents
  doc_summar = metadata_compilation$doc_summar_wo_title_page
  # remove [empty] headings
  doc_summar = doc_summar[!(startsWith(tolower(paragraph_stylename), "heading") & trimws(tolower(text)) == "[empty]"),]


  ############### prepare doc summary object ########################
  # doc_sumar will contain: text (original word contents), mrkdwn  (processed markedown), xml_temp (intermediate 'working' xml text ), xml_text (final xml conform text

  # create xml nodes list for JATS xml, special format in that it is embedded into the doc dataframe, needs to have same length as NROW
  # that is, each row as a cell/list entry for corresponding xml nodes
  doc_summar$xml = vector(mode = "list", length = NROW(doc_summar))
  doc_summar$xml_text = ""
  doc_summar$xml_type = character()
  doc_summar$xml_type = NA

  # reset xml filepackaging directory
  path_xml_filepack_dir = paste0(working_folder, "/jats_xml_filepacking")
  if(dir.exists(path_xml_filepack_dir)) unlink(path_xml_filepack_dir, recursive = T)
  dir.create(path_xml_filepack_dir)


  ############# references ###############
  # reference parsing -> call and assign results
  res_parse_references = parse_references(doc_summar = doc_summar, working_folder = working_folder, reference_parsing = reference_parsing,                            reference_overwrite,
                                          refparsing_inject = refparsing_inject,
                                          grobid_consolidate = grobid_consolidate,
                                          grobid_consolidate_blacklist = grobid_consolidate_blacklist,
                                          augment_global = augment_global,
                                          augment_whitelist = augment_whitelist,
                                          augment_blacklist = augment_blacklist,
                                          url_parsing = url_parsing, doi_parsing = doi_parsing, guess_refnumbers = guess_refnumbers)

  doc_summar = res_parse_references$doc_summar


  ############## headings ##############
  # there seems to be a problem with case sensitivity and style names between libreoffice and word, use tolowe (word uses capital, libreoffice not)
  # doc_summar[tolower(paragraph_stylename) == "heading 1" & startsWith(mrkdwn, "*"), mrkdwn := paste0("# ", substr(mrkdwn, 2, nchar(mrkdwn)), " {-}")] # do not put in TOC
  # convert headings to markdown up to level 5
  doc_summar[tolower(paragraph_stylename) == "heading 1", mrkdwn := paste0("# ", mrkdwn)]
  doc_summar[tolower(paragraph_stylename) == "heading 2", mrkdwn := paste0("##  ", mrkdwn)]
  doc_summar[tolower(paragraph_stylename) == "heading 3", mrkdwn := paste0("###  ", mrkdwn)]
  doc_summar[tolower(paragraph_stylename) == "heading 4", mrkdwn := paste0("####  ", mrkdwn)]
  doc_summar[tolower(paragraph_stylename) == "heading 5", mrkdwn := paste0("#####  ", mrkdwn)]
  doc_summar[startsWith(tolower(paragraph_stylename),"heading"), xml_type:= "heading"]

  # parse headings to xml sections and store into doc_summar
  r = gen_xml_sections(doc_summar)
  doc_summar$xml_pretags = r$hxml_pretags
  doc_summar$xml_postags  = r$hxml_postags

    # to debug
  # xml2::read_xml(paste0("<document>", paste0(hxmltags, collapse = ""), "</document>")) |> xml2::as_list()
  # cbind(headi, hxmltags)

  ############## try to detect illegit figure and tab cross-references
  # no longer needed as we do the labeling manually now and do nto enforce prefixes
  # noncolon_crefs = stringr::str_extract_all(doc_summar$mrkdwn, pattern = "\\@ref\\([^:]+?\\)", simplify = F) |> unlist()
  #
  # if(length(noncolon_crefs) > 0 ){
  #   hgl_warn_S("Probable cross-references without 'fig:' or 'tab:' at the start: " %+% paste(collapse = " ", noncolon_crefs))
  #
  # }

  ############## process tables ##############
  cudoc_tabinds = unique(doc_summar[content_type == "table cell"]$table_index)

  tab_counter = 1 # we keep a specific ounter, if ever cti would deviate because e.g. of failure

  # keep track of used chunklabels so that we can generate unique ones (even if there are custom chunk labels)()
  tab_chunk_labels = list()
  # also for figurs
  l_fig_counters= list()
  i_box_counter = 1
  l_tab_counters = list()
  l_labels = list()

  for(cti in cudoc_tabinds){


    # detect empty paragraphs ahead
    max_table_di = max(doc_summar[table_index == cti]$doc_index)
    min_table_di = min(doc_summar[table_index == cti]$doc_index)

    empties = doc_summar[, doc_index > max_table_di & !grepl(x = mrkdwn, pattern = "$\\s?^")]
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


    ct_dat = doc_summar[content_type == "table cell" & table_index == cti]

    # if we need we can take into account the header here (is_header column in doc_summar)
    # aggreagte separated cells by putting a newline in
    # aggregate.fun is needed since multiple paragraphs in a cell are put on seperate rows
    ct_csv = data.table::dcast.data.table(ct_dat, row_id ~ cell_id, value.var = "mrkdwn",
                                          fun.aggregate = \(x){paste(x, collapse = "\n\n", sep = "")})[,-1] # not first



    chunkspec = gen_tabchunk(ct_csv = ct_csv,
                             tab_opts = tab_opts,
                             tab_counter = tab_counter,
                             folder = working_folder,
                             chunklabels = tab_chunk_labels,
                             compat_cell_md_parsing)

    ctab_chunk = chunkspec$chunk
    tab_chunk_labels[length(tab_chunk_labels)+1] =chunkspec$label

    l_labels[[length(l_labels)+1]] = data.table(type = "tab", counter = tab_counter, label = chunkspec$label)

    ctab_xml = gen_xml_table(ct_csv = ct_csv,
                             tab_opts = tab_opts,
                             tab_counter = tab_counter,
                             compat_cell_md_parsing = compat_cell_md_parsing)


    # delete table rows and table options from document structure
    before = doc_summar[doc_index < min_table_di]

    if(!is.null(tab_opts_raw)){

      after = doc_summar[doc_index > max_table_di & doc_index != tab_opts_raw$doc_index]
    } else{
      after = doc_summar[doc_index > max_table_di]
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
  # data.table for inline math formulas
  d_inlinemath = data.table()


  # inline paragraph commands - do not need escaping
  # put protected dollar fo rinline mathc
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", replacement = "========protecteddollar========\\1========protecteddollar========")]


  # not entirely R style, but use global assignment to easily populate list based on regex pattern recognition, starting with inline math
  doc_summar[, xml_temp:= stringr::str_replace_all(string = xml_temp, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]",
                                                   replacement = function(x){

                                                     r = xo = x
                                                     x = x[!is.na(x)] # do no t propagate NAs

                                                     len = length(x)
                                                     latex = stringr::str_extract(string = x, pattern = "\\[\\[mathinline\\$(.*?)\\$mathinline\\]\\]", group = 1)
                                                     d_inlinemath <<- data.table(index = 1:len, latex = latex)


                                                     rnna = paste0("========protectedinlinemath", 1:len, "========")

                                                     # keep NA for return
                                                     r[!is.na(r)] = rnna
                                                     r

                                                   })]

  # put protected at for refs
  doc_summar[, mrkdwn:= gsub(x = mrkdwn, pattern = "\\@ref\\((.+?)\\)", replacement = "\\========protectedat========ref(\\1)")]


  # noindent
  doc_summar[, mrkdwn:= gsub(x = trimws(mrkdwn), pattern = "^\\[\\[noindent\\]\\](.?)", replacement = "\\\\noindent \\1")]
  # JATS XML 1.3: indent likely not supported, but up to display tools

  # look for commands whole paragraph tag commands [[]]
  command_inds =  which(startsWith(trimws(doc_summar$mrkdwn), "[[") & endsWith(trimws(doc_summar$mrkdwn),"]]"))


  # escape dollar etc in suitable paragraphs
  doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn :=rmd_char_escape(mrkdwn)]


  counter_displaymath = 1

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

      # simple command parsing first position contains command
      c_result = ""
      c_result_xml = ""
      c_xml_type = "command"

      if(length(c_command) > 0 & !inherits(c_command, "error")){

        switch(c_command[[1]],
               ibox={
                 #print("ibox detected")

                 #browser()
                 title    =  c_command[['title']]
                 text     =  c_command[['text']]
                 caption  =  c_command[['caption']]
                 label     = c_command[['label']]
                 caption_title = "Box " %+% i_box_counter # hardoced now, possibly use translation string later


                 if(is.null(c_command[['label']])) label = "ibox" %+% i_box_counter

                 htmltitle = ""
                 if(!is.null(title)) htmltitle = "<div class='iboxtitle'>" %+% title %+%"</div>"

                 caption = escape_caption(caption) # escape %, @, $

                 html_output = paste0("\n\n```{=html}\n",htmltitle,
                                      "<div class='ibox'>",
                                      "<span style='display:block;' id='", label, "'></span>\n",
                                       text,
                                      "<p class='caption'>",
                                      caption_title, ": ", caption,
                                      "</p></div>\n",
                                      "```\n\n")


                 #c_result = "\n\n```{=latex}
              c_result = "\n::: {.ibox data-latex=\"[" %+% title %+% "]{" %+% label %+%  "}{" %+% caption_title %+% "}{"%+% caption %+% "}\"}\n" %+% text %+% "\n:::\n" %+% html_output


              c_result_xml = paste("<boxed-text position='anchor' content-type='infobox'>",
                                "<caption>",
                                 "<title>", caption_title, "</title>",
                                "<p>", caption, "</p>",
                                "</caption>",
                                paste0("<p>",text,"</p>"),
                                paste0( "<xref ref-type='aff' rid='", label  ,"'/>"),
                               "</boxed-text>", sep = "\n")

              # also store labels, counter and type  in a list to create a combined data.table later
              l_labels[[length(l_labels)+1]] = data.table(type = "box", counter = i_box_counter, label = label)

              # increment counter for boxes
              i_box_counter = i_box_counter+1


                 },
               figure={
                 #print("figure detected")
                 # detect optional 'type' argument (fig is default)
                 fig_type = if(!is.null(c_command[['type']])) fig_type = c_command[['type']]
                               else fig_type = "fig"

                 # do not use this as this is reserved by tables
                 if(fig_type == "tab") hgl_error("do not use 'tab' as figure type, this is reserved for tales")
                 if(fig_type == "ibox") hgl_error("do not use 'box' as figure type, this is reserved for boxse")

                 # increment counter for the current type
                 l_fig_counters[[fig_type]] = sum(l_fig_counters[[fig_type]], 1, na.rm = T)

                 # also store labels, counter and type  in a list to create a combined data.table later
                 l_labels[[length(l_labels)+1]] = data.table(type = fig_type, counter = l_fig_counters[[fig_type]], label = c_command[['label']])


                 c_result = gen_figblock(fig_opts = c_command, fig_counter = l_fig_counters[[fig_type]], fig_type = fig_type)
                 c_xml_type = "figure"
                 c_result_xml = gen_xml_figure(fig_opts = c_command, fig_counter = l_fig_counters[[fig_type]],
                                               xml_filepack_dir = path_xml_filepack_dir, base_folder = doc_folder,
                                               fig_type = fig_type)
               },
               math={
                 #print("Math detected")
                 formula = c_command['form']
                 c_result = paste0("$$", trimws(formula), "$$")
                 c_result_xml = gen_xml_displaymath(latex = trimws(formula), counter_displaymath)
                 counter_displaymath = counter_displaymath+1
                 c_xml_type = "display_math"

               },
               quote={
                 #print("quote detected")

                 text = rmd_char_descape(trimws(c_command['text'])) # needs double escape to pass through rmarkdown preprocessing
                 source = if(!is.null(c_command[['src']]) && is.character(c_command[['src']])) trimws(c_command['src']) else ""

                 # determine spacing based on whether the last pargraph was also a quote
                 if(cii > 1 && command_list[[cii-1]]$command[[1]] == "quote" && command_list[[cii-1]]$index  == c_comi -1){
                   vskip = "\\vspace{-3mm}"
                 } else {
                   vskip = "\\vspace{-0mm}"
                 }

# old version
#                  c_result = vskip %+% "
# ::: {.displayquote data-latex=\"{  }\"}
# ::: {.enquote data-latex=\"{\\textit{" %+% text %+% "}} " %+% source %+% "\"}
# \\phantom{}
# :::
# :::
# \\vspace{-5.5mm}
# "
               c_result = "\n```{=latex}\n" %+% vskip %+% "
\\begin{displayquote}{  }
\\begin{enquote}{\\textit{" %+% text %+%"} " %+% source %+% "}
\\phantom{}
\\end{enquote}
\\end{displayquote}
\\vspace{-1mm}
\\
```\n

```{=html}\n
<div class='bquotecontainer'>
<blockquote class='bquote'>
"%+% text %+%" <span class='source'>"%+% source %+%"</span>
</blockquote>
</div>
```\n";

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
                start_singlecol={
                  #print("Math detected")
                  c_result = paste("\n```{=latex}",
                                     "\\end{multicols}", sep = "\n",
                                     #"\\raggedcolumns",
                                     "```\n")


                  },
                end_singlecol={
                  #print("Math detected")
                  c_result = paste("\n```{=latex}",
                                   "\\begin{multicols}{2}", sep = "\n",
                                   #"\\raggedcolumns",
                                   "```\n")

                },

               {
                 # default
                 browser()
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


  ######### Cross references ################
  d_labels = rbindlist(l = l_labels)

  rplcmntfn = function(matches){

    # defalt: no replacement
    replacements = rep("", length(matches))
    # for now only handle those that we already have in our labels (i.e. only figures not tables)
    for(k in 1:NROW(matches)){

      cmatch = matches[k]
      # get label from entire match
      clabel = stringr::str_match(cmatch, pattern = "\\\\========protectedat========ref\\(([a-zA-Z0-9:]{1,})\\)")[1,2]

      # look for label
      hits = d_labels[ label == clabel]

      # we get the original string index of 'x' in fig_labels as name
      # and the index s value
      if(NROW(hits) ==0){
        hgl_warn("Cross-referencing, no item with this label found: "%+% clabel  )
        replacement = ""
      } else if (NROW(hits) > 1){
        hgl_error("Cross-referencing, cross reference matching multiple target labels: "%+% clabel  )
      } else{

        typestring = hits[, type]
        if(hits[, type] == "fig") typestring  = "Figure"

        replacements[k] = paste0("[",hits[, counter],"](#",hits[, label],")")
      }

    }
    return(replacements)
  }

  # generate intext citations
  doc_summar$mrkdwn = stringr::str_replace_all(string = doc_summar$mrkdwn, pattern = "\\\\========protectedat========ref\\([a-zA-Z0-9:]{1,}\\)", replacement = rplcmntfn)
  doc_summar$xml_temp = stringr::str_replace_all(string = doc_summar$xml_temp, pattern = "\\\\========protectedat========ref\\([a-zA-Z0-9:]{1,}\\)", replacement = rplcmntfn)

  # parse to xml  nodes that have not already been prossesd
  doc_summar[is.na(xml_type), `:=`(xml_type = "paragraph", xml_text = gen_xml_paragraphs(xml_temp,d_inlinemath ))]


  ############ statements and declarations ############
  yaml_statements = ''
  l_statements = list()

  # generate author contributions
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

    #rmd_statements = "\n\n# Declarations\n\n"
    statement_id = 1
    for(cn in names(metadata$statements)){

      cle = list()
      cstat <- metadata$statements[[cn]]
      # todo potentially parse markdown?
      cle$text = cstat
      cle$title = cn
      cle$position = statement_id

      statement_id <- statement_id+1

      # checkstring
      chkstr = trimws(tolower(cn))
      # check for words author and contributions in right order and some limited distance from each other - to detect later if it still has to be added
      if(stringr::str_detect(string = chkstr, pattern = "author.{1,5}contribution")){
         consumed_indiv_author_contribs = TRUE

         cstat = paste0(cstat, "\n", indiv_author_contribs) # att this to the setion

      }
      l_statements[[length(l_statements)+1]] = cle
    }

  }


  # no manual author  contributions found in statements and thos author contribution not yet put into rmd -> put specified roles
  if(!consumed_indiv_author_contribs && !is.null(indiv_author_contribs) && !is.na(indiv_author_contribs) && is.character(indiv_author_contribs) && indiv_author_contribs != "") {

    #yaml_statements = yaml_statements %+% "\\subsection{Author contributions}\n\n" %+% indiv_author_contribs %+% "\n\n"
    l_statements[[length(l_statements)+1]] = list(title = "Author contributions" , text = indiv_author_contribs, position = length(l_statements)+1)
  }


  #yaml_statements = yaml::as.yaml(list(statements = yaml_statements))

  yaml_statements =yaml::as.yaml(list(statements = l_statements))

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
  yaml_orcinds = ""
  if(!is.null(author_orcinds) && length(author_orcinds) > 0 && any(author_orcinds)){

    l_orcids  <- metadata$authors[author_orcinds] |> lapply(\(x){
      list(name = x$name,
           orcid = x$orcid,
           position = x$position)})


    yaml_orcinds = yaml::as.yaml(list(orcids = l_orcids))
  }

  rmd_text = c(yaml_preamble, chunk_setup, fig_capts, tab_capts,
               outmrkdwn,
               "\n",
               "\n",
               "---",
               yaml_statements,
               "\n",
               yaml_orcinds,
               "\n",
               res_parse_references$yaml_references, # YAML to store refs in a variable
               "---\n"
               )

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
    xml_statements = NULL
    if(length(xml_metadata$statements) > 0){
      xml_statements = gen_xml_statements(xml_metadata$statements)
    }

    xml_text = paste0(gen_xml_file(doc_summar, article_type = xml_metadata$article_type,
                                   xml_meta  = xml_front, xml_references = xml_refs,
                                   d_xmlintext_cites = res_parse_references$d_xmlintext_cites, xml_statements = xml_statements))


    if(file.exists(paste0(path_xml_filepack_dir, "/document.xml"))) hgl_error(paste0("Error in writing out JATS xml:  document.xml already exists in '", path_xml_filepack_dir,"'"))
    else write(xml_text, file = paste0(path_xml_filepack_dir, "/document.xml")) # overwrites if existing

    # copy all to output dir - trick - base R does not allow copying of *contents* of directories - copy as single directory to parent folder, and then rename
    file.copy(from = list.files(path = paste0(path_xml_filepack_dir), full.names = T), to = paste0(xml_outpath), recursive = T)

    # attempt to create zip of contents
    zip::zip(zipfile = paste0(xml_outpath, "/document.zip"), files = list.files(xml_outpath, full.names= TRUE), mode = "cherry-pick")
  }

  return(rmd_text)
}
