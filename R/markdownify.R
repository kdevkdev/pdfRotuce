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
  type_width = 180

  type_height = 272

  fig_capts = tab_capts = c()


  # read file and backup object
  docx = officer::read_docx(src_docx)
  df = officer::docx_summary(docx,preserve = T,remove_fields = T, detailed = T)


  doc_summar_o = doc_summar  = data.table::as.data.table(df)

  # combine runs here. As of now, detailed = True unfortately seems to remove table contents
  doc_summar = combine_runs(doc_summar)


  # copy over stylename from to paragraph stylename (same as if detailed = T)
  #doc_summar[,paragraph_stylename := style_name]

  #doc_summar[is.na(paragraph_stylename), paragraph_stylename := ""]
  doc_summar[, texto := text]


  # get all l1 headings
  l1_inds = which(tolower(doc_summar$paragraph_stylename) == "heading 1")

  stopifnot("No level 1 heading style found" = length(l1_inds)>0 )

  title_page_inds = l1_inds[1]:(l1_inds[2]-1)

  # find and parse titele page - first heading 1 up to second heading 1
  parsed_meta = parse_title_page(doc_summar[title_page_inds,])

  # predefined metadata & metadata for
  predef_meta  = list()
  periodical_meta = list()

  # get article specific metadata from csv
  if(file.exists(paste0(doc_folder, "/metadata0_periodical.csv"))){

    # load periodical metadata
    pd_mp = read.csv(file = paste0(doc_folder, "/metadata0_periodical.csv"), header = F, fileEncoding = "UTF-8")
    pmvals = pd_mp[[2]]
    names(pmvals) = pd_mp[[1]]

    periodical_meta = as.list(pmvals)
  }


  # get article specific metadata from csv
  if(!is.null(meta_csv) & file.exists(meta_csv)){

    pd_tab = read.csv(file = meta_csv, header = F, fileEncoding = "UTF-8")

    # values to a vector and name to vector names
    tvals = pd_tab[[2]]
    names(tvals) = pd_tab[[1]]

    checkvec = c("volume", "issue", "string_volumeissue",
                 "doi", "has_abstract", "article_type", "author_shortname", "string_corresponding", "string_contact",
                 "string_responsibleeditor", "string_articleihstory", "string_keywords",
                 "string_spanish_keywords", "string_declarations_title","string_keywords_spanish",
                 "string_multilang_abstract_title", "string_bibliography_title")


    # whitelist of values to copy
    for(cn in checkvec){
      predef_meta[[cn]] = tvals[cn]
    }

    # copy over provided values, uppercase for article typer
    # if(is.element("volume", names(tvals)))                    predef_meta$volume                    = tvals["volume"]                         else hgl_warn("'volume' missing in meta csv")
    # if(is.element("issue",  names(tvals)))                    predef_meta$issue                     = tvals["issue"]                          else hgl_warn("'issue' missing in meta csv")
    # if(is.element("string_volumeissue",  names(tvals)))       predef_meta$string_volumeissue        = tvals["string_volumeissue"]             else hgl_warn("'string_volumeissue' missing in meta csv")
    # if(is.element("doi", names(tvals)))                       predef_meta$doi                       = tvals["doi"]                            else hgl_warn("'doi' missing in meta csv")
    # if(is.element("has_abstract", names(tvals)))              predef_meta$has_abstract              = tvals["has_abstract"]                   else hgl_warn("'has_abstrat' missing in meta csv")

    # if(is.element("author_shortname", names(tvals)))          predef_meta$author_shortname          = tvals["author_shortname"]               else hgl_warn("'author_shortname' missing in meta csv")
    # if(is.element("string_corresponding", names(tvals)))      predef_meta$string_corresponding      = tvals["string_corresponding"]           else hgl_warn("'string_corresponding' missing in meta csv")
    # if(is.element("string_contact", names(tvals)))            predef_meta$string_contact            = tvals["string_contact"]                 else hgl_warn("'string_contact missing in meta csv")
    # if(is.element("string_responsibleeditor", names(tvals)))  predef_meta$string_responsibleeditor  = tvals["string_responsibleeditor"]       else hgl_warn("'string_responsibleeditor' missing in meta csv")
    # if(is.element("string_articlehistory", names(tvals)))     predef_meta$string_articlehistory     = tvals["string_articlehistory"]          else hgl_warn("'string_articlehistory' missing in meta csv")
    # if(is.element("string_keywords", names(tvals)))           predef_meta$string_keywords           = tvals["string_keywords"]                else hgl_warn("'string_keywords' missing in meta csv")
    # if(is.element("string_articlehistory", names(tvals)))     predef_meta$string_articlehistory     = tvals["string_articlehistory"]          else hgl_warn("'string_articlehistory' missing in meta csv")
    # if(is.element("string_keywords", names(tvals)))           predef_meta$string_keywords           = tvals["string_keywords"]                else hgl_warn("'string_keywords' missing in meta csv")


    # some more processing
    if(is.element("article_type", names(tvals)))              predef_meta$article_type              = toupper(tvals["article_type"])          else hgl_warn("'article_type' missing in meta csv")
    # split articledates by ';'
    if(is.element("articledates", names(tvals)))             predef_meta$articledates = stringr::str_split(simplify= T, pattern = ";", string =tvals["articledates"]) |> as.vector() |> trimws() else hgl_warn("'articledates' missing in meta csv")

    if(startsWith(predef_meta$doi,"https://doi.org/") || startsWith(predef_meta$doi,"http://doi.org/")){

      # convert form url to doi if needed
      # for url start
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "^(https?://)?doi\\.org/", replacement = "")

      # for url end
      predef_meta$doi = stringr::str_replace(string = predef_meta$doi, pattern = "/$", replacement = "")
    }
  }
  hardcoded_meta = gen_hardcoded_meta(reference_parsing = reference_parsing)


  # combine metadata specified in parsed document with metadata provided in CSV
  metadata = c(predef_meta, parsed_meta, periodical_meta, gen_hardcoded_meta(reference_parsing = reference_parsing))

  # also pack keywords from metadata$atrributes into the respective abstracts

  # find all attrributes starting with keywords
  kw_names = stringr::str_extract_all(string = names(metadata$attributes), pattern = "^keywords.*") |> unlist()

  for(ckwn in kw_names){

    ckws = metadata$attributes[[ckwn]]

    # detect langague suffix after '_', or mainlangaugeS
    if(ckwn == "keywords"){
      clan = 'mainlang'
    } else{
      clan = stringr::str_replace_all(string = ckwn, pattern = "keywords_", replacement = "")
    }

    # check if present
    if(!is.null(metadata$abstracts[[clan]])){

      metadata$abstracts[[clan]]$keywords = ckws

    } else{
      # if ever a template requires sidelang keywords without abstracts then att the sidelang abstract here and keywords after.
      hgl_warn("'" %+% ckwn %+% " provided but no abstract for language '" %+% clan %+% "' present.")
    }
  }



  yaml_preamble = gen_yaml_header(md =metadata, reference_parsing = reference_parsing)


  # delete  title page from document
  doc_summar = doc_summar[-title_page_inds,]

  # remove [empty] headings
  doc_summar = doc_summar[!(startsWith(tolower(paragraph_stylename), "heading") & trimws(tolower(text)) == "[empty]"),]



  ## @@@@ doc_sumar will contain: text (original word contents), mrkdwn  (processed markedown), xml_temp (intermediate 'working' xml text ), xml_text (final xml conform text

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

  ############## process tables ##############
  cudoc_tabinds = unique(doc_summar[content_type == "table cell"]$table_index)

  tab_counter = 1 # we keep a specific ounter, if ever cti would deviate because e.g. of failure

  # keep track of used chunklabels so that we can generate unique ones (even if there are custom chunk labels)()
  tab_chunk_labels = list()

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
    ct_csv = data.table::dcast.data.table(ct_dat, row_id ~ cell_id, value.var = "mrkdwn",
                                          fun.aggregate = \(x){paste(x, collapse = "\n", sep = "")})[,-1] # not first



    chunkspec = gen_tabchunk(ct_csv = ct_csv,
                             tab_opts = tab_opts,
                             tab_counter = tab_counter,
                             folder = working_folder,
                             chunklabels = tab_chunk_labels,
                             compat_cell_md_parsing)

    ctab_chunk = chunkspec$chunk
    tab_chunk_labels[length(tab_chunk_labels)+1] =chunkspec$label

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


  # noindent
  doc_summar[, mrkdwn:= gsub(x = trimws(mrkdwn), pattern = "^\\[\\[noindent\\]\\](.?)", replacement = "\\\\noindent \\1")]
  # JATS XML 1.3: indent likely not supported, but up to display tools

  # look for commands whole para tag commands [[]]
  command_inds =  which(startsWith(trimws(doc_summar$mrkdwn), "[[") & endsWith(trimws(doc_summar$mrkdwn),"]]"))


  # escape dollar etc in suitable paragraphs
  doc_summar[!startsWith(trimws(mrkdwn), "[[") & !startsWith(trimws(mrkdwn), "```{") ,mrkdwn :=rmd_char_escape(mrkdwn)]

  fig_counter = 1
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
  yaml_statements = ''

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
    for(cn in names(metadata$statements)){

      cstat <- metadata$statements[[cn]]
      # todo potentially parse markdown?

      #rmd_statements = rmd_statements %+% "## " %+%cn %+% "\n" %+% "\\noindent " %+% cstat
      yaml_statements = yaml_statements %+% "\\subsection{" %+%cn %+% "}\n" %+% "\\noindent " %+% cstat

      # checkstring
      chkstr = trimws(tolower(cn))
      # check for words author and contributions in right order and some limited distance from each other - to detect later if it still has to be added
      if(stringr::str_detect(string = chkstr, pattern = "author.{1,5}contribution")){
         consumed_indiv_author_contribs = TRUE

         yaml_statements = paste0(yaml_statements, "\n", indiv_author_contribs) # att this to the setion

      }
      yaml_statements = yaml_statements %+% "\n\n"
    }

  }

  # author speciffic contributions not yet put into rmd
  if(!consumed_indiv_author_contribs) {

    yaml_statements = yaml_statements %+% "\\subsection{Author contributions}\n\n" %+% indiv_author_contribs %+% "\n\n"
  }

  yaml_statements = yaml::as.yaml(list(statements = yaml_statements))




  ############ other language abstracts ############
  yaml_multilang_abstracts = ""
  if(any(names(metadata$abstracts)!="mainlang")){

    # figure out what additional languages there are (remove mainlang, usually english)
    side_langs = setdiff(names(metadata$abstracts),"mainlang")

    side_langs_abstracts = metadata$abstracts[side_langs]




    yaml_multilang_abstracts = "\n\\vspace{2mm}"
    first <- TRUE

    for(can in names(side_langs_abstracts)){

      # all abstracgt parts
      ca_parts = side_langs_abstracts[[can]]$parts

      # get article title in current language to add to abstract
      article_lang_title = side_langs_abstracts[[can]]$title


      cab_tit = toupper(switch(can, "", "es" = "RESUMEN", "som" = "CINWAAN")) # Resumén. Generate abstract term for each language, default empty

      if(first){ # but horizontal rule
        #yaml_multilang_abstracts = yaml_multilang_abstracts %+% "{\\noindent\\color{jchsheadercolor}\\rule{\\textwidth}{1.6pt}}\n"
        yaml_multilang_abstracts = yaml_multilang_abstracts %+% "\\begin{tcolorbox}[colframe=themecolor2, colback=themecolor2, sharp corners,boxsep=4mm,top=3.5mm,left=3.8mm,right=2.0mm]\\sffamily\n"
        first <- FALSE
      }



      if(!is.null(article_lang_title) && !is.na(article_lang_title) && article_lang_title != ""){

        yaml_multilang_abstracts = yaml_multilang_abstracts %+% "{\\bfseries \\vskip 0mm" %+% article_lang_title %+% "}\\vskip 3mm"
      }
      yaml_multilang_abstracts = yaml_multilang_abstracts %+% "{\\raggedright\\bfseries " %+% cab_tit %+% "}\\vskip 1mm"


      # go through paragraphs/parts
      for(cp in ca_parts){

        yaml_multilang_abstracts = yaml_multilang_abstracts %+% "{\\bfseries " %+% rmd_char_escape(cp$title) %+% "} " %+% rmd_char_escape(cp$text) %+% "\n\n"
      }
    }

    csidelang = gsub("abstract_", "", x = can)

    if(!is.null(metadata$attributes[[paste0("keywords_", csidelang)]])){

      keywordstitle = ""
      if(csidelang == "es"){
        keywordtitle = "{\\raggedright\\bfseries Palabras clave: }"
      }
      yaml_multilang_abstracts = yaml_multilang_abstracts %+% "\\vskip 3mm" %+% keywordtitle
      yaml_multilang_abstracts = yaml_multilang_abstracts %+% metadata$attributes[[paste0("keywords_", csidelang)]]
    }

    yaml_multilang_abstracts = yaml_multilang_abstracts %+% "\\end{tcolorbox}\\vspace{3mm}\n\n"

  }


  yaml_multilang_abstracts = yaml::as.yaml(list(multilang_abstracts = yaml_multilang_abstracts))

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

    td <- metadata$authors[author_orcinds] |> rbindlist(fill = TRUE)

    plural = ""
    if(NROW(td)> 1){
      plural = "s"
    }

#    yaml_orcinds =                 "## ORCID" %+% plural %+%"\n\n"
    #yaml_orcinds = yaml_orcinds %+% "```{=latex}\n{\\noindent\\raggedright\n"

    yaml_orcinds = yaml_orcinds %+% paste0( paste0(td$name , " \\orcidaffil{", td$orcid, "} \\href{https://orcid.org/",
                                                 td$orcid, "}{",
                                                 td$orcid,"}"), collapse = "\n\n")

    yaml_orcinds = yaml_orcinds

    yaml_orcinds = yaml::as.yaml(list(orcids = yaml_orcinds))
    #yaml_orcinds = yaml_orcinds %+% "\n```"
  }

  # partials output - enables bigger flexibilty in combination with the latex tempalte
  #unlink(working_folder %+% "/partials", recursive = T)
  #dir.create(working_folder %+% "/partials")
  #write(x = rmd_multilang_abstracts, file = working_folder %+% "/partials/multilang_abstracts.tex")
  #write(x = rmd_statements, file = working_folder %+% "/partials/rmd_statements.tex")
  #write(x = res_parse_references$rmd_references, file = working_folder %+% "/partials/rmd_references.tex")


  rmd_text = c(yaml_preamble, chunk_setup, fig_capts, tab_capts,
               #"```{=latex}", # maybe sometimes a restart of the column environemnt might be helpful_
               #"\\begin{multicols}{2}",
               #"\\raggedcolumns",
               #"\\interlinepenalty=10000",
               #"```",
               outmrkdwn,
               "\n",
               "\n",
               "---",
               yaml_statements,
               "\n",
               yaml_orcinds,
               "\n",
               yaml_multilang_abstracts,
               "\n",
               res_parse_references$yaml_references, # YAML to store refs in a variable
               "---\n"
               #rmd_statements,
               #rmd_orcinds,
               #"```{=latex}",
               #"\\end{multicols}",
               #"```",
               #"\\raggedbottom",
               #rmd_multilang_abstracts,
               #"```{=latex}",
               #"\\begin{multicols}{2}",
               #"\\raggedcolumns",
               #"\\interlinepenalty=10000",
               #"```",

               #"```{=latex}",
               #"\\end{multicols}",
               #"```"
               )

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
