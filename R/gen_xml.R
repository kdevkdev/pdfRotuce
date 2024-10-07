gen_xml_header = function(metadata){


  processing_meta = paste0("<processing-meta
                  tagset-family='jats'
                  base-tagset='publishing'
                  mathml-version='2.0'
                  table-model='xhtml'/>")

  # front
  journal_meta = paste0("<journal-meta>",
                        "<issn>",metadata$issn,"</issn>",
                        "<publisher>", #PMC  ids for publisher and pubmed shorthand need likely to be added backupon indexation
                        "<publisher-name>",metadata$publisher,"</publisher-name>",
                        "</publisher>",
                        "</journal-meta>")


  author_list = list()
browser()
  for(ca in metadata$authors){
       authors_list[length(authors_list)+1] =
         paste0("<contrib contrib-type='author'>",
                  "<name>",
                    "<surname>Sullivan</surname>",
                    "<given-names>Amy D.</given-names>",
                  "</name>",
                "</contrib>")
  }

  article_meta = paste0("<article-meta>",
                          "<article-id pub-id-type='publisher-id'>181325198</article-id>", # needed - Ask miguel?
                          "<article-id pub-id-type='doi'>",metadata$doi,"</article-id>",
                          "<article-version vocab='JAV' vocab-identifier='http://www.niso.org/publications/rp/RP-8-2008.pdf'
                            vocab-term='Version of Record' article-version-type='VoR'>Version of Record</article-version>", # ask miguel

                          # ask miguel for how they plan around this?
                          "<article-categories>
                          <subj-group>
                            <subject>Physical Sciences</subject>
                            <subj-group>
                              <subject>Applied Mathematics</subject>
                            </subj-group>
                          </subj-group>
                          <subj-group>
                            <subject>Biological Sciences</subject>
                            <subj-group>
                              <subject>Genetics</subject>
                            </subj-group>
                          </subj-group>
                          </article-categories>",

                          # article title
                          "<title-group>
                            <article-title>", metadata$title, "</article-title>
                          </title-group>",

                          # authors - ask miguel if CREDIT statment will be used?
                          "<contrib-group content-type='authors'>
                            <contrib contrib-type='author'>
                            <name>
                            <surname>Sullivan</surname>
                            <given-names>Amy D.</given-names>
                            </name>
                            <role degree-contribution='lead'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writer &#8212; original draft'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-original-draft/'
                          >Wrote the original draft</role>
                            <role degree-contribution='lead'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writing &#8212; Review &amp; Editing'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-review-editing/'>Led the team for the edit review and did the rewrite</role>
                            <xref ref-type='author-notes' rid='FN150'>&#x002A;</xref>
                            <xref ref-type='aff' rid='aff-1'/>
                            </contrib>
                            <contrib contrib-type='author'>
                            <name>
                            <surname>Wigginton</surname>
                            <given-names>Janis</given-names>
                            </name>
                            <role degree-contribution='lead'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writer &#8212; original draft'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-original-draft/'
                          >Co-wrote Discussion Section for original draft</role>
                            <xref ref-type='aff' rid='aff-1'/>
                            </contrib>
                            <contrib contrib-type='author'>
                            <name>
                            <surname>Kirschner</surname>
                            <given-names>Denise</given-names>
                            </name>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writer &#8212; original draft'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-original-draft/'
                          >Co-wrote Model Section for original draft</role>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writing &#8212; Review &amp; Editing'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-review-editing/'>Writing &#8212; Review &amp; Editing</role>
                            <xref ref-type='corresp' rid='FN151'>&#x2020;</xref>
                            <xref ref-type='aff' rid='aff-1'/>
                            </contrib>
                            </contrib-group>
                            <contrib-group content-type='software'>
                            <contrib contrib-type='software'>
                            <name>
                            <surname>Krosky</surname>
                            <given-names>Mark</given-names>
                            </name>
                            <role degree-contribution='equal'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Software'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/software/'
                          >Programming and technical assistance</role>
                            </contrib>
                            <contrib contrib-type='software'>
                            <name>
                            <surname>Koelle</surname>
                            <given-names>Katia</given-names>
                            </name>
                            <role degree-contribution='equal'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Software'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/software/'
                          >Programming and technical assistance</role>
                            </contrib>
                            <contrib contrib-type='software'>
                            <name>
                            <surname>Chung</surname>
                            <given-names>Kevin</given-names>
                            </name>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Software'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/software/'
                          >Programming and technical assistance</role>
                            </contrib>
                            </contrib-group>
                            <contrib-group content-type='reviewers'>
                            <contrib contrib-type='reviewer'>
                            <name>
                            <surname>DiRita</surname>
                            <given-names>V. J.</given-names>
                            <prefix>Dr.</prefix>
                            </name>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writing &#8212; Review &amp; Editing'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-review-editing/'
                          >Helpful comments and discussions</role>
                            </contrib>
                            <contrib contrib-type='reviewer'>
                            <name>
                            <surname>Kazanjian</surname>
                            <given-names>P.</given-names>
                            <prefix>Dr.</prefix>
                            </name>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writing &#8212; Review &amp; Editing'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-review-editing/'
                          >Helpful comments and discussions</role>
                            </contrib>
                            <contrib contrib-type='reviewer'>
                            <name>
                            <surname>Blower</surname>
                            <given-names>S. M.</given-names>
                            <prefix>Dr.</prefix>
                            </name>
                            <role degree-contribution='supporting'
                          vocab='CRediT'
                          vocab-identifier='http://credit.niso.org'
                          vocab-term='Writing &#8212; Review &amp; Editing'
                          vocab-term-identifier=
                            'http://credit.niso.org/contributor-roles/writing-review-editing/'
                          >Helpful comments and discussions</role>
                            </contrib>
                          </contrib-group>",
                          "<aff id='aff-1'>Department of Microbiology and Immunology, University
                        of Michigan Medical School, Ann Arbor, MI 48109-0620</aff>
                          <author-notes>
                          <fn id='FN150' fn-type='present-address'>
                          <p>&#x002A; Present address: Centers for Disease Control and Prevention Epidemiology Program Office, State Branch Oregon Health Division, 800 NE Oregon Street, Suite 772, Portland, OR 97232.</p>
                          </fn>
                          <corresp id='FN151'>&#x2020; To whom reprint requests should be addressed. E-mail: <email>kirschne@umich.edu</email>.</corresp>
                          <fn fn-type='com'>
                          <p>Communicated by Avner Friedman, University of
                        Minnesota, Minneapolis, MN</p>
                          </fn>
                          </author-notes>
                          <pub-date date-type='pub' publication-format='print'
                        iso-8601-date='2001-08-28'>
                          <day>28</day>
                          <month>8</month>
                          <year>2001</year>
                          </pub-date>
                          <pub-date date-type='pub' publication-format='electronic'
                        iso-8601-date='2001-08-21'>
                          <day>21</day>
                          <month>8</month>
                          <year>2001</year>
                          </pub-date>
                          <volume>98</volume>
                          <issue>18</issue>
                          <fpage>10214</fpage>
                          <lpage>10219</lpage>
                          <history>
                          <date date-type='received' iso-8601-date='2000-05-30'>
                          <day>30</day>
                          <month>5</month>
                          <year>2000</year>
                          </date>
                          <date date-type='accepted' iso-8601-date='2001-06-27'>
                          <day>27</day>
                          <month>6</month>
                          <year>2001</year>
                          </date>
                          </history>
                          <permissions>
                          <copyright-statement>Copyright &#x00A9; 2001, The National Academy of Sciences</copyright-statement>
                          <copyright-year>2001</copyright-year>
                          </permissions>
                          <abstract>
                          <p>We explore the impact of a host genetic factor on heterosexual HIV epidemics by using a deterministic mathematical model. A protective allele unequally distributed across populations is exemplified in our models by the 32-bp deletion in the host-cell chemokine receptor CCR5, CCR5&#x0394;32. Individuals homozygous for CCR5&#x0394;32 are protected against HIV infection whereas those heterozygous for CCR5&#x0394;32 have lower pre-AIDS viral loads and delayed progression to AIDS. CCR5&#x0394;32 may limit HIV spread by decreasing the probability of both risk of infection and infectiousness. In this work, we characterize epidemic HIV within three dynamic subpopulations: CCR5&#x002F;CCR5 (homozygous, wild type), CCR5&#x002F;CCR5&#x0394;32 (heterozygous), and CCR5&#x0394;32&#x002F;CCR5&#x0394;32 (homozygous, mutant). Our results indicate that prevalence of HIV&#x002F;AIDS is greater in populations lacking the CCR5&#x0394;32 alleles (homozygous wild types only) as compared with populations that include people heterozygous or homozygous for CCR5&#x0394;32. Also, we show that HIV can provide selective pressure for CCR5&#x0394;32, increasing the frequency of this allele.</p>
                          </abstract>
                          </article-meta>")


  header = paste(processing_meta, "<front>", journal_meta, article_meta, "</front>", sep = "\n")
}
gen_xml_displaymath = function(latex, counter = NULL){

  processing_meta = paste0("<disp-formula id='E1'>",
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
        xml2::xml_ns_strip(xml_nodes)

        # to develop https://xsltfiddle-beta.liberty-development.net/
        xlst =  xml2::read_xml(system.file("commonmark_xml_to_jats_xml.xml", package="pdfRotuce"))

        # define xslt transformation
        xml_jats = xslt::xml_xslt(xml_nodes,xlst)

        if(is.na(xml_jats)){
          stop("commonmark to JATS xslt transformation failed")
        }

        print(i)
        print(xml_jats |> as.character())

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
gen_xml_file = function(doc_summar, article_type){

  header = paste("<?xml version='1.0' encoding='UTF-8'?>",
                  "<!DOCTYPE article PUBLIC",
                  "'-//NLM//DTD JATS (Z39.96) Journal Publishing DTD v1.3 20210610//EN'",
                  "'JATS-journalpublishing1-3.dtd'>",
                  "<article article-type='",article_type,
  "dtd-version='1.3'",
  "xml:lang='en'",
  "xmlns:mml='http://www.w3.org/1998/Math/MathML'",
  "xmlns:xlink='http://www.w3.org/1999/xlink'",
  "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' >", sep = "\n")


  xmltext_rows = paste(doc_summar$xml_secopen, doc_summar$xml_text, doc_summar$xml_secend)

  mid = paste0(xmltext_rows[nchar(xmltext_rows) > 0], collapse = "\n")

  end = "</article>"

  res = paste0(header, mid, end)

}

