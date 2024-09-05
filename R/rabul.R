#library(rly)

parse_cellmd = function(md){

  rt = parser$parse(md, lexer)[[1]]

  if(is.null(rt)) rt = ""

  rt
}

gen_colspecs = function(ncol, scheme = "twcol", xltabular = T, colwidths= NULL, colaligns= NULL, fullgrid = F){

  # scheme not yet used
  nfillcols = ncol - 1

  if(scheme == "twocol")
  {
    fillspecs = paste0(collapse = "", rep("X[c]", times = nfillcols))
    firstcol = "X[l]"
  } else if(scheme == "onecol"){
    fillspecs = paste0(collapse = "", rep("X[c]", times = nfillcols))
    firstcol = "X"
  }
  else{
    stop("gen_colspecs: No scheme provided")
  }

  if(xltabular ==T){

    if(!is.null(colaligns)){

      stopifnot("Number of provided colaligns entries must be the same as number of table columns" = ncol == length(colaligns))
      stopifnot("All colaligns entries must be either be 'l', 'c', or 'r" = all(colaligns %in% c("l", "c", "r")))

      aligns = ifelse(colaligns == "r", "\\raggedleft\\arraybackslash", ifelse(colaligns == "c", "\\centering\\arraybackslash", "\\raggedright\\arraybackslash"))

    }
    else{
      aligns = rep("",ncol)
    }


    colsepc = ""

    colvline = ""
    if(fullgrid == TRUE){
      colvline = "|"
    }

    if(!is.null(colwidths)){
      # calcuate colwith

      stopifnot("Number of provided colwidths entries must be the same as number of table columns" = ncol == length(colwidths))

      colwidths = as.numeric(colwidths)
      stopifnot("All colwiths entries must be numeric" = all(!is.na(colwidths)))

      rws = ncol * colwidths / sum(colwidths)


      colspec = paste0(">{", aligns,"\\hsize=", rws, "\\hsize\\linewidth=\\hsize}X") |> paste0(collapse = colvline)

    }else
      colspec = paste0(rep("X", ncol), collapse = colvline)

    colspec = paste0(colvline, colspec, colvline) # add outer left right borders
    return(colspec)
  }
  else
    return(paste0("|", firstcol, fillspecs, "|"))
}

#' Create latex code for tables
#'
#' @param d
#' @param linesep
#' @param wide
#' @param caption
#' @param label
#' @param long
#' @param xltabular
#' @param colwidths
#' @param colaligns
#' @param fullgrid
#'
#' @return
#' @export
#'
#' @examples
rabulify = function(d, linesep = "\newline", mode  = "twocolumn" , caption = NULL, footnote = NULL, label = NULL,  long = F, xltabular = T, colwidths = NULL, colaligns = NULL, fullgrid = FALSE){
  l_innerspecs = list()
  l_outerspecs = list()


  # detect header rows
  header_inds = which(startsWith(d[[1]], "#"))

  # manual colbreaks inds
  colbreak_inds  = which(apply(d, 1, \(r) {grep("\\[\\[colbreak\\]\\]", r) |> sum()}) > 0 )

  # remove those '#'
  d[[1]] = gsub(pattern = "#", replacement = " ",  x =d[[1]])

  if(length(header_inds) > 0 ){

    bgrsp = paste(sep = "", header_inds, collapse = ", ")
    l_innerspecs[["bgcol"]] = paste0("row{", bgrsp, "}={bg=jchshlightgray}")
  }
  else
    l_innerspecs[["bgcol"]] = NULL


  ####### run cell markdown parser
  tab = lapply(d, FUN =\(x){

      # boldify
      #pattern <- "\\*\\*(?:[^{}]*|(?R))*\\*\\*"
      #(result <- regmatches(x, gregexpr(pattern, x, perl = TRUE)))
      # pars cell md

      x = sapply(x, \(c) {
          r = parse_cellmd(c)
          # escape relevant chars &, #, {, }
          r = gsub(x = r, pattern = "&", replacement = "\\&", fixed = T)
          r = gsub(x = r, pattern = "#", replacement = "\\#", fixed = T)

          r = gsub(x = r, pattern = "\\{", replacement = "\\{")
          r = gsub(x = r, pattern = "\\}", replacement = "\\}")

          r = gsub(x = r, pattern = "[", replacement = "{[}", fixed = T)
          r = gsub(x = r, pattern = "]", replacement = "{]}", fixed = T)


          r = paste0("{", r ,"}")
        })



      x = gsub(x = x,pattern = "[ ]{3}", replacement = "  \\\\hspace*{1mm} ") # 3 empty spaces
      x = gsub(x = x,pattern = "[%]", replacement = "\\\\%")
      #x = paste0("{", x, "}")
      x
    })  |> data.frame()

  rows = apply(X = tab, MARGIN = 1, paste0, collapse = " & ")

  rows = paste0(rows, "\\\\")

  if(xltabular == T){
    rows[header_inds] =  paste0("\\rowcolor{jchshlightgray}", rows[header_inds], "")

    if(!is.null(colbreak_inds) && length(colbreak_inds) > 0  && !is.na(colbreak_inds) && is.integer(colbreak_inds))
    {
      rows[colbreak_inds] =  paste0(rows[colbreak_inds], "\\pagebreak")
    }

    # check we do not overflow
    # thi = header_inds
    # if(length(thi) > 0 && thi[length(thi)] == length(rows))
    #   thi = thi[1:(length(thi)-1)]

  }


  # https://sv.overleaf.com/learn/latex/Errors/Misplaced_%5Cnoalign - all hlines after first haline need to be preceeded with \\ (escape needed)s
  rowhline = "\n \\hline "
  firsthline = "\n \\hline "
  lasthline = "\n \\hline"
  if(fullgrid == TRUE){
    body = paste0(firsthline, paste0(rows, rowhline, collapse = "\n"))
  } else{
    body = paste0(firsthline, paste0(rows, collapse = "\n"), lasthline)
  }





  row_specspan = paste0("1-",NROW(d))

  colspecs = gen_colspecs(ncol= ncol(tab), scheme = "twocol", xltabular = xltabular, colwidths = colwidths, colaligns = colaligns, fullgrid = fullgrid)

  #l_innerspecs[["colspec"]] = paste0("colspec = {", paste0(rep("X", times = NCOL(tab)), collapse = ""), "}")
  l_innerspecs[["colspec"]] = paste0("colspec = {",colspecs , "}")
  #l_innerspecs[["linewidth"]] = "width=0.3\\linewidth"
  l_innerspecs[["linewidth"]] = "width=1\\linewidth"
  l_innerspecs[["cells2"]] = "cells={font=\\sffamily}"
  #l_innerspecs[["row1"]] = "row{1}={font=\\textbf}"
  #l_innerspecs[["cells"]] = paste0("cell={",row_specspan,"}{2}={c}")

  #l_innerspecs[["valign"]] = "column{2}={c}"

  if(!is.null(caption)){
      l_outerspecs[["caption"]] = "caption={" %+% caption %+% "}"
  }


  #https://tex.stackexchange.com/questions/45980/balancing-long-table-inside-multicol-in-latex

  # this setup tries to deal with two column tables a as flexibly as possible (full autmoatic column pae breaking does not work , neither with tabularray)
  # - use tblr for one column tables per default. They can not be longer than one column
  # - use longtblr for page wide table

  pre_envouter = ""
  post_envouter = ""

#   pre_envouter = "\\makeatletter\\mysavecolroom=\\@colroom\\makeatother
# \\setbox\\ltmcbox\\vbox{
# \\makeatletter\\col@number\\@ne\\makeatother"
#   post_envouter = "\\unskip
# \\unpenalty
# \\unpenalty}
# \\makeatletter\\@colroom=\\mysavecolroom\\makeatother
# \\unvbox\\ltmcbox"


  # for single column tables
  if(long == F){
    begin_envinner  = "\\begin{tblr}"
    end_envinner = "\\end{tblr}"
  }
  else{
    begin_envinner  = "\\begin{longtblr}"
    end_envinner = "\\end{longtblr}"
  }


  # for double column stretching tables
  if(mode  == "wide"){
    # pre_envouter = "\\end{multicols}"
    # post_envouter = "\\begin{multicols}{2}"

    # pre_envouter = "\\begin{strip}"
    # post_envouter = "\\end{strip}" # strip would work but too much spacing

    # pre_envouter = "\\vfill\\pagebreak\\renewcommand{\\TblrNewPage}{\\clearpage}"
    # post_envouter = "\\renewcommand{\\clearpage}{\\newpage}"
    #
    # pre_envouter = "\\onecolumn"
    # post_envouter = "\\twocolumn"

    begin_envinner  = "\\begin{longtblr}"
    end_envinner = "\\end{longtblr}"

    #begin_tablestar = "\\renewcommand{\\TblrNewPage}{\\clearpage} \n"
    #end_tablestar = " \\twocolumn"
    l_innerspecs[["linewidth"]] = "width=1\\textwidth"
    l_innerspecs[["colspec"]] = paste0("colspec = {",gen_colspecs(ncol= ncol(tab), scheme = "onecol"), "}")

    #l_innerspecs[["colspec"]] = paste0("colspec = {", paste0(rep("X[l]", times = NCOL(tab)), collapse = ""), "}")
  }
  if(xltabular == T){

    tbl_env = "xltabular"

    lt_footnote = NULL
    if(!is.null(footnote)){
      lt_footnote = "\\caption*{\\raggedright\\sffamily\\fontsize{9}{11}\\selectfont " %+% gsub("\\n" , "\\\\newline ", footnote) %+%"}"

    }



    if(mode == "wide"){
      pre_envouter = "\\end{multicols}"
      post_envouter = "\\begin{multicols}{2}\\raggedcolumns"
      # pre_envouter = "\\begin{table*}\\begin{multicolslongtable}" # put for floating table. Only works with non-long tables
      # post_envouter = "\\end{multicolslongtable}\\end{table*}"
    }
    else if(mode == "landscape"){
      # https://tex.stackexchange.com/questions/19017/how-to-place-a-table-on-a-new-page-with-landscape-orientation-without-clearing-t

      pre_envouter = "\\end{multicols}\\begin{landscape}\\centering\\thispagestyle{lscape}\\pagestyle{lscape}"
      post_envouter = "\\end{landscape}\\clearpage\\begin{multicols}{2}"
    }
    else if(mode == "twocolumn"){ # default
      pre_envouter = "\\begin{multicolslongtable}"
      post_envouter = "\\end{multicolslongtable}"
    }
    else{
      stop("rabulify: unkown table mode")
    }
    begin_envinner  = "\\setlength\\arrayrulewidth{1pt}{\\renewcommand{\\arraystretch}{1.2}\\sffamily\\fontsize{8.5}{11}\\selectfont
\\begin{"%+% tbl_env %+%"}"
    end_envinner = "\\end{"%+% tbl_env %+%"}}"

    if(!is.null(label) && is.character(label)){

      lab  = paste0("\\label{",label,"}")
    }
    tex = paste0(pre_envouter,
                 begin_envinner , "{1\\linewidth}{",colspecs, "}", "\n",
                 "\\caption{\\raggedright\\sffamily\\fontsize{9}{11}\\selectfont ", caption, "}", lab,"\\\\\n",
                 body,
                 lt_footnote,
                 end_envinner,"\n",
                 post_envouter,"\n\\vspace{3mm}")
  }
  else{

    # tabularrayu

    innerspecs = paste0(collapse = ", ", l_innerspecs)
    outerspecs = paste0(collapse = ", ", l_outerspecs)
    # tabularray specification, see manual
    tex = paste0(pre_envouter,
                 begin_envinner ,"\n",
                 "[", outerspecs, "]",
                 "{", innerspecs, "}",
                 "\n", body, "\n",
                 end_envinner,"\n",
                 post_envouter,"\n")
  }
  tex


}


