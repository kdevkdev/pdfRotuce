library(rly)

TOKENS = c( 'EMPH', 'BOLD', 'EMPHBOLD', 'TEXT', 'LBREAK')
#LITERALS = c('**') # these are "LEXEMES" (ref: https://stackoverflow.com/questions/14954721/what-is-the-difference-between-a-token-and-a-lexeme)

# helper function to easily turn of logging
messlog = function(...){
 #message(m)
 #m = list(...) |>  paste0(collapse = "")
 #cat(m, "\n")
}

Lexer <- R6::R6Class(

  classname = "Lexer",

  public = list(

    tokens = TOKENS,

    #literals = LITERALS,
    # partially based on https://github.com/Tomohare/rydown/blob/master/rydown/lexer.py

    precedence = list(
      c('left', 'EMPH'),
      c('left', 'BOLD'),
      c('left', 'EMPHBOLD'),
      c('left', 'TEXT')
      #c('left', 'DSPACE')

    ),
    # t_DSPACE = function(re = "[[:space:]]{2}",t){
    #   return(t)
    # },
    #t_EMPH = function(re = "\\*([^ ][^*]+[^ ]|[^* ]+)\\*",t){
    t_EMPHBOLD = function(re = "\\*\\*\\*",t){
      messlog("found emphbold: ", t$value)
      #t$value = stringr::str_sub(t$value, 2,0) # cut two off from start
      return(t)
    },
    t_BOLD = function(re = "\\*\\*",t){
      messlog("found bold: ", t$value)
      #t$value = stringr::str_sub(t$value, 2,0) # cut two off from start
      return(t)
    },

    t_EMPH = function(re = "\\*",t){
      messlog("found emphasis: ", t$value)
      #t$value = stringr::str_sub(t$value, 1,0) # cut one off from start and end
      return(t)
    },


    t_TEXT = function(re = "[^`*\n\t\\\\\\[\\]]+",t){
    #t_TEXT = function(re = "[A-Za-z0-9]{1,}",t){
      messlog("found text: ", t$value)
      return(t)
    },


    #t_ignore = "\t",

    t_LBREAK = function(re='\\n', t) {
      t$lexer$lineno <- t$lexer$lineno + nchar(t$value)
      messlog("found newline: ", t$value)
      return(t)
    },

    t_eof = function(t) {
      messlog("found EOF")
      return(NULL)
    },

    t_error = function(t) {
      messlog(sprintf("Illegal token '%s'", t$value[1]))
      t$lexer$skip(1)
      return(t)
    }
  )
)
glob_b = NA
Parser <- R6::R6Class(

  classname = "Parser",

  public = list(

    tokens = TOKENS,


#    literals = LITERALS,

    p_expressions = function(doc='expressions : expression
                                              | expressions expression', p) {
      # p$set(1, self$names[[as.character(p$get(2))]])
      l = p$length()

      rv = vector("character", l)
      for(i in 2:l) rv[i] = p$get(i)

      rs = paste0(rv, collapse = "")

      p$set(1,rs)
      messlog("p_expressions (chain)",p$get(1))
    },
    p_emphboldgroup = function(doc = 'emphboldgroup : EMPHBOLD expression EMPHBOLD',p){

      p$set(1, paste0("\\bfseries{\textit{",p$get(3), "}}"))
      #p$set(1, paste0("{{",p$get(3), "}}"))
      messlog("p_emphboldgroup: ",p$get(1))
    },
    p_boldgroup = function(doc = 'boldgroup : BOLD expression BOLD',p){

      p$set(1, paste0("\\textbf{",p$get(3), "}"))
      #p$set(1, p$get(3))
      #p$set(1, paste0("{",p$get(3), "}"))
      messlog("p_boldgroup: ",p$get(1))
    },
    p_emphgroup = function(doc = 'emphgroup : EMPH expression EMPH',p){

      p$set(1, paste0("\\textit{",p$get(3),  "}"))
      #p$set(1, paste0("{",p$get(3),  "}"))
      messlog("p_emphgroup: ",p$get(1))
    },
    # p_dspace = function(doc = 'dspace : DSPACE',p){
    #   p$set(1, " \\hspace{0.5em} ")
    #   messlog("p_dspace: ",p$get(1))
    # },
    p_text = function(doc = 'text : TEXT',p){
      p$set(1, p$get(2))
      messlog("p_text: ",p$get(1))
    },

    p_lbreak = function(doc = 'lbreak : LBREAK',p){
      #p$set(1, " \\\\ ")
      p$set(1, "\\vphantom{1}\\par ")# with phantom to ensure empty lines get a break as well
      messlog("p_lbreak: ",p$get(1))
    },
    p_node = function(doc='expression : emphboldgroup
                                      | boldgroup
                                      | emphgroup
                                      | text
                                      | lbreak', p) {

      p$set(1, p$get(2))
      messlog("p_expression",p$get(1))
    },









    p_error = function(p) {
      if(is.null(p)){

          messlog("Finished") # messlog("Syntax error at EOF\n")
        }
        else{
        messlog(sprintf("Syntax error at '%s'\n", p$value))
        #browser()
      }
    }

  )
)
lexer  <- lex(Lexer)

parser <- yacc(Parser, debug = T)

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

rabulify = function(d, linesep = "\newline",wide = F, caption = NULL, label = NULL, long = F, xltabular = T, colwidths = NULL, colaligns = NULL, fullgrid = FALSE){
  l_innerspecs = list()
  l_outerspecs = list()

  # detect header rows
  header_inds = which(startsWith(d[[1]], "#"))
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

    # check we do not overflow
    thi = header_inds
    if(length(thi) > 0 && thi[length(thi)] == length(rows))
      thi = thi[1:(length(thi)-1)]

  }

  rowhline = ""
  if(fullgrid == TRUE){
    rowhline = "\n\\hline"
  }



  body = paste0(rowhline, paste0(rows, rowhline, collapse = "\n"))


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

  if(!is.null(caption))
      l_outerspecs[["caption"]] = "caption={" %+% caption %+% "}"


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
  if(wide == T){
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


    begin_envinner  = "\\setlength\\arrayrulewidth{1pt}{\\renewcommand{\\arraystretch}{1.2}\\sffamily\\fontsize{8.5}{11}\\selectfont
\\begin{xltabular}"
    end_envinner = "\\end{xltabular}}"


    if(wide ==T){
      pre_envouter = "\\end{multicols}"
      post_envouter = "\\begin{multicols}{2}"
    }
    else{
      pre_envouter = "\\begin{multicolslongtable}"
      post_envouter = "\\end{multicolslongtable}"
    }

    if(!is.null(label) && is.character(label)){

      lab  = paste0("\\label{",label,"}")
    }
    tex = paste0(pre_envouter,
                 begin_envinner , "{1\\linewidth}{",colspecs, "}", "\n",
                 "\\caption{", caption, "}", lab,"\\\\\n",
                 body, "\n",
                 end_envinner,"\n",
                 post_envouter,"\n")
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


