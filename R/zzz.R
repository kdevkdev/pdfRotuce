# #' @export lexer
# #' @export parser


lexer = NULL
parser_xml = NULL
parser_latex = NULL

.onLoad <-  function(libname, pkgname){


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
        c('left', 'LBREAK'),
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


      # t_TEXT = function(re = "[^`*\n\t\\\\\\[\\]]+",t){
      t_TEXT = function(re = "[^`*\n\t\\\\]+",t){
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
      mode = NULL,
      initialize = function(...){

        args = (list(...))

        switch(args$mode,
               xml = { self$mode <- "xml"},
               latex = {self$mode <- "latex"},
               stop("undefined parser mode")
        )
      },

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

        if(self$mode == "latex")      p$set(1, paste0("\\bfseries{\textit{",p$get(3), "}}"))
        else if(self$mode == "xml")   p$set(1, paste0("<bold><italic>",p$get(3), "</italic></bold>"))

        #p$set(1, paste0("{{",p$get(3), "}}"))
        messlog("p_emphboldgroup: ",p$get(1))
      },
      p_boldgroup = function(doc = 'boldgroup : BOLD expression BOLD',p){

        if(self$mode == "latex")      p$set(1, paste0("\\textbf{",p$get(3), "}"))
        else if(self$mode == "xml")   p$set(1, paste0("<bold>",p$get(3), "</bold>"))
        #p$set(1, p$get(3))
        #p$set(1, paste0("{",p$get(3), "}"))
        messlog("p_boldgroup: ",p$get(1))
      },
      p_emphgroup = function(doc = 'emphgroup : EMPH expression EMPH',p){

        if(self$mode == "latex")      p$set(1, paste0("\\textit{",p$get(3),  "}"))
        else if(self$mode == "xml")   p$set(1, paste0("<italic>",p$get(3),  "</italic>"))

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
        if(self$mode == "latex")      p$set(1, "\\vphantom{1}\\par ")# with phantom to ensure empty lines get a break as well
        else if(self$mode == "xml")   p$set(1, "<break/>") # only allowed within table cells and discouraged according to JATS
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
  lexer  <<- rly::lex(Lexer)

  parser_latex <<- rly::yacc(Parser, debug = T, args = list(mode = "latex"))
  parser_xml <<- rly::yacc(Parser, debug = T, args = list(mode = "xml"))




  }
