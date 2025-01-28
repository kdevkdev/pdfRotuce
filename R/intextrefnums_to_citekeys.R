# replace intext-citations with CITEKEYs
intextrefnums_to_citekeys = function(v_text, d_refs){

  # d_ref needs ot have entry 'BIBLIOGRAPY_NUMBER'
  stringr::str_replace_all(str = v_text, pattern = "\\[(([0-9]+([-–][0-9]+)?)(?:, ?)?)+\\]", replacement = \(match) {

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
      t = d_refs[BIBLIOGRAPHY_NUMBER %in% trefnums]$CITEKEY

      if(length(t) == 0 ) hgl_warn(paste0("intextrefnums_to_citekey: no citekey found for bibliography nunbers '", crange,"'"))
      locrefs_keys[[crange]] = t
    }
    # construct string to return
    replacement = paste0("[", paste0("========protectedat========", unlist(locrefs_keys),collapse = ";") , "]")
    replacement
  })
}
