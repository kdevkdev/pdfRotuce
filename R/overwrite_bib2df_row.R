overwrite_bib2df_row = function(nrow, row_index, df, non_existing_fields = "append"){
  # non-existing fields: either "append", "warn",  or "fail"
  # replace - warning this may also override existing values with empty/NA
  for(ccname in colnames(nrow)){
    if(ccname %in% colnames(df)){
      df[[ccname]][row_index] = nrow[[ccname]][1]
    } else if(non_existing_fields == "append"){

      df[[ccname]] = NA
      df[[ccname]][row_index] = nrow[[ccname]][1]
    }
    else if(non_existing_fields == "warn"){
      hgl_warn(paste0("overwrite_bib2df_row: unkown field in correcection-bibtex item that are not present in original bibtex entry: ", ccname))
    }
    else if(non_existing_fields == "fail") {
      hgl_error(paste0("overwrite_bib2df_row: unkown field in correcection-bibtex item that are not present in original bibtex entry: ", ccname))
    }
  }
  df
}
