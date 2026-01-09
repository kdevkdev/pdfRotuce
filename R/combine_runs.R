combine_runs = function(doc_summar, tabcollapse = "\n"){


  # loop through all doc parts
  res = list()
  # loop through unique doc parts
  unique_docinds = sort(unique(doc_summar$doc_index))
  for(i in 1:length(unique_docinds)){

    cdoc_i = unique_docinds[i]
    cdoc_part =doc_summar[doc_index == cdoc_i]

    # todo: insert rmarkdown for some formatting

    # only copy firsts to res list
    res[[i]] = cdoc_part[1,]
    # collapse content text
    res[[i]]$text = paste0(cdoc_part$run_content_text, collapse = "")


  }
  r_dt = data.table::rbindlist(l = res)

  # also check table cells
  # res2 = list()
  # unique_tabinds = sort(unique(r_dt$table_index))
  #
  # for(i in 1:length(unique_tabinds)){
  #
  #   ctab_i = unique_tabinds[i]
  #
  #   # find dublicate cell ids for current taable
  #   cdoc_part = doc_summar[table_index == ctab_i]
  #
  #   dup_cell_ids = unique(cdoc_part$cell_id[duplicated(cdoc_part$cell_id)])
  #
  #   browser()
  #   # combine and remove dups, directly in bigger data table
  #   for(cdid in dup_cell_ids){
  #
  #     # this time put a seperaor because its seperate paragraphs
  #     combtext = paste0(r_dt[table_index == ctab_i & cell_id == cdid]$text, collapse = tabcollapse)
  #
  #     # only keep first one
  #     slice_inds = r_dt[, which(table_index == ctab_i & cell_id == cdid)]
  #     first_ind = slice_inds[1]
  #     r_dt$text[first_ind] = combtext
  #     slice_inds = slice_inds[-1]
  #
  #     r_dt = r_dt[-slice_inds]
  #   }
  # }
  # browser()


  # remove all unneded columns now
  r_dt$run_content_text          = NULL
  r_dt$field_code                = NULL
  r_dt$image_path                = NULL
  r_dt$align                     = NULL
  r_dt$keep_with_next            = NULL
  r_dt$shading_fill              = NULL
  r_dt$shading_color             = NULL
  r_dt$shading                   = NULL
  #r_dt$link                      = NULL
  r_dt$link_to_bookmark          = NULL
  #r_dt$field_code                = NULL
  r_dt$footnote_text             = NULL
  r_dt$link                      = NULL
  r_dt$bookmark_start            = NULL
  r_dt$sz                        = NULL
  r_dt$sz_cs                     = NULL
  r_dt$font_family_ascii         = NULL
  r_dt$font_family_eastasia      = NULL
  r_dt$font_family_hansi         = NULL
  r_dt$font_family_cs            = NULL
  r_dt$bold                      = NULL
  r_dt$italic                    = NULL
  r_dt$underline                 = NULL
  r_dt$color                     = NULL
  r_dt$character_stylename       = NULL

  return(r_dt)
}
