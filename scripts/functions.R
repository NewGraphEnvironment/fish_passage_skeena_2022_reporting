# build reporting table for minnow trap surveys
tab_fish_mt <- function(sit = my_site){
    hab_fish_indiv %>%
    filter(site == sit & sampling_method == 'minnow trapping' & !is.na(life_stage)) %>%
    group_by(site, location, species_code, life_stage) %>%
    summarise(Number = n()) %>%
    ungroup() %>%
    select(Location = location,
           Species = species_code,
           Stage = life_stage,
           Number) %>%
    pivot_wider(names_from = Stage,
                values_from = Number) %>%
    mutate(Location = case_when(Location == 'us' ~ 'Upstream',
                                T ~ 'Downstream')) %>%
    arrange(Species) %>%
    replace(is.na(.), 0)
}

# take habitat confirmation xls and make a csv with the alias_local_name pasted to the comments
# so people can see which comments are linked to which site
fpr_hab_alias_to_comments <- function(target = target_dir){
  fpr_import_hab_con(col_filter_na = T,
                     row_empty_remove = T,
                     backup = FALSE) %>%
    purrr::pluck("step_4_stream_site_data") %>%
    mutate(comments = paste0('Site ', local_name, '. ', comments)) %>%
    select(reference_number, gazetted_names, local_name, comments) %>%
    write_csv(target_dir)
}
