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
