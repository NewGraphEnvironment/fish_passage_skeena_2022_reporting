## Scripts to update and add data from the 2023 sampling year

source("scripts/packages.R")

file <- "habitat_confirmations.xls"
move_from <- paste0("~/Projects/repo/fish_passage_skeena_2023_reporting/data/", file)
move_to <- "data/2023"


##move the files
file.copy(from = move_from, to = move_to, overwrite = TRUE)


####-------------- habitat and fish data------------------
habitat_confirmations <- fpr_import_hab_con(path = "data/2023/habitat_confirmations.xls", col_filter_na = T, row_empty_remove = T, backup = FALSE)

hab_site_prep <-  habitat_confirmations %>%
  purrr::pluck("step_4_stream_site_data") %>%
  # tidyr::separate(local_name, into = c('site', 'location'), remove = F) %>%
  mutate(average_gradient_percent = round(average_gradient_percent * 100, 1)) %>%
  mutate_if(is.numeric, round, 1) %>%
  select(-gazetted_names:-site_number, -feature_type:-utm_method) %>%   ##remove the feature utms so they don't conflict with the site utms
  distinct(reference_number, .keep_all = T) ##since we have features we need to filter them out


hab_loc <- habitat_confirmations %>%
  purrr::pluck("step_1_ref_and_loc_info") %>%
  dplyr::filter(!is.na(site_number))%>%
  mutate(survey_date = janitor::excel_numeric_to_date(as.numeric(survey_date)))

hab_site <- left_join(
  hab_loc,
  hab_site_prep,
  by = 'reference_number'
) %>%
  tidyr::separate(alias_local_name, into = c('site', 'location'), remove = F) %>%
  mutate(site = as.numeric(site)) %>%
  dplyr::filter(!alias_local_name %like% '_ef') ##get rid of the ef sites

hab_fish_collect_map_prep <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  distinct(local_name, species, .keep_all = T) %>% ##changed this to make it work as a feed for the extract-fish.R file
  mutate(across(c(date_in,date_out), janitor::excel_numeric_to_date)) %>%
  mutate(across(c(time_in,time_out), chron::times))


##prep the location info so it is ready to join to the fish data
hab_loc2 <- hab_loc %>%
  tidyr::separate(alias_local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, location)) %>%
  filter(str_detect(alias_local_name, 'ef|198222|mt')) ##filter ef and mt sites


# test to see what we get at each site
test <- hab_fish_collect_map_prep %>%
  distinct(local_name, species)



##join the tables together
hab_fish_collect_map_prep2 <- right_join(
  # distinct to get rid of lots of sites
  select(hab_loc2, reference_number, alias_local_name, utm_zone:utm_northing) %>% distinct(alias_local_name, .keep_all = T),
  select(hab_fish_collect_map_prep %>% distinct(local_name, species, .keep_all = T), local_name, species),
  by = c('alias_local_name' = 'local_name')
)


##add the species code
hab_fish_codes <- fishbc::freshwaterfish %>%
  select(species_code = Code, common_name = CommonName) %>%
  tibble::add_row(species_code = 'NFC', common_name = 'No Fish Caught') %>%
  mutate(common_name = case_when(common_name == 'Cutthroat Trout' ~ 'Cutthroat Trout (General)', T ~ common_name))

# this is the table to burn to geojson for mapping
# we are just going to keep 1 site for upstream and downstream because more detail won't show well on the map anyway
# purpose is to show which fish are there vs. show all the sites and what was caught at each. TMI

hab_fish_collect_map_prep3 <- left_join(
  hab_fish_collect_map_prep2 %>%
    mutate(species = as.factor(species)),  ##just needed to do this b/c there actually are no fish.

  select(hab_fish_codes, common_name, species_code),
  by = c('species' = 'common_name')
)
# this time we ditch the nfc because we don't want it to look like sites are non-fish bearing.  Its a multipass thing
# WATCH THIS IN THE FUTURE
# filter(species_code != 'NFC')

# need to make an array for mapping the hab_fish_collect files
# this gives a list column vs an array.  prob easier to use postgres and postgis to make the array
hab_fish_collect <- left_join(
  hab_fish_collect_map_prep3 %>%
    select(alias_local_name:utm_northing) %>%
    distinct(),

  hab_fish_collect_map_prep3 %>%
    select(-species, -reference_number, -utm_zone:-utm_northing) %>%
    pivot_wider(names_from = 'alias_local_name', values_from = "species_code") %>%
    pivot_longer(cols = '8530_ds_ef2':'8525_ds_mt') %>%
    rename(alias_local_name = name,
           species_code = value),

  by = 'alias_local_name'
) %>%
  rowwise() %>%
  mutate(species_code = toString(species_code),
         species_code = stringr::str_replace_all(species_code, ',', ''))


rm(hab_fish_collect_map_prep, hab_fish_collect_map_prep2)

hab_fish_collect_prep1 <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_name:-site_number)

hab_features <- left_join(
  habitat_confirmations %>%
    purrr::pluck("step_4_stream_site_data") %>%
    select(reference_number,local_name, feature_type:utm_northing) %>%
    filter(!is.na(feature_type)),

  fpr::fpr_xref_obstacles,

  by = c('feature_type' = 'spreadsheet_feature_type')
)


## fish densities ----------------------------------------------------------
## Needed for fpr_table_fish_density() and fpr_plot_fish_box()

hab_fish_indiv_prep <- habitat_confirmations %>%
  purrr::pluck("step_3_individual_fish_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  select(-gazetted_names:-site_number)

hab_fish_indiv_prep2 <- left_join(
  hab_fish_indiv_prep,
  hab_loc,
  by = 'reference_number'
)

hab_fish_indiv_prep3 <- left_join(
  hab_fish_indiv_prep2,
  select(hab_fish_codes, common_name:species_code),
  by = c('species' = 'common_name')
) %>%
  dplyr::select(reference_number,
                alias_local_name,
                site_number,
                sampling_method,
                method_number,
                haul_number_pass_number,
                species_code,
                length_mm,
                weight_g) ##added method #

hab_fish_collect_info <- habitat_confirmations %>%
  purrr::pluck("step_2_fish_coll_data") %>%
  dplyr::filter(!is.na(site_number)) %>%
  # select(-gazetted_name:-site_number) %>%
  dplyr::distinct(reference_number, sampling_method, method_number, haul_number_pass_number, .keep_all = T)

# join the indiv fish data to existing site info
hab_fish_indiv <- full_join(
  select(hab_fish_indiv_prep3,
         reference_number,
         sampling_method,
         method_number,
         haul_number_pass_number,
         species_code,
         length_mm,
         weight_g),
  select(hab_fish_collect_info,
         reference_number,
         local_name,
         temperature_c:model, ##added date_in:time_out
         comments
  ),
  by = c(
    "reference_number",
    # 'alias_local_name' = 'local_name',
    "sampling_method",
    "method_number",
    "haul_number_pass_number")
) %>%
  mutate(species_code = as.character(species_code)) %>%
  mutate(species_code = case_when(
    is.na(species_code) ~ 'NFC',
    T ~ species_code)
  ) %>%
  mutate(species_code = as.factor(species_code)) %>%
  mutate(life_stage = case_when(  ##this section comes from the histogram below - we include here so we don't need to remake the df
    length_mm <= 65 ~ 'fry',
    length_mm > 65 & length_mm <= 110 ~ 'parr',
    length_mm > 110 & length_mm <= 140 ~ 'juvenile',
    length_mm > 140 ~ 'adult',
    T ~ NA_character_
  ),
  life_stage = case_when(
    species_code %in% c('L', 'SU', 'LSU') ~ NA_character_,
    T ~ life_stage
  ))%>%
  mutate(life_stage = fct_relevel(life_stage,
                                  'fry',
                                  'parr',
                                  'juvenile',
                                  'adult')) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
  mutate(site_id = paste0(site, '_', location))

# make a summary table for fish sampling data

tab_fish_summary <- hab_fish_indiv %>%
  group_by(site_id,
           ef,
           sampling_method,
           species_code) %>% ##added sampling method!
  summarise(count_fish = n()) %>%
  arrange(site_id, species_code, ef)

# this will be joined to the abundance estimates and the confidence intervals
fish_abund_prep <- hab_fish_indiv %>%
  group_by(local_name,
           site_id,
           ef,
           sampling_method,
           haul_number_pass_number,
           species_code,
           life_stage,
           ef_seconds) %>% ##added sampling method!
  filter(sampling_method == 'electrofishing') %>%
  summarise(catch = n()) %>%
  arrange(site_id, species_code, ef) %>%
  # ungroup() %>%
  mutate(catch = case_when(
    species_code == 'NFC' ~ 0L,
    T ~ catch),
    # effort = catch/ef_seconds,
    id = paste0(local_name, '_', species_code, '_', life_stage)) %>%
  ungroup() %>%
  arrange(id)

# join the total number of passes to each event so that we know if it is a higher number than the pass of the catch
fish_abund_prep2 <- left_join(
  fish_abund_prep,

  fish_abund_prep %>%
    group_by(local_name) %>%
    summarise(pass_total = max(haul_number_pass_number)),
  by = 'local_name'
)

# make a dat to indicate if the nfc in the set for each species
fish_nfc_tag<- fish_abund_prep2 %>%
  mutate(nfc_pass = case_when(
    # species_code != 'NFC' &
    haul_number_pass_number == pass_total ~ F,
    T ~ T),
    nfc_pass = case_when(
      species_code == 'NFC' ~ T,
      T ~ nfc_pass)
  ) %>%
  select(local_name, species_code, life_stage, haul_number_pass_number, pass_total, nfc_pass) %>%
  arrange(desc(haul_number_pass_number)) %>%
  # filter(nfc_pass == T) %>%
  distinct(local_name, species_code, life_stage, .keep_all = T) %>%
  select(-haul_number_pass_number, -pass_total)

# dat to show sites  for those that have a pass where no fish of those species were captured
# nfc_pass tag used to indicate that this is an abundance estimate
# fish_nfc_tag <- left_join(
#   fish_abund_prep2,
#
#   fish_nfc_prep,
#   by = c('local_name','species_code', 'life_stage', 'haul_number_pass_number', 'pass_total')
# ) %>%
#   tidyr::fill(nfc_pass, .direction = 'up')

# filter(!is.na(nfc_pass)) %>%

# mutate(nfc_pass = case_when(
#   species_code != 'NFC' ~ 'TRUE',
#   T ~ NA_character_))


# calculate abundance for each site regardless of whether a nfc_pass occurred.
fish_abund_prep3 <- left_join(
  fish_abund_prep2 %>%
    group_by(local_name, species_code, life_stage) %>%
    summarise(catch = sum(catch)),

  fish_nfc_tag,

  by = c('local_name', 'species_code', 'life_stage')
)


# add back the size of the sites so we can do a density
fish_abund <- left_join(
  fish_abund_prep3,

  hab_fish_collect_info %>%
    select(local_name,
           # sampling_method,
           # haul_number_pass_number,
           ef_seconds:enclosure) %>%
    distinct(local_name, ef_length_m, .keep_all = T),

  by = c('local_name')
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1),
         density_100m2 = round(catch/area_m2 * 100,1)) %>%
  tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)

### density results -----------------------------------------------------------
## Needed for fpr_table_fish_site()

# need to summarize just the sites
tab_fish_sites_sum <- left_join(
  fish_abund_prep2 %>%
    select(local_name, pass_total) %>%
    distinct(),


  hab_fish_collect_info %>%
    select(local_name,
           ef_length_m:enclosure) %>%
    distinct(),

  by = 'local_name'
) %>%
  mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
  select(site = local_name, passes = pass_total, ef_length_m, ef_width_m, area_m2, enclosure)

rm(
  fish_abund_nfc_prep,
  fish_abund_prep,
  fish_abund_prep2,
  fish_abund_prep3,
  fish_abund_prep4,
  fish_nfc_tag
)


# # table to summarize ef passes done in a site
# tab_fish_sites <- hab_fish_collect_info %>%
#   select(local_name, haul_number_pass_number, ef_seconds:enclosure) %>%
#   distinct() %>%
#   mutate(area_m2 = round(ef_length_m * ef_width_m,1)) %>%
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F)



# hab_fish_dens <- hab_fish_indiv %>%
#   filter(sampling_method == 'electrofishing') %>% ##added this since we now have mt data as well!!
#   mutate(area = round(ef_length_m * ef_width_m),0) %>%
#   group_by(local_name, method_number, haul_number_pass_number, ef_length_m, ef_width_m, ef_seconds, area, species_code, life_stage) %>%
#   summarise(fish_total = length(life_stage)) %>%
#   ungroup() %>%
#   mutate(density_100m2 = round(fish_total/area * 100, 1)) %>%
#   tidyr::separate(local_name, into = c('site', 'location', 'ef'), remove = F) %>%
#   mutate(site_id = paste0(site, location),
#          location = case_when(location == 'us' ~ 'Upstream',
#                               T ~ 'Downstream'),
#          life_stage = factor(life_stage, levels = c('fry', 'parr', 'juvenile', 'adult')))


