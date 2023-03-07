# this script gets a total count of fish passage field assessments done

# xref my_crossings pscis, pull records from bcdata using record ID

get_this <- bcdata::bcdc_tidy_resources('7ecfafa6-5e18-48cd-8d9b-eae5b5ea2881') %>%
  filter(bcdata_available == T)  %>%
  pull(package_id)

dat <- bcdata::bcdc_get_data(get_this)

## xref_pscis_my_crossing_modelled
xref_pscis_my_crossing_modelled <- dat %>%
  purrr::set_names(nm = tolower(names(.))) %>%
  dplyr::filter(consultant_name == "IRVINE") %>%
  sf::st_drop_geometry() %>%
  tidyr::separate(assessment_date, into = c('year', 'month', 'day'), remove = F)

# total count by year for all pscis assessments and habitat confirmations (before unsubmitted 2022 sites added)
xref_total_count <- xref_pscis_my_crossing_modelled %>%
  group_by(year) %>%
  summarise(total = n())

# total count by year for all phase 2 habitat confirmations
hab_con_count <- xref_pscis_my_crossing_modelled %>%
  filter(str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  add_row() %>%
  # add missing phase 2s from 2022
  # skeena: 9
  # parsnip: 4
  # bulkley: 7
  mutate(year = case_when(is.na(year) ~ '2022', T ~ year)) %>%
  mutate(total = case_when(year == '2022' ~ 20, T ~ total))

# total count by year for all pscis assessments
pscis_count <- xref_pscis_my_crossing_modelled %>%
  filter(!str_detect(funding_project, 'Fish Habitat Confirmations|Phase 2')) %>%
  group_by(year) %>%
  summarise(total = n()) %>%
  # add missing reassessments from 2022 (all phase 1s have been submitted)
  # skeena: 7
  # parsnip: 5
  # bulkley: 5
  # elk: 2
  mutate(total = case_when(year == '2022' ~ total + 19, T ~ total))

# filter out records that have been submitted recently to figure out which assessments have not been submitted yet
# and are therefore not included in the total count yet
# current_count <- xref_pscis_my_crossing_modelled %>%
#   filter(year == '2022')

