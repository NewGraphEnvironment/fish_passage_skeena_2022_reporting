source('scripts/packages.R')
# library(magrittr)
# library(dplyr)

# import a geopackage and rearrange then burn to csv

# relative path to the q project form (assuming we have same folder structure) -
# use an absolute path if you have to but it is preferable to keep our relative paths the same so we can collab

# name the project directory we are pulling from
dir_project <- 'bcfishpass_skeena_20220823'

# find all the pscis forms in the file
list.files(path = paste0('../../gis/mergin/',
                  dir_project),
           # ?glob2rx is a funky little unit
           pattern = glob2rx('form_pscis_*.gpkg')
           )

# lets do multiple forms manually for the pars and then we will get into purrr functions
# and read them all in at once for the skeena


# name the first form we are reading
form_id <- 'form_pscis_202208251308.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

# read in the first form
form1 <- sf::st_read(path)

# name the second form we are reading
form_id <- 'form_pscis_202208280808.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

form2 <- sf::st_read(path)

#name third form
form_id <- 'form_pscis_202208282108.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

form3 <- sf::st_read(path)

# name fourth form
form_id <- 'form_pscis_202209050809.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

form4 <- sf::st_read(path)

#name fifth form
form_id <- 'form_pscis_202209060709.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

# read in the first form
form5 <- sf::st_read(path)

# name sixth form
form_id <- 'form_pscis_202209061414.gpkg'

path <- paste0('../../gis/mergin/',
               dir_project,
               '/',
               form_id)

form6 <- sf::st_read(path)

# join our forms together
form_raw <- bind_rows(
  form1,
  form2,
  form3,
  form4,
  form5,
  form6
)


# this is a table that cross references column names for pscis table and has the columns in the same order as the spreadsheet
xref_names_pscis <- fpr::xref_names_pscis

# get order of columns as per the excel template spreadsheet
# this can be used as a select(all_of(name_pscis_sprd_ordered)) later
# to order columns for the fieldform and/or put the field entered table inorder
name_pscis_sprd_ordered <- fpr::xref_names_pscis %>%
  filter(!is.na(spdsht)) %>%
  select(spdsht) %>%
  pull(spdsht)


# this one is the gov names
# name_pscis_bcdata_ordered <- fpr::xref_names_pscis %>%
#   filter(!is.na(bcdata)) %>%
#   select(bcdata) %>%
#   pull(bcdata)

# see names that coincide between the xref table and what we have
intersect(name_pscis_sprd_ordered, names(form_raw))

# see names that coincide between the xref table and what we have
# intersect(name_pscis_bcdata_ordered, names(form))

# see which are different
setdiff(name_pscis_sprd_ordered, names(form_raw))
# order matters
setdiff(names(form_raw), name_pscis_sprd_ordered)


form_prep1 <- form_raw %>%
  #split date time column into date and time

  # filter out to get only the records newly created
  filter(!is.na(date_time_start)) %>%
  # when necessary split your time into a date and time - skeena only
  form_prep1$date <- as.Date(form_prep1$date_time_start)
  form_prep1$time <- format(as.POSIXct(form_prep1$date_time_start),"%H:%M:%S") %>%
  # note the call to any of
  select(any_of(name_pscis_sprd_ordered)) %>%
  # we are better off leaving the coordinates as per the crossings layer of bcfishpass than moving them around if
  # the switch that utm_corrected is not hit.  If it was we can pull out the coordinates from the geom or use the ones that autopopulate
  # they should be the same but we should qa it
  # see the *_template.R script to see how to convert CRS and pull out coordinates
  # geom is not a column in the spreadsheet template so remove
  sf::st_drop_geometry()


# to use all the columns from the template first we make an empty dataframe from a template
template <- fpr::fpr_import_pscis() %>%
  slice(0)

# then we join it to our populated spreadsheet
# we may as well keep all the columns that are not in the spreadsheet and append to the end
form <- bind_rows(
  template,

  form_raw %>%
    # we are better off leaving the coordinates as per the crossings layer of bcfishpass than moving them around if
    # the switch that utm_corrected is not hit.  If it was we can pull out the coordinates from the geom or use the ones that autopopulate
    # they should be the same but we should QA it
    # see the *_template.R script to see how to convert CRS and pull out coordinates
    # geom is not a column in the spreadsheet template so remove
    sf::st_drop_geometry()
) %>%
  # then arrange it so it is easy to see how to sort to separate spreadsheets
  arrange(crossing_type,
          continuous_embeddedment_yes_no,
          backwatered_yes_no)


# burn to a csv
form %>%

  #sort data by date
  arrange(date) %>%

  readr::write_csv(paste0(
    'data/skeena/survey_form_',
    format(lubridate::now(), "%Y%m%d"),
    '.csv'))



