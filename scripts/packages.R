# # install.packages('pacman')

package_list <- c(
  'plotKML',  #plot kml needed to go ahead of other packages for some reason and wants to reinstall everytime.... not sure why. hash out for now
  'raster', #load this dog before dplyr yo
  'tidyverse',
  'readwritesqlite',
  'sf',
  'readxl',
  'janitor',
  'leafem',
  'leaflet',
  'kableExtra',
  'httr',
  'RPostgres',
  'RPostgreSQL',
  'DBI',
  'magick',
  # 'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
  'lubridate',
  'forcats',
  'bookdown',
  'fasstr',
  'tidyhydat',
  'elevatr',
  'rayshader',
  'exifr',
  'english',
  'leaflet.extras',
  'ggdark',
  'geojsonio',
  'pdftools',
  'xlsx',
  'pagedown'
  # 'analogsea',
  # 'here'
  # rgl,
  # geojsonsf,
  # bit64 ##to make integer column type for pg
  # gert  ##to track git moves
  ##leafpop I think
)

# lapply(package_list,
#        require,
#        character.only = TRUE)

pacman::p_load(package_list,
               character.only = TRUE)


pacman::p_load_gh("poissonconsulting/fwapgr",
                  'poissonconsulting/poisspatial',
                  # "crsh/citr",
                  # 'rstudio/pagedown',
                  "poissonconsulting/fishbc")
