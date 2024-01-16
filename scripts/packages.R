pkgs_cran <- c(
  # 'raster', #load this dog before dplyr yo
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
  # 'RPostgreSQL',
  'DBI',
  'magick',
  'bcdata',
  'jpeg',
  'datapasta',
  'knitr',
  'data.table',
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
  'pagedown',
  'crosstalk',
  'DT',
  'fishbc',
  'chron',
  'remotes',
  'roxygen2',
  'devtools'

  # 'analogsea',
  # 'here'
  # rgl,
  # geojsonsf,
  # bit64 ##to make integer column type for pg
  # gert  ##to track git moves
  ##leafpop I think
)
pkgs_gh <- c(#'Envirometrix/plotKML',  #plot kml needed to go ahead of other packages for some reason and wants to reinstall everytime.... not sure why. hash out for nowpoissonconsulting/fwapgr",
  'poissonconsulting/poisspatial',
  'poissonconsulting/fwapgr',
  "newgraphenvironment/fpr"
)

pkgs_all <- c(pkgs_cran,
              pkgs_gh)


# install or upgrade all the packages with pak
lapply(pkgs_all,
       pak::pkg_install, dependencies = TRUE)

# load all the packages

pkgs_ld <- c(pkgs_cran,
             basename(pkgs_gh))

lapply(pkgs_ld,
       require,
       character.only = TRUE)
