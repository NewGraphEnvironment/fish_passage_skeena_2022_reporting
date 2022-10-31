
## QA to be sure that you have all 6 required photos for submission to PSCIS
## convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis


##path to the photos
path <- paste0(getwd(), '/data/photos')

##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase1.xlsm')

folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- paste0(getwd(), '/data/photos/', folderstocopy)


##########################here we back everything up to the D drive###################################################################
targetdir = paste0("D:/PSCIS/PSCIS_bulkley_2021_phase1/")
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)

filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste
empty_idx <- which(!lengths(filestocopy_list))

fpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

empty_files <- empty_idx %>% fpr_filter_list()


##-----------------------------rename long names-------------------------------------------


photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability notheless

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_phase1.csv'))

fpr_photo_change_name <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = paste0(getwd(), '/data/photos/'), replacement = targetdir)
}


filestopaste_list <- filestocopy_list %>%
  map(fpr_photo_change_name)

# filestopaste_list <- path_to_photos %>%
#   purrr::map2(paths_to_copy)

fpr_copy_over_photos <- function(filescopy, filespaste){
  file.copy(from=filescopy, to=filespaste,
            overwrite = T,
            copy.mode = TRUE)
}


##!!!!!!!!!!!!!!!copy over the photos!!!!!!!!!!!!!!!!!!!!!!!
mapply(fpr_copy_over_photos,
       filescopy =  filestocopy_list,
       filespaste = filestopaste_list)

##also move over the pscis file
file.copy(from = 'data/pscis_phase1.xlsm',
          to = paste0(targetdir, 'pscis_phase1.xlsm'),
          overwrite = T)



## going to make a few notes here about the submission process
## we need to work in microsoft edge and put sites in "Internet Explorer mode pages" and set exceptions for uploading to soft and esf
## https://www.env.gov.bc.ca/csd/imb/soft/soft.shtml
## https://logon7.gov.bc.ca/clp-cgi/capBceid/logon.cgi?flags=1111:1,8&TARGET=$SM$https%3a%2f%2fapps%2enrs%2egov%2ebc%2eca%2fext%2fesf%2fsubmissionWelcome%2edo


# soft url
# https://www.env.gov.bc.ca/perl/soft/dl.pl/20220331022839-14-gp-92bd0e96-9ff1-451a-9fa1-0d72e543?simple=y

##check on your submission here https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=detail&submissionId=2142948
# https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=detail&submissionId=2143131
# https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=detail&submissionId=2143132
