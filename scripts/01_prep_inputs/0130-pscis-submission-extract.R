
## QA to be sure that you have all 6 required photos for submission to PSCIS
## convert jpg (or other formats - need to code) to JPG for consistency and to avoid issues with submission/reporting
##move the photos and spreadsheet ready for submission to pscis


##path to the photos on onedrive
# path <- paste0(getwd(), '/data/photos')
path <- "/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_bulkley_2022_reporting/data/"

##use the pscis spreadsheet to make the folders to copy the photos to
d <- fpr::fpr_import_pscis(workbook_name = 'pscis_phase1.xlsm')

folderstocopy<- d$my_crossing_reference %>% as.character()

path_to_photos <- paste0(path, folderstocopy)


##########################here we transfer just the photos with labels over into the PSCIS directory where we will upload from to the gov interface ###################################################################
targetdir = paste0("/Users/airvine/Library/CloudStorage/OneDrive-Personal/Projects/PSCIS/PSCIS_bulkley_2022_phase1/")
dir.create(targetdir)

folderstocreate<- paste0(targetdir, folderstocopy)

##create the folders
lapply(folderstocreate, dir.create)

filestocopy_list <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_paths_to_copy) %>%
  purrr::set_names(basename(folderstocreate))


##view which files do not have any photos to paste by reviewing the empty_files object
empty_idx <- which(!lengths(filestocopy_list))

fpr_filter_list <- function(idx){
  filestocopy_list[idx]
}

empty_files <- empty_idx %>% fpr_filter_list()


##-----------------------------rename long names if necessary-------------------------------------------


photo_sort_tracking <- path_to_photos %>%
  purrr::map(fpr::fpr_photo_document_all) %>%
  purrr::set_names(folderstocopy) %>%
  bind_rows(.id = 'folder') %>%
  mutate(photo_name = str_squish(str_extract(value, "[^/]*$")),
         photo_name_length = stringr::str_length(photo_name))


###here we back up a csv that gives us the new location and name of the original JPG photos.
## Not ideal becasue we did some sorting by hand without adding name of camera to the file name but a start on reproducability nonetheless

##burn to csv
photo_sort_tracking %>%
  readr::write_csv(file = paste0(getwd(), '/data/photos/photo_sort_tracking_phase1.csv'))


fpr_photo_change_name <- function(filenames_to_change){
  gsub(filenames_to_change, pattern = path, replacement = targetdir)
}
#
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

# !!!!!!!READ!!!!!!! here I record the command to move everything into the repo via command line on linux.  Suggest moving to your repo using command line on windows (google copy all files and directories on command line with windows or something) as well because then it is easy to repeat when things change.
# not quite sure how best to deal with sharing the photos yet and might end up being easiest to just work in onedrive and copy things over via command line into the repo (hope not though)
# Mateo - write down the command to copy over with command line below the linux version
# cp -R ~/Library/CloudStorage/OneDrive-Personal/Projects/repo/fish_passage_bulkley_2022_reporting/data/* ~/Projects/repo/fish_passage_bulkley_2022_reporting/data/photos/

## going to make a few notes here about the submission process
## we need to work in microsoft edge and put sites in "Internet Explorer mode pages" and set exceptions for uploading to soft and esf

# Make exceptions for these sites in setting to use "Internet Explorer mode pages"
## https://www.env.gov.bc.ca/csd/imb/soft/soft.shtml
## https://logon7.gov.bc.ca/clp-cgi/capBceid/logon.cgi?flags=1111:1,8&TARGET=$SM$https%3a%2f%2fapps%2enrs%2egov%2ebc%2eca%2fext%2fesf%2fsubmissionWelcome%2edo
## https://apps.nrs.gov.bc.ca/ext/esf/submissionWelcome.do?


# fill in soft url here
# https://www.env.gov.bc.ca/perl/soft/dl.pl/20221213184447-07-gp-aac61809-81a7-4ba5-8adc-b836a110?simple=y


##fill in where to check on your submission here (copy and paste url once file uploads)
# https://apps.nrs.gov.bc.ca/ext/esf/submissionSearch.do?action=clear
# user reference: 059_cwf_bulkley_20221213
# submission id: 2214091
