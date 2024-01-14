# Introduction ------------------------------------------------------------

# Purpose: Extract information relevant to the youth dashboard from OPs
# Author: Arati Krishnamoorthy
# Version Control:

# Oct 2023
#   modify search pattern for Key Issues Youth section to "Young and Emerging leaders"
#   modify code for separating out the Key Issues Youth section to account for 4 instances:
#      youth section exists, followed by HL Health section
#      youth section exists, followed by a section other than health
#      no youth section, health section exists
#      neither youth nor health sections exist.
#   use file_ids to retrieve files instead of drive_ls with recursive = true
# Sept 2023
#   update to be able to upload multiple years
#   update to install textreadr from the archives in CRAN
#   update to read nested subfolders in google drive
# July 2023  
#   restyle per tidyverse style guide 
#   remove reference to magrittr as it is included in tidyverse
#   replace the magrittr pipe with the base R pipe |> 
#   use pacman to load packages as it does not require separate install and can load from github as well
#   use rio package for import/export
#   use across to perform operations across multiple columns
#   use if_all with filter
#   use count instead of group_by and summarize(n())
#   combine consecutive filter commands

# Run the code chunk below JUST ONCE to install textreadr -----------------------------------------------------------

remotes::install_version(package = "Rtools", version = "4.2")

textreadr_url <- 'https://cran.r-project.org/src/contrib/Archive/textreadr/textreadr_1.2.0.tar.gz'
install.packages(textreadr_url, type="source", repos=NULL)  

install.packages("devtools")
library(devtools)
install_version("textreadr", version = "1.2.0")

# Load packages -----------------------------------------------------------

pacman::p_load("tidyverse",
               "rio",        # import/export
               "here",       # relative file paths
              "googledrive", # import from a googledrive
              "textreadr"   # read the OP documents
)    


# RUN ONCE Test case for textreadr ------------------------------------------------

country_filename = "India Mod 2_23_2023 FY 2022 Full OP Report Approved.docx"
complete_op <- textreadr::read_docx(here("data/raw_data/OP", country_filename))
# check if complete_op is a char vector of 5076 characters


# Import data - download current OP files -------------------------------------------------------------

op_folder = "1auNcM4JrFdwl5i17LlSdtdwfBnf7Pc1s" #id of folder with FY2022 approved OPs

# Note that you will need to authorize access to your google account in the console after you run the next code statement

# use file_ids to retrieve files instead of drive_ls with recursive = true
# In order to access the sub folders, we first need to get their names using the drive_ls function
folder_details <- function(folder_id){drive_ls(as_id(folder_id))}
subfolders_level1 <- folder_details(op_folder)
subfolders_level2 <- map_dfr(subfolders_level1 |> select("id") |> pull(), 
                             folder_details)

# list name of all files in all the region_id by calling filelist for each region_id
country_filedetails_all <- map_dfr(subfolders_level2 |> select("id") |> pull(), 
                               folder_details) 
 
country_filedetails <- country_filedetails_all |> 
    filter(str_detect(name , 
                      regex("Full OP Report Submitted|Full OP Report Approved|Full OP Report open", 
                            ignore_case=TRUE)
                      )) 

country_filedetails_all |> print(n=300)
country_filedetails |> print(n=300)

# TODO replace the x,y,z below with row numbers of OPs that you do not need
country_filedetails <- country_filedetails |> 
                        slice(-x,
                              -y,
                              -z) 

country_filedetails |>  print(n=100)

country_fileid <- country_filedetails['id'] |> pull()
country_filename <- country_filedetails['name'] |> pull()
country_filename

op_download = function(id, name) {
  drive_download(as_id(id), 
                 path = here("data/raw_data/OP",name),
               # type = drive_mime_type("docx"),
                 overwrite  = TRUE)}
  
# download all files to the youth/data/raw_data/OP folder
walk2(country_fileid,
      country_filename,
      \(id, name) op_download(id, name))

# Import data: Read OP documents ---------------------------------------

# function to read in each country OP

op_reader = function(country_filename) {
  
    complete_op <- textreadr::read_docx(here("data/raw_data/OP", country_filename))
    
    # Find the start and end of the implementing mechanism summaries using keywords
    imstart <- complete_op  |>  str_which(pattern = "IMPLEMENTING MECHANISM SUMMARY")
    imend <- complete_op |> str_which(pattern = "KEY ISSUE, SPSD, and PROGRAM SUMMARY")
    
    # Split the OP document 
    op_part1 <- complete_op[1:(imstart-1)]
    op_part2 <- complete_op[imstart:(imend-1)]
    op_part3 <- complete_op[imend:length(complete_op)]
    
    op_part2 <- op_part2 |> 
      str_c(collapse = "@")
    
    im_details <- op_part2 |> 
      str_split(pattern = "IM [:digit:]{5,6}:", simplify = TRUE)
    
    im_details <- as_tibble(im_details[-1])
    
    # split IM details into separate columns
    
    im_details <- im_details |> 
      
      separate(value,
               c("im_info", "im_narrative", "im_funding"), 
               sep = "IMPLEMENTING MECHANISM NARRATIVE|FUNDING SUMMARY") |> 
      
      separate(im_info,
               c("im_heading","im_table"),
               sep = "Mechanism Number")  |> 
      
      select(-im_heading)  |> 
      
      separate(im_table,
               c("im_number",
                 "im_Name",
                 "prime_partner",
                 "award_number",
                 "im_type",
                 "source_agency",
                 "im_agency",
                 "planned_funding",
                 "start_date",
                 "end_date",
                 "total_estimated_cost"
                 ),
               sep = "Implementing Mechanism Name|Prime Partner|Award Number|Implementing Mechanism Type|Source Agency|Implementing Agency|Planned Funding|Start Date|End Date|Total Estimated Cost", 
               convert = TRUE)  |>
      
      #split im_funding here
      
      mutate(across(.cols = im_number:im_funding, 
                    .fns = \(x) str_remove_all(x, "@|:@"))
             ) |> 
      
      mutate(im_narrative = str_squish(im_narrative))  |> 
      
      mutate(im_narrative = str_remove_all(im_narrative,"â€™"))  |>   
      
      mutate(country = str_remove(country_filename," Full OP Report Approved.docx"),
             youth_search = ifelse(
               str_detect(im_narrative,
                          regex("youth|adolescent|adolescents|adolescence|young people|10-14|15-19|20-24|25-29|10-19|20-29|15-24|15-29",
                                ignore_case = TRUE
                                )
                          ),
               "Y",
               NA
               ),
             
             #Assign sector or program area categories
             #Health sector is broken down into program areas
             #Economic growth shown here does not include agriculture, energy, workforce development or environment/climate as these are listed separately
             fp.rh = ifelse(str_detect(im_funding, "HL\\.7\\."), "Y", NA),
             
             peace.security = ifelse(str_detect(im_funding, "PS\\."), "Y", NA),
             
             democracy.humanrights.gov = ifelse(str_detect(im_funding, "DR\\."), "Y", NA),
             
             hivaids = ifelse(str_detect(im_funding, "HL\\.1\\."), "Y", NA),
             
             tuberculosis = ifelse(str_detect(im_funding,"HL\\.2\\."), "Y", NA),
             
             malaria = ifelse(str_detect(im_funding,"HL\\.3\\."), "Y", NA),
             
             healthsecurity = ifelse(str_detect(im_funding,"HL\\.4\\."), "Y", NA),
             
             otherpublichealth = ifelse(str_detect(im_funding,"HL\\.5\\."), "Y", NA),
             
             mcnh = ifelse(str_detect(im_funding,"HL\\.6\\."), "Y", NA),
  
             wash = ifelse(str_detect(im_funding,"HL\\.8\\."), "Y", NA),
             
             nutrition = ifelse(str_detect(im_funding,"HL\\.9\\."), "Y", NA),
             
             education = ifelse(str_detect(im_funding,"ES\\.1\\.|ES\\.2\\."), "Y", NA),
             
             socialservices = ifelse(str_detect(im_funding,"ES\\.3\\."), "Y", NA),
             
             eco.growth = ifelse(str_detect(im_funding,"EG\\."), "Y", NA),
             
             agriculture = ifelse(str_detect(im_funding,"EG\\.3\\."), "Y", NA),
             
             energy = ifelse(str_detect(im_funding,"EG\\.7\\.|EG\\.12\\."), "Y", NA),
             
             workforce = ifelse(str_detect(im_funding,"EG\\.6\\."), "Y", NA),
             
             environ = ifelse(str_detect(im_funding,"EG\\.10\\.|EG\\.11\\.|EG\\.12\\."), "Y", NA),
             
             human.assis = ifelse(str_detect(im_funding, "HA\\."), "Y", NA),
             
      )  |> 
      
      separate(country, c("country_date","op_year"), sep = "(?=FY)") |> 
      separate(op_year, c("op_year",NA), sep = "Full OP Report") |>
      separate(country_date, c("country", "op_date"), sep = "(?=\\d{1,2}_\\d{1,2}_\\d{4} )", extra = "merge")  |> 
      mutate(country = str_trim(country),
             op_year = str_trim(op_year),
             op_date = str_trim(op_date)) |> 
      glimpse()
    
    
    # op_part3 <- complete_op[imend:length(complete_op)]
    
    op_part3 <- op_part3 |> 
      str_c(collapse = " ")
    
    op_part3 <- op_part3 |> 
      str_split(pattern = "Young and Emerging Leaders", n = 2,simplify = TRUE) 
    
    op_part3 <- op_part3[2] # youth section and beyond from the key issues narrative
                            # if there is no youth section or it is named different from the pattern above, this will be empty
    
    op_part3 <- op_part3 |> 
      str_split(pattern = "HL Health", n = 2, simplify = TRUE) 
    # HL Health is the section that follows youth
    # if there is no health section or it is named different from the pattern above, 
    #   the second part of the split will be empty
    
   
    if ((op_part3[2])=="") {
      # if there is no HL Health section in key issues then the parsing of the Key Issue Young and Emerging leaders section is suspect and needs manual intervention

      youth_im <- im_details |> 
                    select(im_number) |> 
                    mutate(youth_keyissue_details = "check",
                           youth_keyissue_funding = "check",
                           youth_keyissue = "check")
      } else {
      
      op_part3 <- op_part3[1] #take only the youth section and remove HL Health section
    
      op_part3 <- op_part3 |> 
        str_split(pattern = "SPSD Account MO Program Funding ",n = 2, simplify = TRUE)
    
      youth_narrative <- op_part3[1]
    
      youth_im <- op_part3[2]
    
      youth_im <- youth_im |> 
        str_split(pattern = "(?<=.)(?=IM#[:space:][:digit:]{5,6}:)")  |> 
        unlist()
    
      #the expression in pattern is a look ahead which uses IM#[:space:][:digit:]{5,6}: as the pattern to split and adds it to the right side of the split
      youth_im <- youth_im |> 
        as_tibble()
    
      youth_im <- youth_im |> 
        separate(value, 
                 c("drop","im_number","im_details_youth"), 
                 sep="(?<=IM# |:)", 
                 extra = "merge" ,
                 fill = "right") |> 
        select(-drop)  |> 
        mutate(im_number = str_squish(im_number),
               im_number = str_remove(im_number, pattern=":"),
               im_details_youth = str_squish(im_details_youth),
               youth_keyissue = "Y" )  |> 
        separate(im_details_youth, 
                 c("youth_keyissue_details", "youth_keyissue_funding"),
                 sep = "SUBTOTAL FOR IM# [[:digit:]]{5,6}",
                 extra = "merge",
                 fill = "right")  |> 
        separate(youth_keyissue_funding, 
                 c("youth_keyissue_funding","drop"),
                 sep = " TOTAL ", 
                 extra = "merge", 
                 fill = "right") |> 
        select(-drop)
    
    } 
    
    im_details <- im_details |> 
      left_join(youth_im, by = "im_number")
    
    im_details <- im_details |> 
      mutate(youth = coalesce(youth_keyissue, youth_search) ) 
    
    im_details |> 
      export(file = here("data/wrangled_data", str_c(country_filename,"_im.csv")))
    
    return(im_details)
  }
  
# run the function for all country OPs
allIM_current <- map_dfr(.x = country_filename, .f = op_reader)

# if the function fails for a country OP it is probably because it is not formatted in the standard manner.
# if this is the case, the formatting for the country OP needs to be manually changed in the .docx
# For 2021, this was the case for the nepal and asia regional OPs; all others were in the standard format.


# Merge historical and current IM files --------------------------------------------------------

# update the FY in the next code statement to reflect the currently available OPs
export(allIM_current, file = here("data/wrangled_data", "allIM_FY2022.csv"))

allIM_historical <- import(file = here("data/wrangled_data", "allIM_historical.csv"))
allIM_historical <- allIM_historical |> 
  mutate(im_number = as.character(im_number))

allIM <- bind_rows(allIM_historical, allIM_current)
export(allIM, file = here("data/wrangled_data", "allIM.csv"))

# Data exploration --------------------------------------------------------

# compare number of youth activities per country and per sector/program area
  
no_youth_per_country <- allIM |>
    filter(youth == "Y") |>
    group_by(country) |>
    summarize(no_youth = n()) |> # can use count() instead  
    arrange(desc(no_youth)) |>
    print(n=nrow(allIM))
  
function_youth_per_sector = function(col_name) {
  value <- allIM |>
      select(youth, all_of(col_name)) |>
      filter(youth == "Y",
             get({{ col_name }}) == "Y") |> # explore using := with the double curly bracket
      # print(n = 311) |>
      count()
      return(value)
  }
  
sectors <- allIM |>
    select(fp.rh:human.assis) |>
    colnames()
  
no_youth_per_sector <- tibble(sectors, map_dfr(.x = sectors, .f = function_youth_per_sector))
  
no_youth_per_sector |>
    arrange(desc(n))|> 
    print()

# search for "child marriage" and "menstrual"
  
IM <- import(file = here("data\raw_data", "allIMmanual.csv"), col_names = TRUE)
  
IM <- IM |> 
    # mutate(youth.manualwithreview = coalesce(Review.1,youth.manual))  |> 
    # drop_na(youth.manualwithreview)  |> 
    # mutate(youth.manualwithreview =factor(x=youth.manualwithreview)) 
    select(-youth.manual,
           -Review.1, 
           -Review.1.Comments,
           -youth.manualwithreview
           ) |> 
    # remove â€™
    mutate(im_narrative = str_remove_all(im_narrative,"â€™"))
  
IM <- IM |>
  mutate(
    childmarriage = ifelse(
      str_detect(im_narrative, regex("child marriage|child marriages", ignore_case = TRUE)), "Y", NA
      ),
    menstrual = ifelse(
      str_detect(im_narrative, regex("menstrual", ignore_case = TRUE)), "Y", NA
      )
    )

export(IM, file = here("data/wrangled_data", "IM_childmarriage_menstrual.csv"))

    