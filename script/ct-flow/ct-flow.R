# processes survey data into format suitable for flowmap.blue
# by Tim Riley



# load packages -----------------------------------------------------------

# list packages in a vector so we can load them using one line of code
packages <- c(
  "tidyverse",
  "readxl",
  "here",
  "janitor",
  "tigris",
  "sf"
)

# load all of the packages
invisible(lapply(packages, library, character.only = TRUE))

# read survey data --------------------------------------------------------

filename <- "data/2018-06-19_customer-satisfaction-OD-rider-survey.XLSX"

survey_data <- read_xlsx(filename, 1) %>%   # read first sheet of excel file
  clean_names() %>%       # handy function from janitor which makes life easier
  select(-(176:178)) %>%  # these columns do not exist in the second sheet
  rbind(read_xlsx(filename, 2) %>% clean_names()) %>%
  select(18, 19, 37, 38)  # columns with origin & destination coordinates


# get census block geometries (for aggregation) ---------------------------

# this is a ~170MB download, so beware!
countyBlockData <- blocks(
  state = 17,
  county = 113,
  class = "sf"
) %>%
  select("GEOID10")   # keep unique ID, drop all other fields


