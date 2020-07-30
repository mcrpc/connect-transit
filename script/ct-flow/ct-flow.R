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
  select(1, 18, 19, 37, 38)  # columns with ID, origin & destination coordinates


# get census block geometries (for aggregation) ---------------------------

# this is a ~170MB download, so beware!
countyBlockData <- blocks(
  state = 17,
  county = 113,
  class = "sf"
) %>%
  select("GEOID10")   # keep unique ID, drop all other fields


# convert survey data into sf ---------------------------------------------

originSF <- survey_data %>%
  select(1:3) %>%
  st_as_sf(coords = c(3, 2), crs = "WGS84")

destinationSF <- survey_data %>%
  select(1, 4:5) %>%
  st_as_sf(coords = c(3, 2), crs = "WGS84")


# convert into PCS (otherwise spatial join won't work) --------------------

countyBlockData_PCS <- countyBlockData %>%
  st_transform(crs = 3443)

originSF_PCS <- originSF %>%
  st_transform(crs = 3443)

destinationSF_PCS <- destinationSF %>%
  st_transform(crs = 3443)

# join survey data to census blocks ---------------------------------------

origin_join <- countyBlockData_PCS %>%
  st_join(originSF_PCS, join = st_contains)

# format aggregate flows and push to google sheet -------------------------


