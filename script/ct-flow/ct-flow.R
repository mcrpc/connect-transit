# processes survey data into format suitable for flowmap.blue
# by Tim Riley

# googlesheets4 auth code:
# 4/2gFS_XRJVuGU5mv6gXPE-bDUj332tMi_Hu8GVhtRTZqH4uSyf6nteNs

# load packages -----------------------------------------------------------

# list packages in a vector so we can load them using one line of code
packages <- c(
  "tidyverse",
  "readxl",
  "here",
  "janitor",
  "tigris",
  "sf",
  "googlesheets4"
)

# load all of the packages
invisible(lapply(packages, library, character.only = TRUE))

# read survey data --------------------------------------------------------

fileName <- "data/2018-06-19_customer-satisfaction-OD-rider-survey.XLSX"

surveyData <- read_xlsx(fileName, 1) %>%   # read first sheet of excel file
  clean_names() %>%       # handy function from janitor which makes life easier
  select(-(176:178)) %>%  # these columns do not exist in the second sheet
  rbind(read_xlsx(fileName, 2) %>% clean_names()) %>%
  select(1, 18, 19, 37, 38)  # columns with ID, origin & destination coordinates


# get census block geometries (for aggregation) ---------------------------

# this is a ~170MB download, so beware!
censusGeometryData <- blocks(
  state = 17,
  county = 113,
  class = "sf"
) %>%
  select("GEOID10")   # keep unique identifier, drop all other fields

# # try using block groups instead
# censusGeometryData <- block_groups(
#   state = 17,
#   county = 113,
#   class = "sf"
# ) %>%
#   select("GEOID")

# convert survey data into sf ---------------------------------------------

originSF <- surveyData %>%
  select(1:3) %>%
  st_as_sf(coords = c(3, 2), crs = "WGS84")

destinationSF <- surveyData %>%
  select(1, 4:5) %>%
  st_as_sf(coords = c(3, 2), crs = "WGS84")


# convert into PCS (otherwise spatial join won't work) --------------------

censusGeometryDataPCS <- censusGeometryData %>%
  st_transform(crs = 3443)

originPCS <- originSF %>%
  st_transform(crs = 3443)

destinationPCS <- destinationSF %>%
  st_transform(crs = 3443)

# join survey data to census blocks ---------------------------------------

originJoin <- censusGeometryDataPCS %>%
  st_join(originPCS, join = st_contains)

destinationJoin <- censusGeometryDataPCS %>%
  st_join(destinationPCS, join = st_contains)

# create a dataframe we will use to update "locations" sheet
locations <- censusGeometryDataPCS %>%
  semi_join(
    tibble(drop_na(originJoin %>% add_row(destinationJoin), id)),
    by = "GEOID10"
  ) %>%
  st_centroid() %>%
  st_transform(4269) %>%
  transmute(
    id = GEOID10,
    name = GEOID10,
    lat = st_coordinates(.)[,2],
    lon = st_coordinates(.)[,1]
  ) %>%
  st_drop_geometry() %>%
  tibble()
  

# format aggregate flows and push to google sheet -------------------------

flows <- select(surveyData, "id") %>%
  right_join(
    select(tibble(originJoin), "id", "origin" = "GEOID10"),
    by = "id"
  ) %>%
  right_join(
    select(tibble(destinationJoin), "id", "dest" = "GEOID10"),
    by = "id"
  ) %>%
  aggregate(
    id ~ origin + dest,
    data = ., FUN = function(x) length(unique(x))
  ) %>%
  rename(count = id)

spreadSheet <- "1HS2xNe-Ewo9EwmIYb3GhJ-3353cfJDXD4DwidC5TjEY"
write_sheet(locations, spreadSheet, 2)
write_sheet(flows, spreadSheet, 3)

# ways to test to make sure locations & flows match up --------------------

## should return TRUE:
# dim(
#   flows %>%
#     select(GEOID10 = origin) %>%
#     add_row(flows %>% select(GEOID10 = dest)) %>%
#     distinct()
# ) == dim(
#   select(locations, GEOID10)
# )

