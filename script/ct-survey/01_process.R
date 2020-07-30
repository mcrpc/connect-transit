packages <- c(
  "tidyverse",
  "janitor",
  "readxl",
  "here",
  "scales",
  "knitr"
  # "showtext",
  # "extrafont"
)

invisible(lapply(packages, library, character.only = TRUE))


# tidy 2018 survey data ---------------------------------------------------
filename <- "data/2018-06-19_customer-satisfaction-OD-rider-survey.XLSX"
survey_2018 <- read_xlsx(filename, 1) %>%
  clean_names() %>%
  select(-(176:178)) %>%
  rbind(read_xlsx(filename, 2) %>% clean_names())

# there is some sort of function for mapping columns from one table to another.
# figure it out and it should make this next step a lot easier
# (will probably have to do something interesting with ethnicity and gender, etc)

# tidy 2019 survey data ---------------------------------------------------
survey_2019 <- read_xls("data/2019-06-27_customer-satisfaction-survey.xls") %>%
  clean_names() %>%
  # the 'other' responses are not terribly informative
  left_join(tribble(
    ~q1_what_is_the_main_reason_you_use_conne, ~"What is the main reason you use Connect Transit?",
    1, "Work",
    2, "Personal Business",
    3, "Shopping",
    4, "Middle School (6-8)",
    5, "High School (9-12)",
    6, "College",
    7, "Hospital/Doctor's Office",
    8, "Social/Recreation",
    9, "Other"
  )) %>%
  left_join(tribble(
    ~q2_will_your_trip_today_include_a_transf, ~"Will your trip today include a transfer?",
    1, "Yes",
    2, "No",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q3_how_do_you_normally_pay_your_fare, ~"How do you normally pay your fare?",
    1, "Cash",
    2, "Change Card",
    3, "1 ride pass",
    4, "1 day pass",
    5, "7 day pass",
    6, "30 day pass",
    7, "ISU ID",
    8, "Heartland ID",
    9, "Wesleyan ID",
    10, "Youthbuild ID",
    11, "Town of Normal ID",
    12, "Country Financial ID",
    13, "Senior ID",
    14, "Circuit Breaker ID",
    15, "Half fare ID",
    99, "No Answer"
  )) %>%
  left_join(tribble(
    ~q4_how_long_have_you_been_riding_transit, ~"How long have you been riding transit?",
    1, "More than 4 years",
    2, "3 to 4 years",
    3, "1 to 2 years",
    4, "Less than 1 year",
    5, "First time",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q5_how_many_days_do_you_use_connect_tran, ~"In a typical week, how many days do you use Connect Transit?",
    1, 1,
    2, 2,
    3, 3,
    4, 4,
    5, 5,
    6, 6,
    7, 7,
    99, 0
  )) %>%
  left_join(tribble(
    ~q6_what_is_your_gender, ~"What is your gender?",
    1, "Male",
    2, "Female",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q7_what_is_your_age, ~"What is your AGE?",
    1, "Under 18",
    2, "18-24",
    3, "25-34",
    4, "35-44",
    5, "45-54",
    6, "55-64",
    7, "65+",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q8_do_you_have_a_valid_drivers_license, ~"Do you have a valid driver's license?",
    1, "Yes",
    2, "No",
    9, "No Answer"
  )) %>%
  # combine the separate columns per possible response into to one column
  mutate_at(15:22, as.character) %>%
  unite("How do you identify your race/ethnicity?", 15:22, na.rm = TRUE, remove = FALSE) %>%
  select(-15, everything()) %>%
  left_join(tribble(
    ~q10_are_you, ~"Are you: [Employment status]?",
    1, "Employed full-time",
    2, "Employed part-time",
    3, "Unemployed",
    4, "Student (K-12)",
    5, "Student (College)",
    6, "Homemaker",
    7, "Retired",
    8, "Disabled",
    9, "No Answer"
  )) %>%
  # may need to un-code this field if it needs to be numeric
  # shout-out to whomever has 14 people in their household, though
  left_join(tribble(
    ~q11_including_yourself_how_many_people, ~"Including YOU, how many people live in your household?",
    1, "1",
    2, "2",
    3, "3",
    4, "4",
    5, "5 or more",
    6, "5 or more",
    7, "5 or more",
    8, "5 or more",
    9, "5 or more",
    14, "5 or more"
  )) %>%
  # may need to un-code this field if it needs to be numeric
  left_join(tribble(
    ~q12_including_yourself_how_many_people, ~"Including YOU, how many people (16 or older) in your household are employed full/part-time?",
    0, "0",
    1, "1",
    2, "2",
    3, "3",
    4, "4 or more",
    5, "4 or more",
    6, "4 or more"
  )) %>%
  # may need to un-code this field if it needs to be numeric
  left_join(tribble(
    ~q13_how_many_working_vehicles_cars_tru, ~"How many working vehicles (cars, trucks, or motorcycles) are available to your household?",
    0, "0",
    1, "1",
    2, "2",
    3, "3",
    4, "4 or more",
    5, "4 or more",
    6, "4 or more",
    7, "4 or more",
    8, "4 or more",
    9,  "4 or more"
  )) %>%
  left_join(tribble(
    ~q13a_could_you_have_used_one_of_these_ve, ~"[If #13 is ONE OR MORE] Could you have used one of these vehicles for this trip?",
    1, "Yes",
    2, "No",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q14_which_of_the_following_best_describe, ~"Which of the following BEST describes your TOTAL ANNUAL HOUSEHOLD INCOME in 2018 before taxes?",
    1, "Less than $15,000",
    2, "$15,000 - $24,999",
    3, "$25,000 - $34,999",
    4, "$35,000 - $49,999",
    5, "$50,000 - $74,999",
    6, "$75,000 - $99,999",
    7, "$100,000 - $149,999",
    8, "$150,000 or more",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q15_one_year_from_now_i_will, ~"Finish this statement: One year from now, I will...",
    1, "Still be using Connect Transit",
    2, "Not be using Connect Transit for other reasons",
    3, "Not be using Connect Transit because I plan to get a car",
    9, "Not sure"
  )) %>%
  left_join(tribble(
    ~q16_how_many_days_per_week_do_you_use_ub, ~"How many days per week do you use Uber, Lyft, or a Taxi?",
    1, 1,
    2, 2,
    3, 3,
    4, 4,
    5, 5,
    6, 6,
    7, 7,
    99, 0
  )) %>%
  left_join(tribble(
    ~q17_where_do_you_get_most_of_your_route, ~"Where do you get most of your route and schedule information?",
    1, "Printed schedules",
    2, "At bus stops",
    3, "Call Connect",
    4, "Connect's website",
    5, "Mobile App",
    9, "No Answer"
  )) %>%
  left_join(tribble(
    ~q18_how_would_you_prefer_to_pay_connect, ~"How would you prefer to pay Connect bus fares?",
    1, "Cash at the bus",
    2, "Passes",
    3, "Smartphone/Smartphone app",
    9, "No Answer"
  )) %>%
  # combine the separate columns per possible response into to one column
  mutate_at(34:38, as.character) %>%
  unite("How would you prefer to receive information about route changes? (Select up to two choices)", 34:38, na.rm = TRUE, remove = FALSE) %>%
  select(-34, everything()) %>%
  mutate(
    "1. Reliability of service (buses are on time)" = na_if(q20_01_01_reliability_of_service_bu, 9),
    "2. Frequency of service" = na_if(q20_02_02_frequency_of_service, 9),
    "3. Ease of bus connections/transfers" = na_if(q20_03_03_ease_of_bus_connections_tr, 9),
    "4. Saturday service" = na_if(q20_04_04_saturday_service, 9),
    "5. Hours of service (evenings, early morning)" = na_if(q20_05_05_hours_of_service_evenings, 9),
    "6. Bus stop safety" = na_if(q20_06_06_bus_stop_safety, 9),
    "7. Bus stop amenities (lighting, shelters, benches)" = na_if(q20_07_07_bus_stop_amenities_lighti, 9),
    "8. Bus cleanliness" = na_if(q20_08_08_bus_cleanliness, 9),
    "9. Onboard safety/security" = na_if(q20_09_09_onboard_safety_security, 9),
    "10. Comfort of buses" = na_if(q20_10_10_comfort_of_buses, 9),
    "11. Bus fleet/equipment" = na_if(q20_11_11_bus_fleet_equipment, 9),
    "12. Driver courtesy" = na_if(q20_12_12_driver_courtesy, 9),
    "13. Driver safety" = na_if(q20_13_13_driver_safety, 9),
    "14. Fare/cost to ride" = na_if(q20_14_14_fare_cost_to_ride, 9),
    "15. Convenience of purchasing a bus pass" = na_if(q20_15_15_convenience_of_purchasing, 9),
    "16. Easy-to-understand route information" = na_if(q20_16_16_easy_to_understand_route_i, 9),
    "17. Printed schedules/system map" = na_if(q20_17_17_printed_schedules_system_m, 9),
    "18. Connect website" = na_if(q20_18_18_connect_website, 9),
    "19. Connect social media (ie Facebook, Twitter)" = na_if(q20_19_19_connect_social_media_i_e, 9),
    "20. Connect's mobile app" = na_if(q20_20_20_connects_mobile_app, 9),
    "21. Sunday service" = na_if(q20_21_21_sunday_service, 9),
    "22. Service before 6am in the morning" = na_if(q20_22_22_service_before_6am_in_the, 9),
    "23. Shelters at bus stops" = na_if(q20_23_23_shelters_at_bus_stops, 9),
    "24. Convenient transfers between routes" = na_if(q20_24_24_convenient_transfers_betwe, 9),
    "25. Service after 9pm in the evening" = na_if(q20_25_25_service_after_9pm_in_the_e, 9),
    "Which is most important? - First" = as.character(na_if(q20a_01_1st, 99)),
    "Which is most important? - Second" = as.character(q20a_02_2nd),
    "Which is most important? - Third" = as.character(q20a_03_3rd)
  ) %>%
  mutate(route_direction = word(route_with_direction, 2)) %>%
  select(c(1, 2, 116, 68:115))




# columns 12 and 23 need to be re-coded specially -------------------------
# "check all that apply" questions are the absolute worst
survey_2019[12][survey_2019[12] == ""] <- "-9"
survey_2019[[12]] <- recode(
  survey_2019[[12]],
  "-9" = "No Answer",
  "1" = "White",
  "1_5" = "White, Hispanic/Latino",
  "2" = "Native Hawaiian/Pacific Islander",
  "3" = "Black",
  "4" = "Native American",
  "5" = "Hispanic/Latino",
  "6" = "Asian",
  .default = "Mixed/Other"
)

survey_2019[23][survey_2019[23] == ""] <- "-9"
survey_2019[[23]] <- recode(
  survey_2019[[23]],
  "-9" = "No Answer",
  "1" = "Website",
  "2" = "App",
  "1_2" = "Website/App",
  "4" = "Facebook",
  "3" = "Twitter",
  "5" = "Other",
  "2_4" = "App/Facebook",
  "2_3" = "App/Twitter",
  "2_5" = "App",
  "1_4" = "Website/Facebook",
  "1_5" = "Website",
  "3_4" = "Facebook/Twitter",
  "1_3" = "Website/Twitter",
  "4_5" = "Facebook",
  .default = "No Answer"
)

survey_2019[[49]] <- recode(
  survey_2019[[49]],
  "1" = "s1_reliability",
  "2" = "s2_frequency",
  "3" = "s3_connectivity",
  "4" = "s4_saturday",
  "5" = "s5_hours",
  "6" = "s6_stop_safety",
  "7" = "s7_amenities",
  "8" = "s8_cleanliness",
  "9" = "s9_bus_safety",
  "10" = "s10_bus_comfort",
  "11" = "s11_bus_equipment",
  "12" = "s12_driver_courtesy",
  "13" = "s13_driver_safety",
  "14" = "s14_fare_price",
  "15" = "s15_fare_convenience",
  "16" = "s16_route_info",
  "17" = "s17_printed_info",
  "18" = "s18_website",
  "19" = "s19_social_media",
  "20" = "s20_mobile_app",
  "21" = "s21_sunday",
  "22" = "s22_before_6",
  "23" = "s23_shelters",
  "24" = "s24_convenient_transfers",
  "25" = "s25_after_9"
  )

survey_2019[[50]] <- recode(
  survey_2019[[50]],
  "1" = "s1_reliability",
  "2" = "s2_frequency",
  "3" = "s3_connectivity",
  "4" = "s4_saturday",
  "5" = "s5_hours",
  "6" = "s6_stop_safety",
  "7" = "s7_amenities",
  "8" = "s8_cleanliness",
  "9" = "s9_bus_safety",
  "10" = "s10_bus_comfort",
  "11" = "s11_bus_equipment",
  "12" = "s12_driver_courtesy",
  "13" = "s13_driver_safety",
  "14" = "s14_fare_price",
  "15" = "s15_fare_convenience",
  "16" = "s16_route_info",
  "17" = "s17_printed_info",
  "18" = "s18_website",
  "19" = "s19_social_media",
  "20" = "s20_mobile_app",
  "21" = "s21_sunday",
  "22" = "s22_before_6",
  "23" = "s23_shelters",
  "24" = "s24_convenient_transfers",
  "25" = "s25_after_9"
)

survey_2019[[51]] <- recode(
  survey_2019[[51]],
  "1" = "s1_reliability",
  "2" = "s2_frequency",
  "3" = "s3_connectivity",
  "4" = "s4_saturday",
  "5" = "s5_hours",
  "6" = "s6_stop_safety",
  "7" = "s7_amenities",
  "8" = "s8_cleanliness",
  "9" = "s9_bus_safety",
  "10" = "s10_bus_comfort",
  "11" = "s11_bus_equipment",
  "12" = "s12_driver_courtesy",
  "13" = "s13_driver_safety",
  "14" = "s14_fare_price",
  "15" = "s15_fare_convenience",
  "16" = "s16_route_info",
  "17" = "s17_printed_info",
  "18" = "s18_website",
  "19" = "s19_social_media",
  "20" = "s20_mobile_app",
  "21" = "s21_sunday",
  "22" = "s22_before_6",
  "23" = "s23_shelters",
  "24" = "s24_convenient_transfers",
  "25" = "s25_after_9"
)

# replace column names using a long vector of names -----------------------
# because janitor::clean_names() can only do so much

shortNames <- c(
  "id",
  "route",
  "route_dir",
  "q1_main_reason",
  "q2_transfer",
  "q3_payment_method",
  "q4_riding_history",
  "q5_per_week_rider",
  "q6_gender",
  "q7_age",
  "q8_license",
  "q9_race",
  "q10_employment",
  "q11_household_size",
  "q12_household_employment",
  "q13_household_vehicles",
  "q14_choice_rider",
  "q15_household_income",
  "q16_future_rider",
  "q17_per_week_uber",
  "q18_info_source",
  "q19_payment_prefer",
  "q20_info_prefer",
  "s1_reliability",
  "s2_frequency",
  "s3_connectivity",
  "s4_saturday",
  "s5_hours",
  "s6_stop_safety",
  "s7_amenities",
  "s8_cleanliness",
  "s9_bus_safety",
  "s10_bus_comfort",
  "s11_bus_equipment",
  "s12_driver_courtesy",
  "s13_driver_safety",
  "s14_fare_price",
  "s15_fare_convenience",
  "s16_route_info",
  "s17_printed_info",
  "s18_website",
  "s19_social_media",
  "s20_mobile_app",
  "s21_sunday",
  "s22_before_6",
  "s23_shelters",
  "s24_convenient_transfers",
  "s25_after_9",
  "q21_first_priority",
  "q22_second_priority",
  "q23_third_priority"
)

names(survey_2019) <- shortNames

# define verbose labels for columns s1-s25
serviceNames <- c(
  "s1_reliability" = "1. Reliability of Service",
  "s2_frequency" = "2. Frequency of Service",
  "s3_connectivity" = "3. Ease of Transfers",
  "s4_saturday" = "4. Saturday Service",
  "s5_hours" = "5. Hours of Service",
  "s6_stop_safety" = "6. Bus Stop Safety",
  "s7_amenities" = "7. Bus Stop Amenities",
  "s8_cleanliness" = "8. Bus Cleanliness",
  "s9_bus_safety" = "9. Onboard Safety/Security",
  "s10_bus_comfort" = "10. Comfort of Buses",
  "s11_bus_equipment" = "11. Bus Fleet/Equipment",
  "s12_driver_courtesy" = "12. Driver Courtesy",
  "s13_driver_safety" = "13. Driver Safety",
  "s14_fare_price" = "14. Fare/Cost to Ride",
  "s15_fare_convenience" = "15. Convenience of Purchasing a Bus Pass",
  "s16_route_info" = "16. Easy-to-understand Route Information",
  "s17_printed_info" = "17. Printed Schedules/System Map",
  "s18_website" = "18. Connect Website",
  "s19_social_media" = "19. Connect Social Media",
  "s20_mobile_app" = "20. Connect Mobile App",
  "s21_sunday" = "21. Sunday Service",
  "s22_before_6" = "22. Service before 6 A.M.",
  "s23_shelters" = "23. Shelters at Bus Stops",
  "s24_convenient_transfers" = "24. Convenient Transfers between Routes",
  "s25_after_9" = "25. Service after 9 P.M."
)

# define responses which we will use to create comparison groups ----------

q1Groups <- list(
  "student" = c("College", "High School (9-12)", "Middle School (6-8)"),
  "commuter" = c("Work", "Personal Business"),
  "other" = c("Shopping", "Hospital/Doctor's Office", "Social/Recreation", "Other")
)
q4Groups <- list(
  "newRider" = c("Less than 1 year", "First time"),
  "oldRider" = c("1 to 2 years", "3 to 4 years", "More than 4 years")
)


# define groups in survey -------------------------------------------------
survey_2019 <- survey_2019 %>% 
  mutate(
    q1_group = case_when(
      q1_main_reason %in% q1Groups$student ~ "Student",
      q1_main_reason %in% q1Groups$commuter ~ "Commuter",
      q1_main_reason %in% q1Groups$other ~ "Other"
    )
  ) %>%
  mutate(
    q4_group = case_when(
      q4_riding_history %in% q4Groups$newRider ~ "New Rider",
      q4_riding_history %in% q4Groups$oldRider ~ "Long-term Rider",
      TRUE ~ "No Answer"
    )
  )

# write to CSV for backup -------------------------------------------------
# write_csv(survey_2019, "data/2019-06-27_customer-satisfaction-survey_tidy.csv", na = "")
