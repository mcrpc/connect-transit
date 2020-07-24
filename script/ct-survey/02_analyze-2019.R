
# profile of riders based on 2019 survey ----------------------------------
# generates figures describing the breakdown of surveyed riders based on 
# demographics and other basic questions
q1Tabyl <- tabyl(survey_2019, q1_main_reason) %>%
  arrange(desc(n)) %>%
  transmute(
    "Reason" = q1_main_reason,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q3Tabyl <- tabyl(survey_2019, q3_payment_method) %>%
  arrange(desc(n)) %>%
  transmute(
    "Payment Method" = q3_payment_method,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q4Tabyl <- tabyl(survey_2019, q4_riding_history) %>%
  arrange(desc(n)) %>%
  transmute(
    "Riding History" = q4_riding_history,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q9Tabyl <- tabyl(survey_2019, q9_race) %>%
  arrange(desc(n)) %>%
  transmute(
    "Race/Ethnicity" = q9_race,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q10Tabyl <- tabyl(survey_2019, q10_employment) %>%
  arrange(desc(n)) %>%
  transmute(
    "Answer" = q10_employment,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q14Tabyl <- tabyl(survey_2019, q14_choice_rider) %>%
  arrange(desc(n)) %>%
  transmute(
    "Answer" = q14_choice_rider,
    "Responses" = n,
    "Percent" = percent(percent)
  )

q15Tabyl <- tabyl(survey_2019, q15_household_income) %>%
  arrange(desc(n)) %>%
  transmute(
    "Household Income" = q15_household_income,
    "Responses" = n,
    "Percent" = percent(percent)
  )




# rider grouping analyses -------------------------------------------------
# detailed analyses of specific groupings of riders e.g. long-term riders
# vs. new riders
# define function to calculate priority rankings --------------------------

prioritize <- function(x, weights = c(3, 2, 1)) {
  require(janitor, magrittr)
  x %>%
    tabyl(q21_first_priority, show_na = FALSE) %>%
    transmute(
      "name" = q21_first_priority,
      "n_first" = n * weights[1]
    ) %>%
    left_join(
      tabyl(x, q22_second_priority, show_na = FALSE) %>%
        transmute(
          "name" = q22_second_priority,
          "n_second" = n * weights[2]
        ),
      by = "name"
    ) %>%
    left_join(
      tabyl(x, q23_third_priority, show_na = FALSE) %>%
        transmute(
          "name" = q23_third_priority,
          "n_third" = n * weights[3]
        ),
      by = "name"
    ) %>%
    replace_na(list(n_first = 0, n_second = 0, n_third = 0)) %>%
    group_by(name) %>%
    summarize(n = sum(n_first, n_second, n_third)) %>%
    arrange(desc(n)) %>%
    rename(
      "Service" = name,
      "Score" = n
    ) %>%
    mutate("Service" = recode(Service, !!!serviceNames))
}

# create tabyls of subcategories ------------------------------------------
studentGroupTabyl <- tabyl(
  filter(survey_2019, q1_group == "Student"),
  q1_main_reason
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Reason" = q1_main_reason,
    "Responses" = n,
    "Percent" = percent(percent)
  )

commuterGroupTabyl <- tabyl(
  filter(survey_2019, q1_group == "Commuter"),
  q1_main_reason
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Reason" = q1_main_reason,
    "Responses" = n,
    "Percent" = percent(percent)
  )

otherGroupTabyl <- tabyl(
  filter(survey_2019, q1_group == "Other"),
  q1_main_reason
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Reason" = q1_main_reason,
    "Responses" = n,
    "Percent" = percent(percent)
  )

riderHistoryTabyl <- tabyl(
  survey_2019,
  q4_riding_history
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Length" = q4_riding_history,
    "Responses" = n,
    "Percent" = percent(percent)
  )

newRiderGroupTabyl <- tabyl(
  filter(survey_2019, q4_group == "New Rider"),
  q4_riding_history
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Length" = q4_riding_history,
    "Responses" = n,
    "Percent" = percent(percent)
  )

oldRiderGroupTabyl <- tabyl(
  filter(survey_2019, q4_group == "Long-term Rider"),
  q4_riding_history
) %>%
  arrange(desc(n)) %>%
  transmute(
    "Length" = q4_riding_history,
    "Responses" = n,
    "Percent" = percent(percent)
  )

# combined graph with q1_group and q4_group
rbind(
  survey_2019 %>%
    group_by(Group = q4_group) %>%
    filter(n() > 10) %>%
    summarize(
      "Reliability of service" = mean(s1_reliability, na.rm = TRUE),
      "Frequency of service" = mean(s2_frequency, na.rm = TRUE),
      "Hours of service" = mean(s5_hours, na.rm = TRUE),
      "Service after 9pm" = mean(s25_after_9, na.rm = TRUE),
      "Mobile App" = mean(s20_mobile_app, na.rm = TRUE),
      "Sunday Service" = mean(s21_sunday, na.rm = TRUE)
    ),
  survey_2019 %>%
    group_by(Group = q1_group) %>%
    filter(n() > 10) %>%
    summarize(
      "Reliability of service" = mean(s1_reliability, na.rm = TRUE),
      "Frequency of service" = mean(s2_frequency, na.rm = TRUE),
      "Hours of service" = mean(s5_hours, na.rm = TRUE),
      "Service after 9pm" = mean(s25_after_9, na.rm = TRUE),
      "Mobile App" = mean(s20_mobile_app, na.rm = TRUE),
      "Sunday Service" = mean(s21_sunday, na.rm = TRUE)
    )
) %>%
  pivot_longer(cols = 2:7, names_to = "Service", values_to = "Satisfaction") %>%
  ggplot(aes(x = Service, y = Satisfaction)) +
  geom_col(aes(fill = Group), position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(values = cols)

survey_2019 %>%
  group_by(Group = q4_group) %>%
  filter(n() > 10) %>%
  summarize(
    "Reliability of service" = mean(s1_reliability, na.rm = TRUE),
    "Frequency of service" = mean(s2_frequency, na.rm = TRUE),
    "Hours of service" = mean(s5_hours, na.rm = TRUE),
    "Service after 9pm" = mean(s25_after_9, na.rm = TRUE),
    "Mobile App" = mean(s20_mobile_app, na.rm = TRUE),
    "Sunday Service" = mean(s21_sunday, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = 2:7, names_to = "Service", values_to = "Satisfaction") %>%
  ggplot(aes(x = Service, y = Satisfaction)) +
  geom_col(aes(fill = Group), position = position_dodge(0.8), width = 0.7) +
  scale_fill_manual(values = cols)


# q3_payment_method -------------------------------------------------------
q3Groups <- list(
  "ISU ID" = "ISU ID",
  "Heartland ID" = "Heartland ID",
  "Cash" = c("Cash", "Change Card"),
  "30-Day Pass" = "30 day pass",
  "BEAM/Senior/Disabled" = c("Circuit Breaker ID", "Senior ID", "Half fare ID"),
  "Other Purchased Pass" = c("1 ride pass", "7 day pass", "1 day pass"),
  "Other ID" = c("Wesleyan ID", "Country Financial ID", "Youthbuild ID")
)

q3_q4GroupTabyl <- survey_2019 %>%
  group_by(Group = q4_group) %>%
  filter(n() > 10) %>%
  mutate(
    "Method" = case_when(
      q3_payment_method %in% q3Groups$`ISU ID` ~ 'ISU ID',
      q3_payment_method %in% q3Groups$`Heartland ID` ~ 'Heartland ID',
      q3_payment_method %in% q3Groups$Cash ~ 'Cash',
      q3_payment_method %in% q3Groups$`30-Day Pass` ~ '30-Day Pass',
      # q3_payment_method %in% q3Groups$`BEAM/Senior/Disabled` ~ 'BEAM/Senior/Disabled',
      # q3_payment_method %in% q3Groups$`Other Purchased Pass` ~ 'Other Purchased Pass',
      # q3_payment_method %in% q3Groups$`Other ID` ~ 'Other ID',
      TRUE ~ 'Other/No Answer'
    )
  ) %>%
  group_by(Group, Method) %>%
  tally(sort = TRUE) %>%
  mutate(Percent = n/sum(n))

q5_q4GroupTabyl <- survey_2019 %>%
  group_by(Group = q4_group) %>%
  filter(n() > 10) %>%
  mutate(
    "Frequency" = case_when(
      q5_per_week_rider %in% c(6, 7) ~ '6-7',
      q5_per_week_rider %in% c(4, 5) ~ '4-5',
      q5_per_week_rider %in% c(2, 3) ~ '2-3',
      q5_per_week_rider %in% c(0, 1) ~ '0-1',
      TRUE ~ 'No Answer'
    )
  ) %>%
  group_by(Group, Frequency) %>%
  tally(sort = TRUE) %>%
  mutate(Percent = n/sum(n)) %>%
  rename(Responses = n)

q16Groups <- list(
  "Still Riding" = "Still be using Connect Transit",
  "Not Riding" = c(
    "Not be using Connect Transit for other reasons",
    "Not be using Connect Transit because I plan to get a car"
  ),
  "Not Sure/No Answer" = "Not sure"
)

q16_q4GroupTabyl <- survey_2019 %>%
  group_by(Group = q4_group) %>%
  filter(n() > 10) %>%
  mutate(
    `Future Ridership` = case_when(
      q16_future_rider %in% q16Groups$`Still Riding` ~ "Still Riding",
      q16_future_rider %in% q16Groups$`Not Riding` ~ "Not Riding",
      q16_future_rider %in% q16Groups$`Not Sure/No Answer` ~ "Not Sure/No Answer",
      TRUE ~ "Not Sure/No Answer"
    )
  ) %>%
  group_by(Group, `Future Ridership`) %>%
  tally(sort = TRUE) %>%
  mutate(Percent = n/sum(n)) %>%
  rename(
    Responses = n
  )
