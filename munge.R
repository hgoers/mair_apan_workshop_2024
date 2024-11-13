library(tidyverse)

# Read in the latest version of the US Presidential results
state_results_df <- list.files(path = here::here("data-raw"),
                               pattern = "pres_state_results_.*",
                               full.names = T) |> 
  max() |> 
  read_csv()

# Exploration -------------------------------------------------------------

min_year <- state_results_df |> 
  slice_min(year) |> 
  distinct(year) |> 
  pull()

max_year <- state_results_df |> 
  slice_max(year) |> 
  distinct(year) |> 
  pull()

# Count the number of candidates running annually
n_candidates_annual <- state_results_df |> 
  distinct(year, candidate) |> 
  drop_na() |> 
  count(year)

n_candidates_annual

# Count the number of candidates by party running annually 
party_count_annual <- state_results_df |> 
  distinct(year, party_simplified, candidate) |> 
  drop_na(candidate) |> 
  count(year, party_simplified)

party_count_annual

# Count the number of write-in candidates annually 
# Create a scope data frame
scope_df <- state_results_df |> 
  distinct(year) |> 
  crossing(writein = c(T, F))

write_in_count_annual <- state_results_df |> 
  drop_na(candidate, writein) |> 
  distinct(year, writein, candidate) |> 
  count(year, writein)

# Append your calculations to that scope
write_in_count_annual_full <- scope_df |> 
  left_join(write_in_count_annual, by = join_by(year, writein)) |> 
  mutate(n = replace_na(n, 0))

write_in_count_annual_full

# Count the number of candidates by state
state_count_annual <- state_results_df |> 
  distinct(year, state_po, candidate) |> 
  drop_na(candidate) |> 
  count(year, state_po)

state_count_annual

# Major party analysis ----------------------------------------------------

pres_state_results_df <- state_results_df |> 
  filter(
    # Isolate DEM and REP from other party candidates
    party_simplified %in% c("DEMOCRAT", "REPUBLICAN"),
    # Remove other candidates
    !candidate %in% c("OTHER", NA_character_) 
  ) |>
  # Get the total count of votes cast for other candidates, preserving
  # the total vote count for the state and the candidate
  group_by(year, state, state_po, party_simplified, candidate, totalvotes) |> 
  summarise(candidatevotes = sum(candidatevotes)) |>
  # Always remember to ungroup your data
  ungroup() |> 
  # Calculate the proportion of votes cast in each state won by each 
  # candidate
  mutate(prop_votes = candidatevotes / totalvotes)

pres_state_results_df

# 2020 results
pres_state_results_2020 <- pres_state_results_df |> 
  filter(year == 2020)

pres_state_results_2020

# Proportion won by each candidate in 2020
prop_won_2020 <- pres_state_results_2020 |> 
  # Make sure the order of the parties is consistent
  arrange(state, party_simplified) |> 
  group_by(state) |> 
  mutate(prop_adv = prop_votes - lead(prop_votes)) |>
  ungroup() |> 
  drop_na(prop_adv)

prop_won_2020

# Over time
pres_national_yearly <- pres_state_results_df |> 
  group_by(year, party_simplified) |> 
  summarise(candidatevotes = sum(candidatevotes),
            totalvotes = sum(totalvotes),
            prop = candidatevotes / totalvotes) |> 
  ungroup()

pres_national_yearly

pres_adv_states <- pres_state_results_df |> 
  pivot_wider(names_from = party_simplified, values_from = prop_votes) |> 
  group_by(year, state, state_po) |> 
  summarise(DEMOCRAT = sum(DEMOCRAT, na.rm = T),
            REPUBLICAN = sum(REPUBLICAN, na.rm = T)) |> 
  ungroup()

pres_adv_states

swing_states_2024 <- c("AZ", "GA", "MI", "NV", "NC", "PA", "WI")
swing_states_2024
