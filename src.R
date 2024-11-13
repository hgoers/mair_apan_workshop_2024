library(tidyverse)
library(dataverse)

# Collect the latest version of MIT Election Lab's Presidential state-level
# returns

state_results_df <- get_dataframe_by_name(
  filename = "1976-2020-president.tab",
  dataset = "10.7910/DVN/42MVDX",
  server = "dataverse.harvard.edu",
  original = T,
  .f = readr::read_csv
)

write_csv(state_results_df, 
          here::here("data-raw", glue::glue("pres_state_results_{today()}.csv")))