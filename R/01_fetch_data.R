# 01_fetch_data.R
# Pulls adverse event reaction counts for 4 statins from the openFDA API

library(httr2)
library(dplyr)

statins <- c("ATORVASTATIN", "SIMVASTATIN", "ROSUVASTATIN", "PRAVASTATIN")

# Make sure the output folder exists (does nothing if it already does)
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# API key is stored privately in .Renviron (NOT in this script, so it never goes to GitHub)
api_key <- Sys.getenv("FDA_API_KEY")
if (api_key == "") stop("FDA_API_KEY not found. Add it to .Renviron and restart R.")

# Helper: send a request to openFDA and return the parsed result
fetch_fda <- function(params) {
  request("https://api.fda.gov/drug/event.json") |>
    req_url_query(api_key = api_key, !!!params) |>
    req_user_agent("statin-adverse-events (student project)") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)
}

# 1. Top 1000 reactions reported for one drug
get_reactions <- function(drug) {
  result <- fetch_fda(list(
    search = paste0('patient.drug.openfda.generic_name:"', drug, '"'),
    count  = "patient.reaction.reactionmeddrapt.exact",
    limit  = 1000
  ))$results
  result$drug <- drug
  Sys.sleep(0.5)  # pause between requests to be polite to the API
  result
}

# 2. Total number of reports for one drug
get_total <- function(drug) {
  total <- fetch_fda(list(
    search = paste0('patient.drug.openfda.generic_name:"', drug, '"'),
    limit  = 1
  ))$meta$results$total
  Sys.sleep(0.5)
  data.frame(drug = drug, total_reports = total)
}

# 3. Run for all 4 statins and combine
reactions <- bind_rows(lapply(statins, get_reactions))
totals    <- bind_rows(lapply(statins, get_total))

# 4. Save raw data
write.csv(reactions, "data/raw/statin_reactions.csv", row.names = FALSE)
write.csv(totals,    "data/raw/statin_totals.csv",    row.names = FALSE)

# 5. Quick check
print(totals)
print(head(reactions))
