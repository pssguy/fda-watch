# ============================================================
# FDA Watch — Drug Adverse Events Data Pipeline
# ============================================================
# This script pulls drug adverse event data from the openFDA API,
# processes it into JSON summaries ready for the chat app.
#
# No API key needed for basic use (limited to 40 requests/min).
# With a free key from https://open.fda.gov/apis/authentication/
# you get 240 requests/min.
#
# Packages needed (install once):
#   install.packages(c("httr2", "jsonlite", "dplyr", "purrr", "lubridate"))
# ============================================================

library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(lubridate)

# ── Configuration ──
# Set your API key here (or leave blank for keyless access)
API_KEY <- Sys.getenv("FDA_API_KEY") # Get one free at https://open.fda.gov/apis/authentication/
BASE_URL <- "https://api.fda.gov"
OUTPUT_DIR <- "data"  # Where JSON files go — this folder feeds the app

dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ── Helper: Call the openFDA API ──
fda_query <- function(endpoint, search = NULL, count = NULL, limit = NULL) {
  url <- paste0(BASE_URL, endpoint)

  req <- request(url)

  if (!is.null(search)) req <- req |> req_url_query(search = search)
  if (!is.null(count))  req <- req |> req_url_query(count = count)
  if (!is.null(limit))  req <- req |> req_url_query(limit = limit)
  if (nchar(API_KEY) > 0) req <- req |> req_url_query(api_key = API_KEY)

  resp <- req |>
    req_retry(max_tries = 3, backoff = ~ 2) |>
    req_perform()

  resp |> resp_body_json()
}


# ============================================================
# 1. TOP ADVERSE REACTIONS (across all drugs)
# ============================================================
cat("Pulling top adverse reactions...\n")

top_reactions <- fda_query(
  endpoint = "/drug/event.json",
  count = "patient.reaction.reactionmeddrapt.exact",
  limit = 25
)

reactions_df <- map_dfr(top_reactions$results, ~ tibble(
  reaction = .x$term,
  count    = .x$count
)) |>
  mutate(
    pct_of_max = round(count / max(count) * 100),
    count_k    = paste0(round(count / 1000), "k")
  )

cat("  Top reaction:", reactions_df$reaction[1], "with", reactions_df$count_k[1], "reports\n")


# ============================================================
# 2. TOP DRUGS BY ADVERSE EVENT REPORTS
# ============================================================
cat("Pulling top drugs by report volume...\n")

top_drugs <- fda_query(
  endpoint = "/drug/event.json",
  count = "patient.drug.openfda.generic_name.exact",
  limit = 20
)

drugs_df <- map_dfr(top_drugs$results, ~ tibble(
  drug  = .x$term,
  count = .x$count
)) |>
  mutate(
    pct_of_max = round(count / max(count) * 100),
    count_k    = paste0(round(count / 1000), "k")
  )

cat("  Top drug:", drugs_df$drug[1], "with", drugs_df$count_k[1], "reports\n")


# ============================================================
# 3. SERIOUS OUTCOME BREAKDOWN
# ============================================================
cat("Pulling serious outcome types...\n")

outcomes <- fda_query(
  endpoint = "/drug/event.json",
  count = "serious",
  limit = 10
)

# serious = 1 means serious, 2 means not serious
outcomes_df <- map_dfr(outcomes$results, ~ tibble(
  serious = ifelse(.x$term == 1, "Serious", "Not Serious"),
  count   = .x$count
)) |>
  mutate(pct = round(count / sum(count) * 100, 1))

cat("  Serious outcomes:", outcomes_df$pct[outcomes_df$serious == "Serious"], "%\n")


# ============================================================
# 4. REPORTS BY COUNTRY (top 15)
# ============================================================
cat("Pulling reports by country...\n")

by_country <- fda_query(
  endpoint = "/drug/event.json",
  count = "occurcountry.exact",
  limit = 15
)

country_df <- map_dfr(by_country$results, ~ tibble(
  country = .x$term,
  count   = .x$count
)) |>
  mutate(
    pct_of_max = round(count / max(count) * 100),
    count_k    = paste0(round(count / 1000), "k")
  )

cat("  Top country:", country_df$country[1], "\n")


# ============================================================
# 5. SPECIFIC DRUG LOOKUP (example: metformin)
# ============================================================
# This function can be called for any drug — in production,
# the app would call it on demand when a user asks about a drug.

drug_profile <- function(drug_name) {
  cat("Pulling profile for", drug_name, "...\n")

  # Total reports for this drug
  total <- fda_query(
    endpoint = "/drug/event.json",
    search = paste0('patient.drug.openfda.generic_name.exact:"', drug_name, '"'),
    limit = 1
  )
  total_count <- total$meta$results$total

  # Top reactions for this drug
  drug_reactions <- fda_query(
    endpoint = "/drug/event.json",
    search = paste0('patient.drug.openfda.generic_name.exact:"', drug_name, '"'),
    count = "patient.reaction.reactionmeddrapt.exact",
    limit = 15
  )

  react_df <- map_dfr(drug_reactions$results, ~ tibble(
    reaction = .x$term,
    count    = .x$count
  )) |>
    mutate(pct_of_max = round(count / max(count) * 100))

  # Serious vs not
  drug_serious <- fda_query(
    endpoint = "/drug/event.json",
    search = paste0('patient.drug.openfda.generic_name.exact:"', drug_name, '"'),
    count = "serious"
  )

  serious_df <- map_dfr(drug_serious$results, ~ tibble(
    serious = ifelse(.x$term == 1, "Serious", "Not Serious"),
    count   = .x$count
  )) |>
    mutate(pct = round(count / sum(count) * 100, 1))

  # Patient sex breakdown
  drug_sex <- tryCatch({
    res <- fda_query(
      endpoint = "/drug/event.json",
      search = paste0('patient.drug.openfda.generic_name.exact:"', drug_name, '"'),
      count = "patient.patientsex"
    )
    map_dfr(res$results, ~ tibble(
      sex   = case_when(.x$term == 1 ~ "Male", .x$term == 2 ~ "Female", TRUE ~ "Unknown"),
      count = .x$count
    ))
  }, error = function(e) tibble(sex = "Unknown", count = 0))

  list(
    drug_name    = drug_name,
    total_reports = total_count,
    top_reactions = react_df,
    serious      = serious_df,
    patient_sex  = drug_sex
  )
}

# Pull profiles for common drugs people will ask about
common_drugs <- c("METFORMIN", "LISINOPRIL", "ATORVASTATIN",
                  "AMLODIPINE", "OMEPRAZOLE", "ASPIRIN",
                  "IBUPROFEN", "ACETAMINOPHEN", "LEVOTHYROXINE")

drug_profiles <- list()
for (drug in common_drugs) {
  drug_profiles[[drug]] <- tryCatch(
    drug_profile(drug),
    error = function(e) {
      cat("  Error for", drug, ":", e$message, "\n")
      NULL
    }
  )
  Sys.sleep(0.5)  # Be polite to the API
}


# ============================================================
# 6. WRITE EVERYTHING TO JSON
# ============================================================
cat("\nWriting JSON files...\n")

# Summary data for the chat app's context
summary_data <- list(
  generated     = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  top_reactions = reactions_df,
  top_drugs     = drugs_df,
  outcomes      = outcomes_df,
  by_country    = country_df
)

write_json(summary_data,
           file.path(OUTPUT_DIR, "summary.json"),
           pretty = TRUE, auto_unbox = TRUE)

# Individual drug profiles
for (name in names(drug_profiles)) {
  if (!is.null(drug_profiles[[name]])) {
    write_json(drug_profiles[[name]],
               file.path(OUTPUT_DIR, paste0("drug_", tolower(name), ".json")),
               pretty = TRUE, auto_unbox = TRUE)
  }
}

cat("\nDone! Files written to:", OUTPUT_DIR, "\n")
cat("Files created:\n")
cat(paste(" ", list.files(OUTPUT_DIR), collapse = "\n"), "\n")
