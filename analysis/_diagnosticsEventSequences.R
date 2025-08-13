# Setup and Dependencies ----------------------------------------
library(tidyverse) 
library(DBI)
library(SqlRender)
library(readr)
library(stringr)
library(lubridate)

dir.create(file.path(outputFolder, "study_results", "event_sequence_diagnostics"), showWarnings = FALSE, recursive = TRUE)

# Cohort ID Setup ---------------------------------------------
#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

# Extract cohort IDs
all_nsclc_id <- preparedCohortManifest %>%
    filter(cohortName == "ARTEMIS_NSCLC_cohort") %>%
    pull(cohortId)

target_cohort_1b_id <- preparedCohortManifest %>%
    filter(cohortName == "Target_Cohort_1B") %>%
    pull(cohortId)

met_cohort_id <- preparedCohortManifest %>%
    filter(cohortName == "Metastasis") %>%
    pull(cohortId)

prior_respiratory_tumour_cohort_id <- preparedCohortManifest %>%
    filter(cohortName == "Prior_Respiratory_Tumour") %>%
    pull(cohortId)

 # Data Loading ------------------------------------------------
all_patients <- dbGetQuery(con, SqlRender::render(
    "SELECT c1.* 
    FROM @work_database_schema.@cohort_table c1
    INNER JOIN (
        SELECT subject_id 
        FROM @work_database_schema.@cohort_table
        WHERE cohort_definition_id = @all_nsclc_id
        ) c2 on c1.subject_id = c2.subject_id",
    work_database_schema = executionSettings$workDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    all_nsclc_id = all_nsclc_id
))

episodes <- dbGetQuery(con, SqlRender::render(
    "SELECT * FROM @work_database_schema.@episodes_table",
    work_database_schema = executionSettings$workDatabaseSchema,
    episodes_table = executionSettings$ARTEMISEpisodeTableName
))

all_patients <- mutate(all_patients, subject_id = as.character(subject_id))
   
# NSCLC to Metastasis Analysis --------------------------------
# Calculate time from NSCLC diagnosis to metastasis
metastasis_date_relative_to_nsclc_date <- all_patients %>%
    filter(cohort_definition_id %in% c(target_cohort_1b_id)) %>% 
    left_join(all_patients %>%
        filter(cohort_definition_id == met_cohort_id) %>%
        select(subject_id, metastasis_date = cohort_start_date) %>%
        arrange(metastasis_date) %>%
        slice(1, .by = subject_id)
    ) %>%
    mutate(
        metastasis_date = case_when(
            metastasis_date > cohort_end_date ~ NA, 
            TRUE ~ metastasis_date
        ),
        time_to_metastasis = as.integer(metastasis_date - cohort_start_date),
        status = case_when(
            is.na(metastasis_date) ~ "No metastasis",
            cohort_start_date > metastasis_date ~ "metastasis before NSCLC",
            cohort_start_date <= metastasis_date ~ "NSCLC before metastasis"
        )
    ) 

# Summarize NSCLC to metastasis timing
summary_metastasis_date_relative_to_nsclc_date <- metastasis_date_relative_to_nsclc_date %>%
    summarise(
        count = n(),
        min_time = min(time_to_metastasis, na.rm = TRUE),
        c10_time = quantile(time_to_metastasis, 0.10, na.rm = TRUE),
        lq_time = quantile(time_to_metastasis, 0.25, na.rm = TRUE),
        median_time = median(time_to_metastasis, na.rm = TRUE),
        uq_time = quantile(time_to_metastasis, 0.75, na.rm = TRUE),
        c90_time = quantile(time_to_metastasis, 0.90, na.rm = TRUE),
        max_time = max(time_to_metastasis, na.rm = TRUE),
        .by = c(status, cohort_definition_id)
    ) %>%
    mutate(
        count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
        across(-c(count, status), ~case_when(count < 0 ~ NA, TRUE ~ .x))
    ) %>%
    arrange(cohort_definition_id, status)

write_csv(summary_metastasis_date_relative_to_nsclc_date, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "time_from_nsclc_to_metastasis_by_status.csv"))

# Create binned distribution of times
binned_metastasis_date_relative_to_nsclc_date <- metastasis_date_relative_to_nsclc_date %>%
    mutate(
        time_to_metastasis_bin = cut(
            time_to_metastasis, 
            breaks = c(-Inf, -365, -180, -90, -60, -30, 0, 30, 60, 90, 180, 365, Inf), 
            right = FALSE, 
            ordered_result = TRUE
        )
    ) %>%
    summarise(count = n(), .by = c(time_to_metastasis_bin, cohort_definition_id)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
    arrange(cohort_definition_id, time_to_metastasis_bin)

write_csv(binned_metastasis_date_relative_to_nsclc_date, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "binned_time_from_nsclc_to_metastasis.csv"))

# Prior Respiratory Tumour Analysis ---------------------------
# Add prior respiratory tumour information
metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date <- metastasis_date_relative_to_nsclc_date %>%
    left_join(all_patients %>%
        filter(cohort_definition_id == prior_respiratory_tumour_cohort_id) %>%
        select(subject_id, first_prior_respiratory_tumour_date = cohort_start_date) %>%
        arrange(first_prior_respiratory_tumour_date) %>%
        slice(1, .by = subject_id)
    ) %>%
    mutate(
        status2 = case_when(
            is.na(first_prior_respiratory_tumour_date) ~ "no prior respiratory tumour",
            first_prior_respiratory_tumour_date > metastasis_date ~ "metastasis before prior respiratory tumour",
            first_prior_respiratory_tumour_date <= metastasis_date ~ "prior respiratory tumour before metastasis",
            is.na(metastasis_date) ~ "prior respiratory tumour, no metastasis"
        ),
        time_from_prior_respiratory_tumour_to_nsclc = as.integer(cohort_start_date - first_prior_respiratory_tumour_date),
        time_from_prior_respiratory_tumour_to_metastasis = as.integer(metastasis_date - first_prior_respiratory_tumour_date)
    ) 

# Summarize timing relative to prior respiratory tumour
summary_prior_respiratory_tumour_date_relative_to_nsclc_date <- metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date %>%
    mutate(
        status = case_when(!is.na(time_from_prior_respiratory_tumour_to_nsclc) ~ "Has prior code",
                            TRUE ~ "Does not have prior code")) %>%
    summarise(
        count = n(),
        min_time = min(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        c10_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.10, na.rm = TRUE),
        lq_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.25, na.rm = TRUE),
        median_time = median(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        uq_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.75, na.rm = TRUE),
        c90_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.90, na.rm = TRUE),
        max_time = max(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        .by = c(status, cohort_definition_id)
    ) %>%
    mutate(
        count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
        across(-c(count), ~case_when(count < 0 ~ NA, TRUE ~ .x))
    ) %>%
    arrange(cohort_definition_id, status)

write_csv(summary_prior_respiratory_tumour_date_relative_to_nsclc_date, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "time_from_prior_respiratory_tumour_to_nsclc.csv"))

summary_prior_respiratory_tumour_date_relative_to_nsclc_date_by_status <- metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date %>%
    summarise(
        count = n(),
        min_time = min(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        c10_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.10, na.rm = TRUE),
        lq_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.25, na.rm = TRUE),
        median_time = median(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        uq_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.75, na.rm = TRUE),
        c90_time = quantile(time_from_prior_respiratory_tumour_to_nsclc, 0.90, na.rm = TRUE),
        max_time = max(time_from_prior_respiratory_tumour_to_nsclc, na.rm = TRUE),
        .by = c(cohort_definition_id, status, status2)
    ) %>%
    mutate(
        count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
        across(-c(count, status, status2), ~case_when(count < 0 ~ NA, TRUE ~ .x))
    ) %>%
    arrange(cohort_definition_id, status, status2)

write_csv(summary_prior_respiratory_tumour_date_relative_to_nsclc_date_by_status, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "time_from_prior_respiratory_tumour_to_nsclc_by_status.csv"))

binned_prior_respiratory_tumour_date_relative_to_nsclc_date <- metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date %>%
    mutate(time_from_prior_respiratory_tumour_to_nsclc_bin = cut(time_from_prior_respiratory_tumour_to_nsclc, breaks = c(-Inf, -365, -180, -90, -60, -30, 0, 30, 60, 90, 180, 365, Inf), right = F, ordered_result = TRUE)) %>%
    summarise(count = n(), .by = c(cohort_definition_id, time_from_prior_respiratory_tumour_to_nsclc_bin)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
    arrange(cohort_definition_id, time_from_prior_respiratory_tumour_to_nsclc_bin)

write_csv(binned_prior_respiratory_tumour_date_relative_to_nsclc_date, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "binned_time_from_prior_respiratory_tumour_to_nsclc.csv"))

summary_metastasis_date_relative_to_prior_respiratory_tumour_date_by_status <- metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date %>%
    summarise(
        count = n(),
        min_time_ = min(time_from_prior_respiratory_tumour_to_metastasis,na.rm=T),
        c10_time = quantile(time_from_prior_respiratory_tumour_to_metastasis ,0.10,na.rm=T),
        lq_time = quantile(time_from_prior_respiratory_tumour_to_metastasis ,0.25,na.rm=T),
        median_time = median(time_from_prior_respiratory_tumour_to_metastasis,na.rm=T),
        uq_time = quantile(time_from_prior_respiratory_tumour_to_metastasis ,0.75,na.rm=T),
        c90_time = quantile(time_from_prior_respiratory_tumour_to_metastasis ,0.90,na.rm=T),
        max_time = max(time_from_prior_respiratory_tumour_to_metastasis,na.rm=T),
        .by = c(cohort_definition_id, status, status2)
    ) %>%
    mutate(
        count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
        across(-c(count, status, status2), ~case_when(count < 0 ~ NA, TRUE ~ .x))
    ) %>%
    arrange(cohort_definition_id, status, status2)

write_csv(summary_metastasis_date_relative_to_prior_respiratory_tumour_date_by_status, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "time_from_prior_respiratory_tumour_to_metastasis_by_status.csv"))
    
 binned_metastasis_date_relative_to_prior_respiratory_tumour_date <- metastasis_date_relative_to_nsclc_date_w_prior_respiratory_tumour_date %>%
    mutate(time_to_metastasis_from_prior_respiratory_tumour_bin = cut(time_from_prior_respiratory_tumour_to_metastasis, breaks = c(-Inf, -365, -180, -90, -60, -30, 0, 30, 60, 90, 180, 365, Inf), right = F, ordered_result = TRUE)) %>%
    summarise(count = n(), .by = c(cohort_definition_id, time_to_metastasis_from_prior_respiratory_tumour_bin)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
    arrange(cohort_definition_id, time_to_metastasis_from_prior_respiratory_tumour_bin)

write_csv(binned_metastasis_date_relative_to_prior_respiratory_tumour_date, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "binned_time_from_prior_respiratory_tumour_to_metastasis.csv"))

#### Treatment Analysis ----------------------------------------
target_cohort_2a_id <- preparedCohortManifest %>%
    filter(cohortName == "Target_Cohort_2A") %>%
    pull(cohortId)

target_cohort_2b_id <- preparedCohortManifest %>%
    filter(cohortName == "Target_Cohort_2B") %>%
    pull(cohortId)

# Get first treatment episodes
episodes_target_cohort_2 <- all_patients %>%
    filter(cohort_definition_id %in% c(target_cohort_2a_id, target_cohort_2b_id)) %>%
    mutate(subject_id = as.character(subject_id)) %>%
    inner_join(
        episodes %>%    
            rename(subject_id = person_id) %>% 
            mutate(subject_id = as.character(subject_id))
    ) %>%
    filter(episode_end_date >= cohort_start_date) %>% 
    arrange(cohort_definition_id, subject_id, episode_start_date) %>% 
    group_by(cohort_definition_id, subject_id) %>%
    slice(1) %>%
    select(cohort_definition_id, subject_id, episode_start_date)


# Analyze first treatment timing
first_treatment_episode <- all_patients %>%
    mutate(subject_id = as.character(subject_id)) %>%
    filter(cohort_definition_id %in% c(target_cohort_2a_id, target_cohort_2b_id)) %>%
    left_join(episodes_target_cohort_2) %>%
    mutate(
        status = case_when(
            is.na(episode_start_date) ~ "No treatment", 
            cohort_start_date > episode_start_date ~ "Treatment initiated before mNSCLC",
            TRUE ~ "Treatment initiated after mNSCLC"
        ),
        time_to_treatment = as.integer(episode_start_date - cohort_start_date)
    ) 

# Summarize treatment timing
summary_first_treatment_episode <- first_treatment_episode %>%
    summarise(
        count = n(),
        min_time = min(time_to_treatment, na.rm = TRUE),
        c10_time = quantile(time_to_treatment, 0.10, na.rm = TRUE),
        lq_time = quantile(time_to_treatment, 0.25, na.rm = TRUE),
        median_time = median(time_to_treatment, na.rm = TRUE),
        uq_time = quantile(time_to_treatment, 0.75, na.rm = TRUE),
        c90_time = quantile(time_to_treatment, 0.90, na.rm = TRUE),
        max_time = max(time_to_treatment, na.rm = TRUE),
        .by = c(cohort_definition_id, status)
    ) %>%
    mutate(
        count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
        across(-c(count, status), ~case_when(count < 0 ~ NA, TRUE ~ .x))
    ) %>%
    arrange(cohort_definition_id, status)

write_csv(summary_first_treatment_episode, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "time_to_first_treatment_episode_by_status.csv"))

# Create binned distribution of treatment times
binned_first_treatment_episode <- first_treatment_episode %>%
    mutate(
        time_to_treatment_bin = cut(
            time_to_treatment, 
            breaks = c(-Inf, -365, -180, -90, -60, -30, 0, 30, 60, 90, 180, 365, Inf), 
            right = FALSE, 
            ordered_result = TRUE
        )
    ) %>%
    summarise(count = n(), .by = c(cohort_definition_id, time_to_treatment_bin)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
    arrange(cohort_definition_id, time_to_treatment_bin)

write_csv(binned_first_treatment_episode, 
    file.path(outputFolder, "study_results", "event_sequence_diagnostics", "binned_time_to_first_treatment_episode.csv"))
    
    
