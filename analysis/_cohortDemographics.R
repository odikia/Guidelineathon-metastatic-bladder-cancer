library(ggplot2)
library(dplyr)  
library(DBI)   
library(lubridate)
library(SqlRender)
library(stringr)
library(readr)

#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

target_cohort_definition_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName,"^Target")) %>%
  pull(cohortId)

all_patients_demographics <- dbGetQuery(con, SqlRender::translate(SqlRender::render (
    "SELECT c.subject_id, c.cohort_start_date, c.cohort_end_date, c.cohort_definition_id, p.gender_concept_id, p.year_of_birth
    FROM @work_database_schema.@cohort_table c
    LEFT JOIN @cdmDatabaseSchema.person p
      ON c.subject_id = p.person_id
        WHERE c.cohort_definition_id IN (@cohort_definition_ids)",
    work_database_schema = executionSettings$workDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    cdmDatabaseSchema = executionSettings$cdmDatabaseSchema,
    cohort_definition_ids = str_c(target_cohort_definition_ids, collapse = ",")), connectionDetails$dbms))  %>%
  mutate(
    age = year(cohort_start_date) - year_of_birth,
    age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 

for(strata in (c("all","age_group", "gender_concept_id"))) {

  save_dir <- file.path(outputFolder, "study_results", "demographics", strata)

  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

  if(strata == "all") strata = NULL

cohort_demographics <- all_patients_demographics %>%
  left_join(preparedCohortManifest %>% select(cohortId, cohortName), by = c("cohort_definition_id" = "cohortId")) %>%
  summarise(cohort_size = n_distinct(subject_id),
            prop_males = n_distinct(subject_id[gender_concept_id == 8507])/cohort_size,
            prop_females = n_distinct(subject_id[gender_concept_id == 8532])/cohort_size,
            age_median = median(year(cohort_start_date) - year_of_birth),
            age_q25 = quantile(year(cohort_start_date) - year_of_birth, 0.25),
            age_q75 = quantile(year(cohort_start_date) - year_of_birth, 0.75),
            age_min = min(year(cohort_start_date) - year_of_birth),
            age_max = max(year(cohort_start_date) - year_of_birth),
            .by = c(cohortName, cohort_definition_id, strata)) %>%
  arrange(cohort_definition_id, strata)

cohort_demographics <- cohort_demographics %>%
  mutate(
    cohort_size = case_when(cohort_size > 0 & cohort_size < minCellCount ~ -minCellCount, TRUE ~ cohort_size),
    across(-c(cohort_size,cohort_definition_id,cohortName,strata), ~case_when(cohort_size < 0 ~ NA, TRUE ~ .x))
  )

write_csv(cohort_demographics, file.path(save_dir, "cohort_demographics.csv"))

### distribution by cohort_start_date year
index_year_distribution <- all_patients_demographics %>%
  left_join(preparedCohortManifest %>% select(cohortId, cohortName), by = c("cohort_definition_id" = "cohortId")) %>%      
  mutate(cohort_start_date_year = year(cohort_start_date)) %>%
  summarise(cohort_size = n_distinct(subject_id), .by = c( cohort_definition_id, cohortName, strata, cohort_start_date_year))

index_year_distribution <- index_year_distribution %>%
  mutate(cohort_size = case_when(cohort_size > 0 & cohort_size < minCellCount ~ -minCellCount, TRUE ~ cohort_size))

write_csv(index_year_distribution, file.path(save_dir, "index_year_distribution.csv"))

}
