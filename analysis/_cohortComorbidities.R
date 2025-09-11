library(dplyr) 
library(DatabaseConnector) 
library(SqlRender)
library(readr)
library(stringr)

# Cohort ID Setup ---------------------------------------------
#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

# Extract cohort IDs
all_nsclc_id <- preparedCohortManifest %>%
  filter(cohortName == "ARTEMIS_NSCLC_cohort") %>%
  pull(cohortId)

target_cohort_definition_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName, "^Target")) %>%
  pull(cohortId)

covariate_cohort_definition_ids <- preparedCohortManifest %>%
  filter(!str_detect(cohortName, "^Target|Death|Radiotherapy|X_All_NSCLC|Performance_Status")) %>%
  pull(cohortId)

# Data Loading ------------------------------------------------
all_comorbidities <- DatabaseConnector::dbGetQuery(
  con,
  SqlRender::translate(SqlRender::render(
    "SELECT c1.*, p1.gender_concept_id, YEAR(c1.cohort_start_date) - p1.year_of_birth as age, comorbidity_cohort_definition_id, c2.cohort_start_date as comorbidity_start_date
    FROM @work_database_schema.@cohort_table c1
    LEFT JOIN @cdm_database_schema.PERSON p1 ON c1.subject_id = p1.person_id
    INNER JOIN (
        SELECT cohort_definition_id as comorbidity_cohort_definition_id, subject_id, min(cohort_start_date) as cohort_start_date
        FROM @work_database_schema.@cohort_table
        WHERE cohort_definition_id IN (@covariate_cohort_definition_ids)
        GROUP BY cohort_definition_id, subject_id
        ) c2 on c1.subject_id = c2.subject_id
             and c1.cohort_start_date >= c2.cohort_start_date
    WHERE c1.cohort_definition_id IN (@target_cohort_definition_ids)",
    work_database_schema = executionSettings$workDatabaseSchema,
    cdm_database_schema = executionSettings$cdmDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    covariate_cohort_definition_ids = str_c(covariate_cohort_definition_ids, collapse = ","),
    target_cohort_definition_ids = str_c(target_cohort_definition_ids, collapse = ",")
  ), connectionDetails$dbms)) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 


for(strata in (c("all","age_group", "gender_concept_id"))) {
  
  save_dir <- file.path(outputFolder, "study_results", "comorbidities", strata)
  
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

  if(strata == "all") strata = NULL
  
  all_comorbidities %>%
    summarise(count = n_distinct(subject_id), .by = c(cohort_definition_id, comorbidity_cohort_definition_id, strata)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount,
                             TRUE ~ count)) %>% 
    write_csv(file.path(save_dir, "comorbidity_table.csv"))
  
}

### FIND ECOG CLOSES TO INDEX DATE. PRIORITISE THOSE 30 DAYS BEFORE INDEX DATE OVER THOSE 30 DAYS AFTER INDEX DATE
sql <- "
WITH ranked_comorbidities AS (
        SELECT 
            c1.cohort_definition_id as target_cohort_definition_id,
            c1.cohort_start_date,
            c2.subject_id,
            c2.cohort_definition_id as comorbidity_cohort_definition_id,
            c2.cohort_start_date as comorbiditiy_date,
            ROW_NUMBER() OVER (
                PARTITION BY c1.subject_id, c1.cohort_definition_id
                ORDER BY 
                    CASE 
                        WHEN DATEDIFF(day, c1.cohort_start_date, c2.cohort_start_date) BETWEEN -30 AND 0 THEN 1  -- BEFORE has priority 1
                        WHEN DATEDIFF(day, c1.cohort_start_date, c2.cohort_start_date) BETWEEN 1 AND 30 THEN 2    -- AFTER has priority 2
                        ELSE 3
                    END,
                    ABS(DATEDIFF(day, c2.cohort_start_date, c1.cohort_start_date))
            ) as rn
        FROM @work_database_schema.@cohort_table c2
        INNER JOIN @work_database_schema.@cohort_table c1 
            ON c2.subject_id = c1.subject_id
            AND c1.cohort_definition_id IN (@target_cohort_definition_ids)
        WHERE c2.cohort_definition_id IN (@covariate_cohort_definition_ids)
        AND ABS(DATEDIFF(day, c2.cohort_start_date, c1.cohort_start_date)) <= 30
) 
SELECT 
  c1.*, 
  p1.gender_concept_id, 
  YEAR(c1.cohort_start_date) - p1.year_of_birth as age, 
  r.comorbidity_cohort_definition_id, 
  r.comorbiditiy_date
FROM @work_database_schema.@cohort_table c1
LEFT JOIN @cdm_database_schema.PERSON p1 ON c1.subject_id = p1.person_id
LEFT JOIN ranked_comorbidities  r
    ON c1.subject_id = r.subject_id
    AND c1.cohort_definition_id = r.target_cohort_definition_id
    AND rn = 1
WHERE c1.cohort_definition_id IN (@target_cohort_definition_ids)
"

ecog_cohort_definition_ids <- preparedCohortManifest %>%
  filter(cohortName %in% c("Performance_Status_0","Performance_Status_1","Performance_Status_2")) %>%
  pull(cohortId)

  # Data Loading ------------------------------------------------
ecog_statuses <- DatabaseConnector::dbGetQuery(
  con,
  SqlRender::translate(SqlRender::render(
    sql,
    work_database_schema = executionSettings$workDatabaseSchema,
    cdm_database_schema = executionSettings$cdmDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    covariate_cohort_definition_ids = str_c(ecog_cohort_definition_ids, collapse = ","),
    target_cohort_definition_ids = str_c(target_cohort_definition_ids, collapse = ",")
  ), connectionDetails$dbms)) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 

                               
for(strata in (c("all","age_group", "gender_concept_id"))) {
  
  save_dir <- file.path(outputFolder, "study_results", "ecog_statuses", strata)
  
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

   if(strata == "all") strata = NULL
  
  ecog_statuses %>%
    summarise(count = n_distinct(subject_id), .by = c(cohort_definition_id, comorbidity_cohort_definition_id, strata)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount,
                             TRUE ~ count)) %>% 
    write_csv(file.path(save_dir, "ecog_status.csv"))
  
}

### Radiotherapy
sql <- "
WITH radiotherapy AS (
  SELECT 
    c1.cohort_definition_id, c1.subject_id, c1.cohort_start_date, 
    min(r.cohort_start_date) as radiotherapy_start_date
  FROM @work_database_schema.@cohort_table c1
  LEFT JOIN @work_database_schema.@cohort_table r
    ON c1.subject_id = r.subject_id
    AND r.cohort_start_date >= c1.cohort_start_date
    AND r.cohort_start_date <= DATEADD(day, 360, c1.cohort_start_date)
    AND r.cohort_definition_id IN (@radiotherapy_cohort_definition_ids)
WHERE c1.cohort_definition_id IN (@target_cohort_definition_ids)
GROUP BY c1.cohort_definition_id, c1.subject_id, c1.cohort_start_date
)
SELECT 
  r.cohort_definition_id, 
  r.subject_id, 
  r.cohort_start_date,
  p1.gender_concept_id,
  YEAR(r.cohort_start_date) - p1.year_of_birth as age,
  CASE 
    WHEN r.radiotherapy_start_date IS NULL THEN 'No radiotherapy within 1Y'
    WHEN r.radiotherapy_start_date <= DATEADD(day, 30, r.cohort_start_date) THEN '0-30d'
    WHEN r.radiotherapy_start_date <= DATEADD(day, 60, r.cohort_start_date) THEN '31-60d'
    WHEN r.radiotherapy_start_date <= DATEADD(day, 180, r.cohort_start_date) THEN '61-180d'
    WHEN r.radiotherapy_start_date <= DATEADD(day, 365, r.cohort_start_date) THEN '181-365d'
    ELSE 'Other'
  END as radiotherapy_status
FROM radiotherapy r
LEFT JOIN @cdm_database_schema.PERSON p1 ON r.subject_id = p1.person_id
"

radiotherapy_cohort_definition_ids <- preparedCohortManifest %>%
  filter(cohortName == "Radiotherapy") %>%
  pull(cohortId)

radiotherapy_statuses <- DatabaseConnector::dbGetQuery(
  con,
  SqlRender::translate(SqlRender::render(
    sql,
    work_database_schema = executionSettings$workDatabaseSchema,
    cdm_database_schema = executionSettings$cdmDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    radiotherapy_cohort_definition_ids = str_c(radiotherapy_cohort_definition_ids, collapse = ","),
    target_cohort_definition_ids = str_c(target_cohort_definition_ids, collapse = ",")
  ), connectionDetails$dbms)) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 

for(strata in (c("all","age_group", "gender_concept_id"))) {
  
  save_dir <- file.path(outputFolder, "study_results", "radiotherapy_statuses", strata)
  
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

  if(strata == "all") strata = NULL
  
  radiotherapy_statuses %>%
    summarise(count = n_distinct(subject_id), .by = c(cohort_definition_id, radiotherapy_status, strata)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount,
                             TRUE ~ count)) %>% 
    write_csv(file.path(save_dir, "radiotherapy_statuses.csv"))
  
}