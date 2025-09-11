library(dplyr) 
library(DatabaseConnector) 
library(SqlRender)
library(readr)
library(stringr)

# Cohort ID Setup ---------------------------------------------
#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

cohort_definition_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName,"^Target")) %>%
  pull(cohortId) %>%
  str_c(collapse = ",")

sql <- "
-- Conditions
with disease as ( -- define disease categories similar to ICD10 C
  select 1 as precedence, 'Blood disease' as disease_category, 440371 as snomed_rollup union
  select 1, 'Blood disease', 443723 union
  select 2, 'Injury and poisoning', 432795 union
  select 2, 'Injury and poisoning', 442562 union
  select 2, 'Injury and poisoning', 444363 union
  select 3, 'Congenital disease', 440508 union
  select 4, 'Pregnancy or childbirth disease', 435875 union
  select 4, 'Pregnancy or childbirth disease', 4088927 union
  select 4, 'Pregnancy or childbirth disease', 4154314 union
  select 4, 'Pregnancy or childbirth disease', 4136529 union
  select 5, 'Perinatal disease', 441406 union
  select 6, 'Infection', 432250 union
  select 7, 'Neoplasm', 4266186 union
  select 8, 'Endocrine or metabolic disease', 31821 union
  select 8, 'Endocrine or metabolic disease', 4090739 union
  select 8, 'Endocrine or metabolic disease', 436670 union
  select 9, 'Mental disease', 432586 union
  select 9, 'Mental disease', 4023059 union
  select 9, 'Mental disease', 4293175 union
  select 10, 'Nerve disease and pain', 376337 union
  select 10, 'Nerve disease and pain', 4011630 union
  select 11, 'Eye disease', 4038502 union
  select 12, 'ENT disease', 4042836 union
  select 13, 'Cardiovascular disease', 134057 union
  select 14, 'Respiratory disease', 320136 union
  select 14, 'Respiratory disease', 4115386 union
  select 15, 'Digestive disease', 4302537 union
  select 16, 'Skin disease', 4028387 union
  select 17, 'Soft tissue or bone disease', 4244662 union
  select 17, 'Soft tissue or bone disease', 433595 union
  select 17, 'Soft tissue or bone disease', 4344497 union
  select 17, 'Soft tissue or bone disease', 40482430 union
  select 17, 'Soft tissue or bone disease', 4027384 union
  select 18, 'Genitourinary disease', 4041285 union
  select 19, 'Iatrogenic condition', 4105886 union
  select 19, 'Iatrogenic condition', 4053838 union
  select 19, 'Iatrogenic condition', 444199 union
  select 20, 'Not categorized', 441840
),
subject_conditions AS (
  SELECT 
    cohort_definition_id,
    subject_id,
    YEAR(c.cohort_start_date) - p1.year_of_birth as age,
    gender_concept_id,
    condition_concept_id
   FROM @work_database_schema.@cohort_table c
   
   LEFT JOIN @cdm_database_schema.PERSON p1 ON c.subject_id = p1.person_id
   INNER JOIN @cdm_database_schema.condition_occurrence co 
    ON c.subject_id = co.person_id 
    AND c.cohort_start_date >= co.condition_start_date
  WHERE c.cohort_definition_id IN (@cohort_definition_ids)
),
categorized_conditions AS (
    SELECT 
        sc.*,
        FIRST_VALUE(COALESCE(disease_category, 'Other Condition')) 
            OVER (PARTITION BY condition_concept_id 
                  ORDER BY CASE WHEN precedence IS NULL THEN 1 ELSE 0 END, precedence) as disease_category,
        snomed_rollup
    FROM subject_conditions sc
    LEFT JOIN (
        SELECT descendant_concept_id, snomed_rollup, disease_category, precedence
        FROM @cdm_database_schema.concept_ancestor 
        JOIN disease ON ancestor_concept_id = snomed_rollup
    ) d ON descendant_concept_id = condition_concept_id
)
SELECT DISTINCT
    cohort_definition_id, 
    subject_id, 
    snomed_rollup, 
    age, 
    gender_concept_id,
    disease_category
FROM categorized_conditions;
"

rollups <- dbGetQuery(
  con, 
  SqlRender::translate(
    SqlRender::render(sql, cohort_definition_ids = cohort_definition_ids, cdm_database_schema = executionSettings$cdmDatabaseSchema, work_database_schema = executionSettings$workDatabaseSchema, cohort_table = executionSettings$cohortTable),
    connectionDetails$dbms
  )
) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 


sql <- "
-- Drugs
WITH drug AS ( -- define ATC level 1 categories
  SELECT 
    ROW_NUMBER() OVER (ORDER BY concept_code) as precedence, 
    concept_name as category_name, 
    concept_id as category_id
  FROM @cdm_database_schema.concept
  WHERE vocabulary_id = 'ATC' 
    AND concept_class_id = 'ATC 1st'
),
subject_drugs AS (
  SELECT 
    cohort_definition_id,
    subject_id,
    YEAR(c.cohort_start_date) - p1.year_of_birth as age,
    gender_concept_id,
    drug_concept_id
  FROM @work_database_schema.@cohort_table c
  LEFT JOIN @cdm_database_schema.PERSON p1 
    ON c.subject_id = p1.person_id
  INNER JOIN @cdm_database_schema.drug_exposure de
    ON c.subject_id = de.person_id 
    AND c.cohort_start_date >= de.drug_exposure_start_date
  WHERE c.cohort_definition_id IN (@cohort_definition_ids)
),
categorized_drugs AS (
  SELECT 
    sd.*,
    FIRST_VALUE(COALESCE(category_name, 'Other Drug')) 
      OVER (PARTITION BY drug_concept_id 
            ORDER BY CASE WHEN precedence IS NULL THEN 1 ELSE 0 END, precedence) as drug_category,
    category_id
  FROM subject_drugs sd
  LEFT JOIN (
    SELECT descendant_concept_id, category_id, category_name, precedence
    FROM @cdm_database_schema.concept_ancestor
    JOIN drug ON ancestor_concept_id = category_id
  ) d ON descendant_concept_id = drug_concept_id
)
SELECT DISTINCT
  cohort_definition_id, 
  subject_id, 
  category_id, 
  age, 
  gender_concept_id,
  drug_category
FROM categorized_drugs;
"

# Execute query and process results
drug_rollups <- dbGetQuery(
  con, 
  SqlRender::translate(
    SqlRender::render(sql, cohort_definition_ids = cohort_definition_ids, cdm_database_schema = executionSettings$cdmDatabaseSchema, work_database_schema = executionSettings$workDatabaseSchema, cohort_table = executionSettings$cohortTable),
    connectionDetails$dbms
  )
) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 


for(strata in (c("all","age_group", "gender_concept_id"))) {
  
  save_dir <- file.path(outputFolder, "study_results", "rollups", strata)
  
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  
  if(strata == "all") strata = NULL
  
  rollups %>%
    summarise(count = n_distinct(subject_id), .by = c(cohort_definition_id, disease_category, strata)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount,
                             TRUE ~ count)) %>% 
    write_csv(file.path(save_dir, "condition_rollups.csv"))
  
  # Overall counts
  drug_rollups %>%
    summarise(count = n_distinct(subject_id), 
              .by = c(cohort_definition_id, drug_category, strata)) %>%
    mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount,
                             TRUE ~ count)) %>% 
    write_csv(file.path(save_dir, "drug_rollups.csv"))
  
  
}
