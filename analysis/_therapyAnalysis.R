library(dplyr)  
library(DBI) 
library(SqlRender)
library(readr)
library(stringr)

dir.create(file.path(outputFolder, "study_results", "therapy_analysis"), showWarnings = FALSE, recursive = TRUE)

regimen_categories <- read_csv("extras/regimen_classifications.csv")

# Cohort ID Setup ---------------------------------------------
#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

target_cohort_definition_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName, "_base.json")) %>%
  pull(cohortId)

all_bc_id <- preparedCohortManifest %>%
  filter(cohortName == "ARTEMIS_bladder_cohort") %>%
  pull(cohortId)

 # Data Loading ------------------------------------------------
all_patients <- DatabaseConnector::dbGetQuery(
  con,
  SqlRender::translate(SqlRender::render(
    "SELECT c1.*, p1.gender_concept_id, YEAR(c1.cohort_start_date) - p1.year_of_birth as age
    FROM @work_database_schema.@cohort_table c1
    LEFT JOIN @cdm_database_schema.PERSON p1 ON c1.subject_id = p1.person_id
    INNER JOIN (
        SELECT subject_id 
        FROM @work_database_schema.@cohort_table
        WHERE cohort_definition_id = @all_bc_id
        ) c2 on c1.subject_id = c2.subject_id",
    work_database_schema = executionSettings$workDatabaseSchema,
    cdm_database_schema = executionSettings$cdmDatabaseSchema,
    cohort_table = executionSettings$cohortTable,
    all_bc_id = all_bc_id
), connectionDetails$dbms)) %>%
  mutate(age_group = case_when(age < 65 ~ "18-64",
                               age <= 80 ~ "65-80",
                               TRUE ~ "81+")) 


episodes <- dbGetQuery(con, SqlRender::render(
    "SELECT * FROM @work_database_schema.@episodes_table",
    work_database_schema = executionSettings$workDatabaseSchema,
    episodes_table = executionSettings$ARTEMISEpisodeTableName))

#### Identify maintenance therapies
episodes <- episodes %>%
    arrange(person_id, episode_start_date) %>%
    mutate(
        maintenance_therapy = 
        case_when(episode_number == 1 | episode_start_date - lag(episode_end_date,1) > 120 ~ FALSE,
    (episode_source_value == "Bevacizumab monotherapy" | episode_object_concept_id == 35803688) & 
        (lag(episode_source_value,1) %in% c("Carboplatin & Docetaxel (DCb) & Bevacizumab", "Carboplatin & Gemcitabine (GCb) & Bevacizumab", "PacCBev: Paclitaxel, Carboplatin, Bevacizumab", "B+CP: Bevacizumab, Carboplatin, Paclitaxel", "BCP: Bevacizumab, Carboplatin, Paclitaxel", "Cisplatin & Docetaxel (DC) & Bevacizumab", "Cisplatin & Gemcitabine (GC) & Bevacizumab", "Cisplatin & Vinorelbine (CVb) & Bevacizumab") | 
        lag(episode_object_concept_id,1) %in% c(37557097, 35806482, 35806401, 35806401, 35806401, 37557103, 35806411)) ~ TRUE,

    # Docetaxel maintenance
    (episode_source_value == "Docetaxel monotherapy" | episode_object_concept_id == 35804162) & 
        (lag(episode_source_value,1) == "Cisplatin & Docetaxel (DC)" | lag(episode_object_concept_id,1) == 35803574) ~ TRUE,

    # Gemcitabine maintenance
    (episode_source_value == "Gemcitabine monotherapy" | episode_object_concept_id == 35804135) & 
        (lag(episode_source_value,1) %in% c("Cisplatin & Gemcitabine") | lag(episode_object_concept_id,1) %in% c(35803575,35804156)) ~ TRUE,

    # Ipilimumab maintenance
    (episode_source_value == "Ipilimumab monotherapy" | episode_object_concept_id == 35806118) & 
        (lag(episode_source_value,1) == "Carboplatin & Paclitaxel (CP) & Ipilimumab" | lag(episode_object_concept_id,1) == 35806402) ~ TRUE,

    # Pembrolizumab maintenance
    (episode_source_value == "Pembrolizumab monotherapy" | episode_object_concept_id == 35803678) & 
        str_detect(lag(episode_source_value,1), "Pembrolizumab") & str_detect(lag(episode_source_value,1), "Carboplatin|Cisplatin") ~ TRUE,  # platinum-based regimen

    # Pemetrexed maintenance
    (episode_source_value == "Pemetrexed monotherapy" | episode_object_concept_id == 35804168) & 
        (str_detect(lag(episode_source_value,1), "Pemetrexed") & str_detect(lag(episode_source_value,1), "Carboplatin|Cisplatin")) ~ TRUE,

    # Nivo maintenance
    (episode_source_value == "Nivolumab monotherapy" | episode_object_concept_id == 35803677) & 
        (str_detect(lag(episode_source_value,1), "Nivolumab") & str_detect(lag(episode_source_value,1), "Ipilimumab")) ~ TRUE,

    # CPP Maintanence
    (episode_source_value == "Pemetrexed & Pembrolizumab" | episode_source_value == "Pembrolizumab & Pemetrexed" | episode_object_concept_id == 35806404) & 
        (lag(episode_source_value,1) == "Carboplatin, Pemetrexed, Pembrolizumab" | lag(episode_object_concept_id,1) == 35806403) ~ TRUE, 
    # Default case
    TRUE ~ FALSE
), .by = person_id) %>%
    mutate(episode_number = case_when(maintenance_therapy ~ episode_number - 1, TRUE ~ episode_number)) %>%
    summarise(
        episode_id = episode_id[1],
        episode_type_concept_id = episode_type_concept_id[1],
        episode_start_date = min(episode_start_date), 
        episode_end_date = max(episode_end_date), 
        episode_source_value = episode_source_value[1],
        episode_concept_id = episode_concept_id[1],
        episode_parent_id = episode_parent_id[1],
        episode_source_concept_id = episode_source_concept_id[1],
        episode_start_datetime = min(episode_start_datetime),
        episode_end_datetime = max(episode_end_datetime),
        episode_object_concept_id = episode_object_concept_id[1],
        .by = c(person_id, episode_number)) %>%
    mutate(episode_number = as.numeric(episode_number)) %>%
    arrange(person_id, episode_number) %>%
    mutate(episode_number = row_number(), .by =  person_id)


target_initiated_id <- preparedCohortManifest %>%
    filter(cohortName == "Target_1A_initiated_base" | cohortName == "Target_1B_initiated_base ") %>%
    pull(cohortId)

for(guideline in c("4","5","6")){
  for(strata in (c("all","age_group", "gender_concept_id"))) {

    save_dir <- file.path(outputFolder, "study_results", "therapy_analysis", strata, str_c("guideline_",guideline))

    dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)

    if(strata == "all") strata = NULL

    treated_patients <- all_patients %>% 
      filter(cohort_definition_id %in% target_initiated_id) %>%
      left_join(select(episodes, subject_id = person_id, episode_start_date, episode_end_date, episode_source_value, episode_number, episode_object_concept_id)) %>%
      filter(episode_end_date >= cohort_start_date) %>%
      mutate(episode_start_date = pmax(episode_start_date, cohort_start_date)) %>%
      arrange(strata, cohort_definition_id, subject_id, episode_start_date) %>%
      mutate(episode_number = row_number(), .by = c(strata, cohort_definition_id, subject_id)) %>%
      left_join(regimen_categories, by = c("episode_source_value" = "regName")) %>%
      mutate(category = coalesce(!!sym(str_c("cohort_T",guideline)), "Not categorized")) %>%
      mutate(
        time_since_last_treatment = as.integer(episode_start_date - lag(episode_end_date,1)), 
        time_to_next_treatment = as.integer(lead(episode_start_date,1) - episode_end_date),
        treatment_length = as.integer(episode_end_date - episode_start_date),
        .by = c(strata, cohort_definition_id, subject_id)
        )

    ### Regimen category counts
    treated_patients %>%
      mutate(treatment_year = year(episode_start_date),
             line_of_therapy = "All") %>%
      bind_rows(mutate({.}, line_of_therapy = str_c("Line",episode_number))) %>%
      summarise(count = n(), .by = c(strata, cohort_definition_id, category, line_of_therapy)) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
      arrange(strata, cohort_definition_id, category, line_of_therapy) %>%
      write_csv(file.path(save_dir, "category_counts.csv"))

    ### Regimen category counts by year
    treated_patients %>%
      mutate(treatment_year = year(episode_start_date),
             line_of_therapy = "All") %>%
      bind_rows(mutate({.}, line_of_therapy = str_c("Line",episode_number))) %>%
      mutate(total_count = n(), .by = c(strata, cohort_definition_id, treatment_year, line_of_therapy)) %>%
      summarise(count = n(), .by = c(strata, cohort_definition_id, treatment_year, total_count, category, line_of_therapy)) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
             total_count = case_when(total_count > 0 & total_count < minCellCount ~ -minCellCount, TRUE ~ total_count)
      ) %>%
      arrange(strata, cohort_definition_id, treatment_year, line_of_therapy) %>%
      write_csv(file.path(save_dir, "category_counts_by_year.csv"))

    ### Treatment summary
    treated_patients %>%
      summarise(lines_of_therapy = n_distinct(episode_number),
                treatment_length = as.integer(max(episode_end_date) - min(episode_start_date)),
                .by = c(strata, cohort_definition_id, subject_id)) %>%
      summarise(count = n(), 
                treatment_length_median = median(treatment_length),
                treatment_length_q25 = quantile(treatment_length, 0.25),
                treatment_length_q75 = quantile(treatment_length, 0.75),
                treatment_length_min = min(treatment_length),
                treatment_length_max = max(treatment_length),
                lines_of_therapy_median = median(lines_of_therapy),
                lines_of_therapy_q25 = quantile(lines_of_therapy, 0.25),
                lines_of_therapy_q75 = quantile(lines_of_therapy, 0.75),
                lines_of_therapy_min = min(lines_of_therapy),
                lines_of_therapy_max = max(lines_of_therapy),
                .by = c(strata, cohort_definition_id)
      ) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
      mutate(across(-c(strata, cohort_definition_id, count), ~case_when(count < 0 ~ NA_real_, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, "treatment_length_and_lines_of_therapy_summary.csv"))

    ### Treatment stats by episode
    treated_patients %>%
      summarise(count = n(),
                treatment_length_median = median(treatment_length, na.rm = T),
                treatment_length_q25 = quantile(treatment_length, 0.25, na.rm = T),
                treatment_length_q75 = quantile(treatment_length, 0.75, na.rm = T),
                treatment_length_min = min(treatment_length, na.rm = T),
                treatment_length_max = max(treatment_length, na.rm = T),
                time_since_last_treatment_median = median(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_q25 = quantile(time_since_last_treatment, 0.25, na.rm = T),
                time_since_last_treatment_q75 = quantile(time_since_last_treatment, 0.75, na.rm = T),
                time_since_last_treatment_min = min(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_max = max(time_since_last_treatment, na.rm = T),
                count_with_next_treatment = sum(!is.na(time_to_next_treatment)),
                time_to_next_treatment_median = median(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_q25 = quantile(time_to_next_treatment, 0.25, na.rm = T),
                time_to_next_treatment_q75 = quantile(time_to_next_treatment, 0.75, na.rm = T),
                time_to_next_treatment_min = min(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_max = max(time_to_next_treatment, na.rm = T),
                .by = c(strata, cohort_definition_id, episode_number)
      ) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
             across(treatment_length_median:time_since_last_treatment_max, ~case_when(count < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
      mutate(count_with_next_treatment = case_when(count_with_next_treatment > 0 & count_with_next_treatment < minCellCount ~ -minCellCount, TRUE ~ count_with_next_treatment),
             across(time_to_next_treatment_median:time_to_next_treatment_max, ~case_when(count_with_next_treatment < 0 ~ NA_real_, TRUE ~ .x))
       ) %>%
       arrange(strata, cohort_definition_id, episode_number) %>%
      write_csv(file.path(save_dir, "treatment_stats_by_episode.csv"))

    ### Treatment stats by episode and regimen category
    treated_patients %>%
      summarise(count = n(),
                treatment_length_median = median(treatment_length, na.rm = T),
                treatment_length_q25 = quantile(treatment_length, 0.25, na.rm = T),
                treatment_length_q75 = quantile(treatment_length, 0.75, na.rm = T),
                treatment_length_min = min(treatment_length, na.rm = T),
                treatment_length_max = max(treatment_length, na.rm = T),
                time_since_last_treatment_median = median(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_q25 = quantile(time_since_last_treatment, 0.25, na.rm = T),
                time_since_last_treatment_q75 = quantile(time_since_last_treatment, 0.75, na.rm = T),
                time_since_last_treatment_min = min(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_max = max(time_since_last_treatment, na.rm = T),
                count_with_next_treatment = sum(!is.na(time_to_next_treatment)),
                time_to_next_treatment_median = median(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_q25 = quantile(time_to_next_treatment, 0.25, na.rm = T),
                time_to_next_treatment_q75 = quantile(time_to_next_treatment, 0.75, na.rm = T),
                time_to_next_treatment_min = min(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_max = max(time_to_next_treatment, na.rm = T),
                .by = c(strata, cohort_definition_id, episode_number, category)
      ) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
             across(treatment_length_median:time_since_last_treatment_max, ~case_when(count < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
       mutate(count_with_next_treatment = case_when(count_with_next_treatment > 0 & count_with_next_treatment < minCellCount ~ -minCellCount, TRUE ~ count_with_next_treatment),
             across(time_to_next_treatment_median:time_to_next_treatment_max, ~case_when(count_with_next_treatment < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
      arrange(strata,cohort_definition_id, episode_number, category) %>%
      write_csv(file.path(save_dir, "treatment_stats_by_episode_and_category.csv"))

    ### Treatment stats by episode and regimen category and regimen name
    treated_patients %>%
      summarise(count = n(),
                treatment_length_median = median(treatment_length, na.rm = T),
                treatment_length_q25 = quantile(treatment_length, 0.25, na.rm = T),
                treatment_length_q75 = quantile(treatment_length, 0.75, na.rm = T),
                treatment_length_min = min(treatment_length, na.rm = T),
                treatment_length_max = max(treatment_length, na.rm = T),
                time_since_last_treatment_median = median(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_q25 = quantile(time_since_last_treatment, 0.25, na.rm = T),
                time_since_last_treatment_q75 = quantile(time_since_last_treatment, 0.75, na.rm = T),
                time_since_last_treatment_min = min(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_max = max(time_since_last_treatment, na.rm = T),
                count_with_next_treatment = sum(!is.na(time_to_next_treatment)),
                time_to_next_treatment_median = median(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_q25 = quantile(time_to_next_treatment, 0.25, na.rm = T),
                time_to_next_treatment_q75 = quantile(time_to_next_treatment, 0.75, na.rm = T),
                time_to_next_treatment_min = min(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_max = max(time_to_next_treatment, na.rm = T),
                .by = c(strata, cohort_definition_id, episode_number, category, episode_source_value, episode_object_concept_id)
      ) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
             across(treatment_length_median:time_since_last_treatment_max, ~case_when(count < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
       mutate(count_with_next_treatment = case_when(count_with_next_treatment > 0 & count_with_next_treatment < minCellCount ~ -minCellCount, TRUE ~ count_with_next_treatment),
             across(time_to_next_treatment_median:time_to_next_treatment_max, ~case_when(count_with_next_treatment < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
      arrange(strata, cohort_definition_id, episode_number, category, episode_source_value, episode_object_concept_id) %>%
      write_csv(file.path(save_dir, "treatment_stats_by_episode_and_category_and_regimen_name.csv"))

    ### Treatment stats by regimen category and regimen name
    treated_patients %>%
      summarise(count = n(),
                treatment_length_median = median(treatment_length, na.rm = T),
                treatment_length_q25 = quantile(treatment_length, 0.25, na.rm = T),
                treatment_length_q75 = quantile(treatment_length, 0.75, na.rm = T),
                treatment_length_min = min(treatment_length, na.rm = T),
                treatment_length_max = max(treatment_length, na.rm = T),
                time_since_last_treatment_median = median(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_q25 = quantile(time_since_last_treatment, 0.25, na.rm = T),
                time_since_last_treatment_q75 = quantile(time_since_last_treatment, 0.75, na.rm = T),
                time_since_last_treatment_min = min(time_since_last_treatment, na.rm = T),
                time_since_last_treatment_max = max(time_since_last_treatment, na.rm = T),
                count_with_next_treatment = sum(!is.na(time_to_next_treatment)),
                time_to_next_treatment_median = median(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_q25 = quantile(time_to_next_treatment, 0.25, na.rm = T),
                time_to_next_treatment_q75 = quantile(time_to_next_treatment, 0.75, na.rm = T),
                time_to_next_treatment_min = min(time_to_next_treatment, na.rm = T),
                time_to_next_treatment_max = max(time_to_next_treatment, na.rm = T),
                .by = c(strata, cohort_definition_id, category, episode_source_value, episode_object_concept_id)
      ) %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count),
             across(treatment_length_median:time_since_last_treatment_max, ~case_when(count < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
      arrange(strata, cohort_definition_id, category, episode_source_value, episode_object_concept_id) %>%
       mutate(count_with_next_treatment = case_when(count_with_next_treatment > 0 & count_with_next_treatment < minCellCount ~ -minCellCount, TRUE ~ count_with_next_treatment),
             across(time_to_next_treatment_median:time_to_next_treatment_max, ~case_when(count_with_next_treatment < 0 ~ NA_real_, TRUE ~ .x))
      ) %>%
      write_csv(file.path(save_dir, "treatment_stats_by_category_and_regimen_name.csv"))

    ### Treatment stats by regimen category and line sankey 

    if(is.null(strata)){

      full_pathways <- treated_patients %>% 
        filter(episode_number <= 3) %>%
        mutate(line = str_c("Line" ,episode_number)) %>%
        select(strata,  cohort_definition_id, subject_id, line, category) %>%
        complete(nesting(cohort_definition_id, subject_id), line = c("Line1", "Line2", "Line3")) %>%
        pivot_wider (names_from = line, values_from = category) %>%
        summarise(count = n(),
                .by = c(strata, cohort_definition_id, Line1, Line2, Line3)
        ) %>%
        arrange(strata, cohort_definition_id, desc(count))
    }

    if(!is.null(strata)){

      full_pathways <- treated_patients %>% 
        filter(episode_number <= 3) %>%
        mutate(line = str_c("Line" ,episode_number)) %>%
        select(strata,  cohort_definition_id, subject_id, line, category) %>%
        complete(nesting(!!sym(strata), cohort_definition_id, subject_id), line = c("Line1", "Line2", "Line3")) %>%
        pivot_wider (names_from = line, values_from = category) %>%
        summarise(count = n(),
                .by = c(strata, cohort_definition_id, Line1, Line2, Line3)
        ) %>%
        arrange(strata, cohort_definition_id, desc(count))
    }

      
    full_pathways %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
      write_csv(file.path(save_dir, "treatment_pathways_overall.csv"))

    full_pathways_censored_line3 <- full_pathways %>%
      mutate(Line3 = case_when(count > 0 & count < minCellCount ~ NA_character_, TRUE ~ Line3)) %>%
      summarise(count = sum(count), .by = c(strata, cohort_definition_id, Line1, Line2, Line3)) %>%
      arrange(strata, cohort_definition_id, desc(count))

    full_pathways_censored_line3 %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>%
      write_csv(file.path(save_dir, "treatment_pathways_censored_line3.csv"))

    full_pathways_censored_line2 <- full_pathways_censored_line3 %>%
      mutate(Line2 = case_when(count > 0 & count < minCellCount ~ NA_character_, TRUE ~ Line2)) %>%
      summarise(count = sum(count), .by = c(strata, cohort_definition_id, Line1, Line2, Line3)) %>%
      arrange(strata, cohort_definition_id, desc(count))

    full_pathways_censored_line2 %>%
      mutate(count = case_when(count > 0 & count < minCellCount ~ -minCellCount, TRUE ~ count)) %>% 
      write_csv(file.path(save_dir, "treatment_pathways_censored_line3_line2.csv"))
  }
}
