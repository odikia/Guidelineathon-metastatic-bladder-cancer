library(survival)
library(ggplot2)
library(dplyr)  # Added for dplyr functions
library(DBI)    # Added for database connection functions
library(survminer)

dir.create(file.path(outputFolder, "study_results", "time_to_event"), showWarnings = FALSE, recursive = TRUE)

# Cohort ID Setup ---------------------------------------------
#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

# Extract cohort IDs
all_bc_id <- preparedCohortManifest %>%
  filter(cohortName == "ARTEMIS_bladder_cohort") %>%
  pull(cohortId)

# Data Loading ------------------------------------------------
all_patients <- DatabaseConnector::dbGetQuery(
  con,
  SqlRender::render(
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
)) %>%
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

# Define cohort survival function
cohort_surv <- function(
    target_population = target,
    outcome_population = outcome,
    prior_outcome_handling = "ignore",  # Options: "ignore", "bring_forward", "remove_patients"
    target_start_date = "cohort_start_date",
    target_censor_date = "cohort_end_date",
    outcome_date = "outcome_cohort_start_date",
    target_strata = NULL,
    plot_incidence = FALSE
) {
  
  # Validate prior_outcome_handling parameter
  if (!prior_outcome_handling %in% c("ignore", "bring_forward", "remove_patients")) {
    stop("prior_outcome_handling must be one of: 'ignore', 'bring_forward', 'remove_patients'")
  }
  
  ds <- target_population %>%
    left_join(outcome_population, by = "subject_id") %>%
    mutate(
      has_prior_outcome = any(!is.na(!!sym(outcome_date)) & !!sym(outcome_date) < !!sym(target_start_date)),
      .by = subject_id
    )
  
  # Handle prior outcomes according to specified behavior
  ds <- switch(prior_outcome_handling,
               "ignore" = {
                 ds %>% mutate(across(all_of(outcome_date), ~case_when(. < !!sym(target_start_date) ~ NA, TRUE ~ .)))
                 },
               "bring_forward" = {
                 ds %>% mutate(across(all_of(outcome_date), ~case_when(. < !!sym(target_start_date) ~ !!sym(target_start_date), TRUE ~ .)))
                },
               "remove_patients" = {
                 ds %>% filter(!has_prior_outcome)
                }
                )
  
  ds <- ds %>%
    filter(is.na(!!sym(outcome_date)) | !!sym(outcome_date) >= !!sym(target_start_date)) %>%
    arrange(is.na(!!sym(outcome_date)), !!sym(outcome_date)) %>%
    #group_by(subject_id) %>%
    #slice(1) %>%
    #ungroup() %>%
    mutate(
      flag = !is.na(!!sym(outcome_date)) & !!sym(outcome_date) <= !!sym(target_censor_date),
      time = as.integer(pmin(!!sym(outcome_date), !!sym(target_censor_date), na.rm = TRUE) - !!sym(target_start_date))
    ) %>%
    select(subject_id, flag, time, all_of(target_strata))
  
  # Create survival object
  surv_object <- Surv(ds$time, ds$flag)
  
  formula <- if (!is.null(target_strata)) {
    as.formula(paste("surv_object ~", str_c(target_strata, collapse = " + ")))
  } else {
    as.formula("surv_object ~ 1")
  }
  
  fit <- survfit(formula, data = ds)
  surv_data <- broom::tidy(fit, time = TRUE) %>% filter(estimate < 1)
  
  if (is.null(target_strata)) {
    surv_data$strata <- "All"
  }
  
  surv_plot <- surv_data %>%
    mutate(estimate = case_when(plot_incidence == T ~ 1-estimate, TRUE ~ estimate)) %>%
    ggplot(aes(x = time, y = estimate, colour = strata)) +
    geom_step() +
    theme_bw() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(y = case_when(plot_incidence == T ~ "CumulativeIncidence", TRUE ~ "Survival"))
  


  median_survival <- surv_median(fit)
  
  if(is.null(fit$strata)) median_survival$n <- fit$n
  if(!is.null(fit$strata)) median_survival <- left_join(median_survival, tibble(strata = names(fit$strata), n = fit$n))
  
  median_ttp <- ds %>%
    group_by(across(all_of(target_strata))) %>%
    summarise(count = n(), n.event = sum(flag), median_time = median(time[flag == 1]), q25 = quantile(time[flag == 1], 0.25), q75 = quantile(time[flag == 1], 0.75), .groups = "drop")
  
  return(list(
    survival = list(data = surv_data, plot = surv_plot), 
    plain = median_ttp,
    median_survival = median_survival
  ))
}

for(guideline in c("4","5","6")){
for(strata in (c("all","age_group", "gender_concept_id"))) {
    
  save_dir <- file.path(outputFolder, "study_results", "time_to_event", strata,  str_c("guideline_",guideline))
  
  dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
  
  if(strata == "all") strata = NULL

  ###############
  ## TIME TO TREATMENT FROM MBC TO TREATMENT
  ###############
  for(cohort in c("Target_1A","Target_1B")) {
    
    target_cohort_2_id <- preparedCohortManifest %>%
      filter(cohortName == cohort) %>%
      pull(cohortId)
    
    # Extract target cohort
    target <- all_patients %>%  
      filter(cohort_definition_id == target_cohort_2_id)
    
    # Processing the episode data correctly
    outcome <- target %>%
      inner_join(episodes %>% rename(subject_id = person_id)) %>% 
      filter(episode_end_date >= cohort_start_date) %>%
      arrange(subject_id, episode_start_date) %>%
      slice(1, .by = subject_id) %>%  
      mutate(outcome_cohort_start_date = pmax(cohort_start_date, episode_start_date)) %>%
      select(-cohort_start_date, -cohort_end_date, -cohort_definition_id, -strata) 
    
    # Ignore prior outcomes (original behavior)
    time_to_treatment_outputs <- cohort_surv(plot_incidence = T, target_strata = strata)
    
    time_to_treatment_outputs$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_km_data_", cohort, ".csv")))
    
    time_to_treatment_outputs$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_stats_", cohort, ".csv")  ))

    time_to_treatment_outputs$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_median_survival_", cohort, ".csv")  ))
    
  }
  
  ###############
  ## EPISODE DATASET
  ###############
  
  for(cohort in c("Target_1A_initiated_base","Target_1B_initiated_base")) {
    
    target_cohort_3_id <- preparedCohortManifest %>%
      filter(cohortName == cohort) %>%
      pull(cohortId)
    
    death_cohort_id <- preparedCohortManifest %>%
      filter(str_detect(cohortName, "^Death")) %>%
      pull(cohortId)
    
    treated_patients <- all_patients %>% 
      filter(cohort_definition_id == target_cohort_3_id) %>%
      left_join(select(episodes, subject_id = person_id, episode_start_date, episode_end_date, episode_source_value, episode_number, episode_object_concept_id)) %>%
      filter(episode_end_date >= cohort_start_date) %>%
      mutate(episode_start_date = pmax(episode_start_date, cohort_start_date)) %>%
      arrange(strata, subject_id, episode_start_date) %>%
      mutate(episode_number = row_number(), .by = subject_id) %>%
      left_join(regimen_categories, by = c("episode_source_value" = "regName")) %>%
      mutate(category = coalesce(!!sym(str_c("cohort_T",guideline)), "Not categorized")) %>%
      mutate(
        time_since_last_treatment = episode_start_date - lag(episode_end_date,1), 
        time_to_next_treatment = lead(episode_start_date,1) - episode_end_date,
        .by = subject_id
      )
    
    ##############
    ## TIME TO TREATMENT DISCONTINUATIOn
    ###############
    target <- treated_patients %>%
      select(strata, subject_id, episode_number, episode_start_date, cohort_end_date, category) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      filter(episode_number <= 3) %>%
      mutate(episode_number = str_c("Line ",as.character(episode_number)))
    
    outcome <- treated_patients %>%
      select(subject_id, episode_number, episode_end_date) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      select(-episode_number)
    
    time_to_treatment_discontinuation_total <- cohort_surv(target_start_date = "episode_start_date", target_censor_date = "cohort_end_date", outcome_date = "episode_end_date", target_strata = c(strata,"episode_number"))
    
    time_to_treatment_discontinuation_total$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_total_km_data_", cohort, ".csv")))
    
    time_to_treatment_discontinuation_total$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_total_stats_", cohort, ".csv")))
    
    time_to_treatment_discontinuation_total$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_total_median_survival_", cohort, ".csv")))

    time_to_treatment_discontinuation_category <- cohort_surv(target_start_date = "episode_start_date", target_censor_date = "cohort_end_date", outcome_date = "episode_end_date", target_strata = c(strata, "episode_number","category"))
    
    time_to_treatment_discontinuation_category$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_category_km_data_", cohort, ".csv")))
    
    time_to_treatment_discontinuation_category$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_category_stats_", cohort, ".csv")))
    
    time_to_treatment_discontinuation_category$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("time_to_treatment_discontinuation_category_median_survival_", cohort, ".csv")))

    ##############
    ## TIME TO NEXT TREATMENT
    ###############
    target <- treated_patients %>%
      select(strata, subject_id, episode_number, episode_start_date, cohort_end_date, category) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      filter(episode_number <= 3) %>%
      mutate(episode_number = str_c("Line ",as.character(episode_number)))
    
    outcome <- treated_patients %>%
      mutate(next_treatment_date = episode_end_date + time_to_next_treatment) %>%
      select(subject_id, episode_number, next_treatment_date) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      select(-episode_number)
    
    time_to_next_treatment_total <- cohort_surv(target_start_date = "episode_start_date", target_censor_date = "cohort_end_date", outcome_date = "next_treatment_date", target_strata = c(strata,"episode_number"))
    
    time_to_next_treatment_total$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_total_km_data_", cohort, ".csv")))
    
    time_to_next_treatment_total$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_total_stats_", cohort, ".csv")))
    
    time_to_next_treatment_total$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_total_median_survival_", cohort, ".csv")))

    time_to_next_treatment_category <- cohort_surv(target_start_date = "episode_start_date", target_censor_date = "cohort_end_date", outcome_date = "next_treatment_date", target_strata = c(strata,"episode_number","category"))
    
    time_to_next_treatment_category$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_category_km_data_", cohort, ".csv")))  
    
    time_to_next_treatment_category$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_category_stats_", cohort, ".csv")  ))
    
    time_to_next_treatment_category$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("time_to_next_treatment_category_median_survival_", cohort, ".csv")))

    ##############
    ## TREATMENT FREE INTERVAL
    ###############
    target <- treated_patients %>%
      select(strata, subject_id, episode_number, episode_end_date, cohort_end_date, category) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      filter(episode_number <= 3) %>%
      mutate(episode_number = str_c("Line ",as.character(episode_number)))
    
    outcome <- treated_patients %>%
      mutate(next_treatment_date = episode_end_date + time_to_next_treatment) %>%
      select(subject_id, episode_number, next_treatment_date) %>%
      mutate(subject_id = str_c(as.character(subject_id),"_",as.character(episode_number))) %>%
      select(-episode_number)
    
    treatment_free_interval_total <- cohort_surv(target_start_date = "episode_end_date", target_censor_date = "cohort_end_date", outcome_date = "next_treatment_date", target_strata = c(strata,"episode_number"))
    
    treatment_free_interval_total$survival$data %>%  
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_total_km_data_", cohort, ".csv")))
    
    treatment_free_interval_total$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_total_stats_", cohort, ".csv")))
    
    treatment_free_interval_total$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_total_median_survival_", cohort, ".csv")))

    treatment_free_interval_category <- cohort_surv(target_start_date = "episode_end_date", target_censor_date = "cohort_end_date", outcome_date = "next_treatment_date", target_strata = c(strata,"episode_number","category"))
    
    treatment_free_interval_category$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_category_km_data_", cohort, ".csv")))
    
    treatment_free_interval_category$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_category_stats_", cohort, ".csv")))
    
    treatment_free_interval_category$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("treatment_free_interval_category_median_survival_", cohort, ".csv")))
    
    ###############
    ## TREATED SURVIVAL
    ###############
    
    target <- treated_patients %>%
      filter(episode_number == 1) %>%
      mutate(treatment_year = str_c(year(cohort_start_date)))
    
    outcome <- all_patients %>%
      filter(cohort_definition_id == death_cohort_id) %>%
      select(subject_id, outcome_cohort_start_date = cohort_start_date)
    
    overall_survival_treated_cohort <- cohort_surv(target_start_date = "cohort_start_date", target_censor_date = "cohort_end_date", outcome_date = "outcome_cohort_start_date", target_strata = strata, prior_outcome_handling = "remove_patients")
    
    overall_survival_treated_cohort$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_km_data_", cohort, ".csv")))
    
    overall_survival_treated_cohort$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_stats_", cohort, ".csv")))
    
    overall_survival_treated_cohort$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_median_survival_", cohort, ".csv")))

    overall_survival_treated_cohort_year <- cohort_surv(target_start_date = "cohort_start_date", target_censor_date = "cohort_end_date", outcome_date = "outcome_cohort_start_date", target_strata = c(strata,"treatment_year"), prior_outcome_handling = "remove_patients")
    
    overall_survival_treated_cohort_year$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_year_km_data_", cohort, ".csv")))
    
    overall_survival_treated_cohort_year$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_year_stats_", cohort, ".csv")))
    
    overall_survival_treated_cohort_year$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_treated_cohort_year_median_survival_", cohort, ".csv")))

    overall_survival_starting_treatment <- cohort_surv(target_start_date = "cohort_start_date", target_censor_date = "cohort_end_date", outcome_date = "outcome_cohort_start_date", target_strata = c(strata,"category"), prior_outcome_handling = "remove_patients")
    
    overall_survival_starting_treatment$survival$data %>%
      mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_starting_treatment_km_data_", cohort, ".csv")))
    
    overall_survival_starting_treatment$plain %>%
      mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_starting_treatment_stats_", cohort, ".csv")))
    
    overall_survival_starting_treatment$median_survival %>%
      mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
      mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
      write_csv(file.path(save_dir, str_c("overall_survival_starting_treatment_median_survival_", cohort, ".csv"))) 

  } 
  
  ###############
  ## OVERALL SURVIVAL
  ###############
  
  target_cohort_ids <- preparedCohortManifest %>%
    filter(cohortName %in% c("Target_1A_initiated_base", "Target_1B_initiated_base", "Target_1A_initiated_L01", "Target_1B_initiated_L01","Target_1A","Target_1B")) %>%
    select(cohortId, cohortName)
  
  target <- all_patients %>%
    inner_join(target_cohort_ids, by = c("cohort_definition_id" = "cohortId")) %>%
    select(strata, subject_id, cohort_start_date, cohort_end_date, cohortName) 
  
  outcome <- all_patients %>%
    filter(cohort_definition_id == death_cohort_id) %>%
    select(subject_id, outcome_cohort_start_date = cohort_start_date)
  
  overall_survival <- cohort_surv(target_start_date = "cohort_start_date", target_censor_date = "cohort_end_date", outcome_date = "outcome_cohort_start_date", target_strata = c(strata,"cohortName"), prior_outcome_handling = "remove_patients")
  
  overall_survival$survival$data %>%
    mutate(across(c(n.risk, n.censor, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
    write_csv(file.path(save_dir, "overall_survival_km_data.csv"))
  
  overall_survival$plain %>%
    mutate(across(c(count, n.event), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
    write_csv(file.path(save_dir, "overall_survival_stats.csv"))

  overall_survival$median_survival %>%
    mutate(across(c(n), ~case_when(.x > 0 & .x < minCellCount ~ -minCellCount, TRUE ~ .x))) %>%
    mutate(across(c(median, upper, lower), ~case_when(n >= 0 ~ .x, TRUE ~ NA))) %>%
    write_csv(file.path(save_dir, "overall_survival_median_survival.csv"))
  
}
}