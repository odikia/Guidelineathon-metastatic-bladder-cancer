library(ggplot2)
library(dplyr)  
library(DBI)   
library(lubridate)
library(SqlRender)
library(stringr)
library(readr)

getCohortAttritionViewResults <- function(inclusionResultTable, maxRuleId) {
  numberToBitString <- function(numbers) {
    vapply(numbers, function(number) {
      if (number == 0) {
        return("0")
      }
      bitString <- character()
      while (number > 0) {
        bitString <- c(as.character(number %% 2), bitString)
        number <- number %/% 2
      }
      paste(bitString, collapse = "")
    }, character(1))
  }
  
  bitsToMask <- function(bits) {
    positions <- seq_along(bits) - 1
    number <- sum(bits * 2 ^ positions)
    return(number)
  }
  
  ruleToMask <- function(ruleId) {
    bits <- rep(1, ruleId)
    mask <- bitsToMask(bits)
    return(mask)
  }
  
  inclusionResultTable <- inclusionResultTable %>%
    dplyr::mutate(inclusionRuleMaskBitString = numberToBitString(inclusionRuleMask))
  
  output <- c()
  
  for (i in (1:maxRuleId)) {
    suffixString <- numberToBitString(ruleToMask(i))
    output[[i]] <- inclusionResultTable %>%
      dplyr::filter(
        endsWith(
          x = inclusionRuleMaskBitString,
          suffix = suffixString
        )
      ) %>%
      dplyr::group_by(
        cohortDefinitionId,
        modeId
      ) %>%
      dplyr::summarise(
        personCount = sum(personCount),
        .groups = "drop"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(id = i)
  }
  
  output <- dplyr::bind_rows(output)
  output <- bind_rows(
    dplyr::slice(output, 1) %>% 
      mutate(
        id = 0,
        personCount = sum(inclusionResultTable$personCount)
      ),
    output
  )
  
  return(output)
}

#preparedCohortManifest <- prepManifestForCohortGenerator(getCohortManifest())

target_cohort_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName, "^Target"))

attrition <- map_df(target_cohort_ids$cohortId, function(cohortId) {

  example <- read_csv(file.path(outputFolder, "cohort_inc_result.csv")) %>%
    filter(cohort_id == cohortId, mode_id == 1)
  
  
  rules <- read_csv(file.path(outputFolder, "cohort_inclusion.csv")) %>%
    filter(cohort_id == cohortId)
  
  colnames(rules) <- SqlRender::snakeCaseToCamelCase(colnames(rules))
  
  rules <- rules %>%
    mutate(id = ruleSequence + 1) %>%
    select(id, name)
  
  colnames(example) <- SqlRender::snakeCaseToCamelCase(colnames(example))
  
  getCohortAttritionViewResults(
    rename(example, cohortDefinitionId = cohortId),
    5
  ) %>%
    left_join(rules)
}) 

attrition <- attrition %>%
    mutate(personCount = case_when(personCount < minCellCount ~ -minCellCount, TRUE ~ personCount))

write_csv(attrition, file.path(outputFolder, "study_results", "attrition.csv"))

index_event_cohort_ids <- preparedCohortManifest %>%
  filter(str_detect(cohortName, "^Target") | str_detect(cohortName, "^Prior_Respiratory_Tumour"))

index_event_breakdown <- read_csv(file.path(outputFolder, "index_event_breakdown.csv")) %>% 
    inner_join(index_event_cohort_ids %>% select(cohortId, cohortName), by = c("cohort_id" = "cohortId")) %>%
    select(cohortName, cohort_id, domain_field, concept_id, subject_count) %>% 
    arrange(cohort_id, desc(subject_count)) 

## Some weirdness means someimtes cohort is not being generated during cohort diagnostics
if(file.exists(file.path(outputFolder, "study_results", "concept.csv"))){
  index_event_breakdown <- index_event_breakdown %>%
    left_join(read_csv(file.path(outputFolder, "study_results", "concept.csv")) %>% 
        select(concept_id, concept_name)) 
}

index_event_breakdown %>%
    mutate(subject_count = case_when(subject_count > 0 & subject_count < minCellCount ~ -minCellCount, TRUE ~ subject_count)) %>%
    write_csv(file.path(outputFolder, "study_results", "index_event_breakdown.csv"))
