# A. File Info -----------------------

# Task: Run ARTEMNIS
# Description: The purpose of the _runARTEMIS.R script is to run ARTEMIS and create an episode table
library(tidyverse)
library(ARTEMIS)

# B. Functions ------------------------
runARTEMIS <- function(
    connectionDetails,
    cdmSchema,
    cohortTable,
    cohortSchema,
    cohortManifestRow,
    regimens =  ARTEMIS::loadRegimens(condition = "lungCancer"),
    validDrugs = ARTEMIS::loadDrugs()
){
  
  conDF <- getConDF(connectionDetails = connectionDetails,
                    json = cohortManifestRow,
                    name = cohortTable,
                    cdmSchema = cdmSchema,
                    writeSchema = cohortSchema)
  
  stringDF <- stringDF_from_cdm(con_df = conDF,
                                writeOut = F,
                                validDrugs = validDrugs)
  
  output_all <- generateRawAlignments(stringDF,
                                      regimens = regimens,
                                      g = 0.4,
                                      Tfac = 0.5,
                                      verbose = 0,
                                      mem = -1,
                                      removeOverlap = 1,
                                      method = "PropDiff")
  
  processedAlignments <- processAlignments(output_all,
                                           regimenCombine = 28,
                                           regimens = regimens)
  
  processedEras <- calculateEras(processedAlignments, discontinuationTime = 120)
  
  reference_dates <- conDF[conDF$ancestor_concept_id %in% validDrugs$valid_concept_id,] %>%
    summarise(refDate = min(drug_exposure_start_date), .by = person_id) %>%
    rename(personID = person_id) %>%
    mutate(across(personID, as.character))
  
  
  sql_template <- "SELECT concept_name, concept_id 
                   FROM @cdmSchema.concept 
                   WHERE concept_class_id = 'Regimen' 
                   AND standard_concept = 'S'"
  
  rendered_sql <- SqlRender::render(sql_template, cdmSchema = cdmSchema)
  
  connection <- DatabaseConnector::connect(connectionDetails)
  
  regimen_concepts <- DatabaseConnector::dbGetQuery(
    conn = connection,
    statement = rendered_sql 
  )
  
  DatabaseConnector::disconnect(connection)
  
  start_episode_id <- 1
  
  episode <- processedEras %>%
    left_join(reference_dates) %>%
    mutate(concept_name = str_replace_all(component, "&", "and")) %>%
    left_join(regimen_concepts, relationship = "many-to-many") %>%
    arrange(personID, t_start) %>%
    mutate(episode_id = start_episode_id + row_number() - 1) %>%
    mutate(
      person_id = as.integer(personID),
      episode_concept_id = 32941,
      episode_start_date = refDate + days(t_start),
      episode_start_datetime = NA_POSIXct_,
      episode_end_date = refDate + days(t_end),
      episode_end_datetime = NA_POSIXct_,
      episode_parent_id = NA_integer_,
      episode_number = row_number(),
      episode_object_concept_id = concept_id,
      episode_type_concept_id = 0,
      episode_source_value = component,
      episode_source_concept_id = 0,
      .by = personID
    ) %>%
    select(episode_id:episode_source_concept_id)
  
  return(list(episodes = episode, eras = processedEras, raw_alignments = output_all, valid_drug_exposures = conDF[conDF$ancestor_concept_id %in% validDrugs$valid_concept_id, ]
))
}