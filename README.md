# Guidelineathon-metastatic-bladder-cancer

### 1.1 Setup  

This study uses `renv` to reproduce the R environment needed. The study code maintains an `renv.lock` file in the main branch of the repository. To activate the R dependencies through renv use `renv::init()`, choosing option 1, or follow `setup.R`.

**Note:** While `renv` is able to fully reproduce the R environment needed for the study, failure to ensure that your pythonb environment is set up correctly may lead to issues with `renv`. The `ARTEMIS` package uses `reticulate` to run python code and should  able to access a suitable python binary (ideally version `3.12`) with `numpy` and `pandas` installed. To set up a minimal virtual python environment, see `setup.R`. If you have issues setting up a suitable python environment, please let us know and we will do our best to help.

**Note2:** This study uses `DatabaseConnector` to connect to the database, which primarily uses a JDBC connection. If you connect to your OMOP database using ODBC, we are happy to assist.

### 1.2 Required Credentials 

Populate the following in `run.R` (see `?DatabaseConnector::createConnectionDetails` for more details):

`databaseName` - a user readable database name.  
`dbms` - the name of the dbms you are using (redshift, postgresql, snowflake, etc).    
`user` - the username credential used to connect to the OMOP database.  
`password` - the password credential used to connect to the OMOP database.  
`server` - the OMOP database server.  
`cdmDatabaseSchema` - the database and schema used to access the cdm.
`vocabDatabaseSchema` - the database and schema used to access the vocabulary tables. Note this is typically the same as the cdmDatabaseSchema.  
`workDatabaseSchema` - a section of the database where the user has read/write access. This schema is where we write the cohortTable used to enumerate the cohort definitions.  
`cohortTable` - the name of the new table where cohort definitions will be enumerated.  
`ARTEMISCohortTable` - the name of the new table where cohort definitions specifically for running ARTEMIS will be enumerated.  
`ARTEMISEpisodeTableName` -  the name of the new table where ARTEMIS treatment episodes will be written.    
