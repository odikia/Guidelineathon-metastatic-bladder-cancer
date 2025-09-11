  -- Begin Episode Criteria
select C.person_id, C.episode_id as event_id, C.episode_start_date as start_date,
       COALESCE(C.EPISODE_END_DATE, DATEADD(day,1,C.EPISODE_START_DATE)) as end_date,
       0 AS visit_occurrence_id,C.EPISODE_START_DATE as sort_date
from 
(
  select de.* 
  FROM @target_database_schema.@regimen_episode_table de
  JOIN @target_database_schema.@regimen_classification_table rc 
    ON de.episode_source_value = rc.regName
) C
JOIN @cdm_database_schema.PERSON P on C.person_id = P.person_id
-- End Episode Criteria