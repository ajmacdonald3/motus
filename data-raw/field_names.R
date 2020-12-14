library(glue)
library(dplyr)
library(stringr)


# ** GET FIELDS -----------------------------------------------------------


local_auth()
field_names <- srvSchema() %>%
  rename_all(tolower) %>%
  rename(column = column_name, table = table_name, type = data_type) %>%
  mutate(type = toupper(type),
         table = tolower(table)) %>%
  filter(!str_detect(table, "deprecated"),
         !column %in% c("accessLevel"),
         !str_detect(column, "is_private"))



# activity ------------------------------------------------------------------
activity <- field_names %>%
  filter(str_detect(table, "batches_activity")) %>%
  mutate(table = "activity",
         keys = column %in% c("batchID", "ant", "hourBin"),
         uniques = column %in% c("batchID", "ant", "hourBin"))

# activityAll ------------------------------------------------------------------
activityAll <- activity %>%
  mutate(table = "activityAll")


# antDeps ------------------------------------------------------------------
antDeps <- field_names %>%
  filter(str_detect(table, "antdeps"),
         !column %in% c("projectID", "deviceID")) %>%
  mutate(table = "antDeps",
         keys = column %in% c("deployID", "port"),
         extra = list(c("CREATE INDEX IF NOT EXISTS antDeps_deployID on antDeps(deployID)",
                        "CREATE INDEX IF NOT EXISTS antDeps_port on antDeps(port)")))

# batches ------------------------------------------------------------------
# "batches" table applies to batches_for_receiver and batches_for_tag
batches <- field_names %>%
  filter(str_detect(table, "batches_for_tag"),
         !column %in% c("version")) %>%
  mutate(table = "batches",
         keys = column == "batchID")




# gps --------------------------------------------------------------------
gps <- field_names %>%
  filter(str_detect(table, "gps_for_tag"),
         !column %in% c("motusProjectID", "recvProjectID")) %>%
  mutate(table = "gps",
         keys = column == "gpsID",
         references = if_else(column == "batchID", "batches", ""),
         # Remove empty gps detections
         extra = list(c("DELETE FROM gps where lat = 0 and lon = 0 and alt = 0")))

# gpsAll --------------------------------------------------------------------
gpsAll <- gps %>%
  mutate(table = "gpsAll",
         extra = str_replace(extra, "gps", "gpsAll"))


# hits --------------------------------------------------------------------
# "hits" table applies to hits_for_receiver and hits_for_tag_project
hits <- field_names %>%
  filter(str_detect(table, "hits_for_tag"),
         !column %in% c("projectID")) %>%
  mutate(table = "hits",
         keys = column == "hitID",
         not_nulls = column %in% c("runID", "batchID", "ts", "sig"),
         references = case_when(column == "runID" ~ "runs",
                                column == "batchID" ~ "batches"),
         extra = list(c("CREATE INDEX IF NOT EXISTS hits_batchID_ts on hits(batchID, ts)")))


# nodeData --------------------------------------------------------------------
# "nodeData" table applies to node_data_for_receiver and node_data_for_tag_project
nodeData <- field_names %>%
  filter(str_detect(table, "node_data_for_tag"),
         !column %in% c("projectID", "deviceID")) %>%
  mutate(keys = column == "nodeDataID",
         not_nulls = column %in% c("nodeDataID", "batchID", "ts", "nodeNum", "ant"))

# nodeDeps --------------------------------------------------------------------
nodeDeps <- field_names %>%
  filter(str_detect(table, "nodedeps"),
         !column %in% c("projectID", "deviceID", "nodeNum")) %>%
  mutate(table = "nodeDeps",
         keys = column == "nodeDeployID",
         not_nulls = column %in% c("deployID", "nodeDeployID", "tsStart", "tsEnd"))

# pulseCounts --------------------------------------------------------------------
pulseCounts <- field_names %>%
  filter(str_detect(table, "pulse")) %>%
  mutate(table = "pulseCounts",
         keys = column %in% c("batchID", "ant", "hourBin"),
         not_nulls = column %in% c("batchID", "ant"))

# recvDeps --------------------------------------------------------------------
recvDeps <- field_names %>%
  filter(str_detect(table, "recvdeps")) %>%
  add_row(column = "macAddress", type = "TEXT") %>%
  mutate(table = "recvDeps",
         keys = column == "deployID",
         extra = list(c("CREATE INDEX IF NOT EXISTS recvDeps_serno on recvDeps(serno)",
                        "CREATE INDEX IF NOT EXISTS recvDeps_deviceID on recvDeps(deviceID)",
                        "CREATE INDEX IF NOT EXISTS recvDeps_projectID on recvDeps(projectID)")))

# runs --------------------------------------------------------------------
# "runs" table applies to runs_for_receiver and runs_for_tag_project
runs <- field_names %>%
  filter(str_detect(table, "runs_for_tag"),
         !column %in% c("projectID", "batchID")) %>%
  mutate(table = "runs",
         keys = column == "runID",
         not_nulls = column %in% c("batchIDbegin", "done", "motusTagID", "ant"),
         defaults = if_else(column == "done", 0, as.numeric(NA)))

# species --------------------------------------------------------------------
species <- field_names %>%
  filter(str_detect(table, "tags_species")) %>%
  add_row(column = "sort", type = "INT") %>%
  mutate(table = "species",
         keys = column == "id",
         not_nulls = column %in% c("id"))

# tagAmbig --------------------------------------------------------------------
tagAmbig <- field_names %>%
  filter(str_detect(table, "tags_for_ambiguities"),
         !column %in% c("projectID", "batchID")) %>%
  add_row(column = "masterAmbigID", type = "INT") %>%
  mutate(table = "tagAmbig",
         keys = column == "ambigID",
         not_nulls = column == "ambigID")

# tagDeps --------------------------------------------------------------------
tagDeps <- field_names %>%
  filter(str_detect(table, "tags_deps")) %>%
  add_row(column = "bandNumber", type = "TEXT") %>%
  add_row(column = "id", type = "INT") %>%
  add_row(column = "bi", type = "INT") %>%
  add_row(column = "fullID", type = "INT") %>%
  mutate(table = "tagDeps",
         keys = column == "deployID",
         extra = list(c("CREATE INDEX IF NOT EXISTS tagDeps_projectID on tagDeps(projectID)",
                        "CREATE INDEX IF NOT EXISTS tagDeps_deployID on tagDeps(deployID)")))

# tagProps --------------------------------------------------------------------
tagProps <- field_names %>%
  filter(str_detect(table, "properties"),
         !column %in% c("projectID")) %>%
  mutate(table = "tagProps",
         keys = column == "propID",
         not_nulls = column %in% c("tagID", "deployID", "propName"),
         extra = list(c("CREATE INDEX IF NOT EXISTS tagProps_deployID ON tagProps (deployID ASC)")))


# tags --------------------------------------------------------------------
tags <- field_names %>%
  filter(str_detect(table, "metadata_for_tags$")) %>%
  mutate(table = "tags",
         keys = column == "tagID",
         extra = list(c("CREATE INDEX IF NOT EXISTS tags_projectID on tags(projectID)")))




# ** NOT FROM SCHEME ---------------------------------------------------------



# admInfo --------------------------------------------------------------------
admInfo <- tribble(~column,        ~type,
                   "db_version",   "INTEGER",
                   "data_version", "TEXT") %>%
  mutate(table = "admInfo")

# batchRuns --------------------------------------------------------------------
batchRuns <- tribble(~column,   ~type,
                     "batchID", "INTEGER",
                     "runID",   "INTEGER") %>%
  mutate(table = "batchRuns",
         not_nulls = TRUE,
         extra = list(c("CREATE INDEX batchRuns_batchID on batchRuns (batchID)",
                        "CREATE INDEX batchRuns_runID on batchRuns (runID)")))


# clarified ------------------------------------------------------------------
clarified <- tribble(~column,   ~type,
                     "ambigID", "INTEGER",
                     "tagID",   "INTEGER",
                     "tsStart", "REAL",
                     "tsEnd",   "REAL") %>%
  mutate(table = "clarified",
         extra = list(c("CREATE INDEX IF NOT EXISTS clarified_ambigID_tsStart ON clarified(ambigID, tsStart)")))


# filters ------------------------------------------------------------------
filters <- dplyr::tribble(
  ~column,        ~type,
  "filterID",     "INTEGER",   # locally unique filterID
  "userLogin",    "TEXT",      # motus login of the user who created the filter
  "filterName",   "TEXT",      # short name used to refer to the filter by the user
  "motusProjID",  "INTEGER",   # optional project ID when the filter needs to be shared with other users of a project
  "descr",        "TEXT",      # longer description of what the filter contains
  "lastModified", "TEXT") %>%  # date when the filter was last modified
  mutate(table = "filters",
         keys = column == "filterID",
         not_nulls = column %in% c("userLogin", "filterName", "motusProjID",
                                   "lastModified"),
         extra = list(c("CREATE UNIQUE INDEX IF NOT EXISTS 
                        filters_filterName_motusProjID ON filters 
                        (filterName ASC, motusProjID ASC)")))


# meta --------------------------------------------------------------------
meta <- dplyr::tribble(
  ~column,    ~type,
  "key",      "CHAR",     # name of key for meta data
  "val",      "CHAR") %>% # character string giving meta data; might be in JSON format
  mutate(table = "meta", 
         keys = column == "key",
         not_nulls = column == "key",
         uniques= column == "key")

# projs -------------------------------------------------------------------
projs <- dplyr::tribble(
  ~column,             ~type,
  "id",                 "INTEGER",
  "name",               "TEXT",
  "label",              "TEXT",
  "tagsPermissions",    "INTEGER",
  "sensorsPermissions", "INTEGER") %>%
  mutate(table = "projs", 
         keys = column == "id",
         not_nulls = column == "id")
  



# projAmbig --------------------------------------------------------------------

projAmbig <- dplyr::tribble(
  ~column,             ~type,
  "ambigProjectID",   "INTEGER", 
  "projectID1",       "INTEGER",
  "projectID2",       "INTEGER",
  "projectID3",       "INTEGER",
  "projectID4",       "INTEGER",
  "projectID5",       "INTEGER",
  "projectID6",       "INTEGER") %>%
  mutate(table = "projAmbig",
         keys = column == "ambigProjectID",
         not_nulls = column %in% c("ambigProjectID", "projectID1"))
  
# projBatch --------------------------------------------------------------------
#
# Table for keeping track of which batches we already have, *by*
# tagDepProjectID, and which hits we already have therein. A single batch might
# require several records in this table:  an ambiguous tag detection has
# (negative) tagDepProjectID, which corresponds to a unique set of projects
# which might own the tag detection.

projBatch <- dplyr::tribble(
  ~column,           ~type,
  "tagDepProjectID", "INTEGER",          # project ID
  "batchID",         "INTEGER",          # unique identifier for batch
  "maxHitID",        "INTEGER") %>%      # unique identifier for largest hit we have for this tagDepProjectID, batchID
  mutate(table = "projBatch",
         keys = column %in% c("tagDepProjectID", "batchID"),
         not_nulls = TRUE)


# recvs --------------------------------------------------------------------
recvs <- recvDeps %>%
  filter(column %in% c("serno", "deviceID")) %>%
  mutate(table = "recvs",
         keys = column == "deviceID",
         not_nulls = column == "deviceID")


# runsFilter --------------------------------------------------------------
runsFilter <- dplyr::tribble(
  ~column,       ~type,
  "filterID",    "INTEGER",            # locally unique filterID
  "runID",       "INTEGER",            # unique ID of the run record to which the filter applies
  "motusTagID",  "INTEGER",            # unique ID of the Motus tag. Should match the actual motusTagID, not the negative ambigID in the case of ambiguous runs.
  "probability", "REAL") %>%           # probability (normally between 0 and 1) attached to the run record
  mutate(table = "runsFilter",
         keys = column %in% c("filterID", "runID", "motusTagID"),
         not_nulls = TRUE,
         extra = list("CREATE INDEX IF NOT EXISTS 
                       runsFilters_filterID_runID_motusTagID ON runsFilters 
                       (filterID ASC, runID ASC, motusTagID ASC, probability ASC);"))


# ** COMBINE ---------------------------------------------------------------
sql_tables <- bind_rows(activity, activityAll, antDeps, batches, gps, gpsAll, hits, nodeData, nodeDeps, 
                    pulseCounts, recvDeps, runs, species, tagAmbig, tagDeps, 
                    tagProps, tags, admInfo, batchRuns, clarified, filters, meta,
                    projs, projAmbig, projBatch, recvs, runsFilter) %>%
  as_tibble() %>%
  select(-ordinal_position, -is_nullable) %>%
  mutate(across(c(-defaults, -references), tidyr::replace_na, FALSE),
         not_nulls = if_else(not_nulls, "NOT NULL", ""),
         keys = if_else(keys, "PRIMARY KEY", ""),
         uniques = if_else(uniques, "UNIQUE", ""),
         references = if_else(!is.na(references), glue("REFERENCES {references}"), ""),
         defaults = if_else(!is.na(defaults), glue("DEFAULTS {defaults}"), ""),
         sql = glue("{column} {type} {keys} {uniques} {defaults} {references}"),
         sql = str_remove_all(sql, "[ ]{2,}"),
         extra_sql = purrr::map_chr(extra, 
                                    ~glue_collapse(.[[1]], sep = "; ")),
         extra_sql = if_else(extra_sql == FALSE, "", paste0("\n", extra_sql, ";"))) %>%
  group_by(table) %>%
  summarize(sql = glue("CREATE table {table[1]} ({glue_collapse(sql, sep = \"\n\")});",
                       "{extra_sql[1]}"), .groups = "drop")

