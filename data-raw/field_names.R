library(glue)
library(dplyr)
library(stringr)

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



# antDeps ------------------------------------------------------------------
antDeps <- field_names %>%
  filter(str_detect(table, "antdeps"),
         !column %in% c("projectID", "deviceID")) %>%
  mutate(table = "antDeps",
         keys = column %in% c("deployID", "port"),
         nulls = FALSE,
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
         references = if_else(column == "batchID", "batches", ""))


# hits --------------------------------------------------------------------
# "hits" table applies to hits_for_receiver and hits_for_tag_project
hits <- field_names %>%
  filter(str_detect(table, "hits_for_tag"),
         !column %in% c("projectID")) %>%
  mutate(table = "hits",
         keys = column == "hitID",
         not_nulls = column %in% c("runID", "batchID", "ts", "sig"),
         references = case_when(column == "runID" ~ "runs",
                                column == "batchID" ~ "batches"))


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
         defaults = if_else(column == "done", 0, NA))

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
         not_null = column == "ambigID")

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
         nulls = column = "propValue",
         extra = list(c("CREATE INDEX IF NOT EXISTS tagProps_deployID ON tagProps (deployID ASC);")))


# tags --------------------------------------------------------------------
tags<- field_names %>%
  filter(str_detect(table, "metadata_for_tags$")) %>%
  mutate(table = "tags",
         keys = column == "tagID",
         extra = list(c("CREATE INDEX IF NOT EXISTS tags_projectID on tags(projectID)")))





if (! "runsFilters" %in% tables) {
  sql("
CREATE TABLE runsFilters (
   filterID INTEGER NOT NULL,               # locally unique filterID
   runID INTEGER NOT NULL,                  # unique ID of the run record to which the filter applies
   motusTagID INTEGER NOT NULL,             # unique ID of the Motus tag. Should match the actual motusTagID, not the negative ambigID in the case of ambiguous runs.
   probability REAL NOT NULL,               # probability (normally between 0 and 1) attached to the run record
   PRIMARY KEY(filterID,runID,motusTagID)
);
");
  sql("
CREATE INDEX IF NOT EXISTS runsFilters_filterID_runID_motusTagID ON runsFilters (
  filterID ASC, 
  runID ASC, 
  motusTagID ASC,
  probability ASC);
")
  
}

if(! "nodeData" %in% tables) {
  sql(makeTables(type = "nodeData"))
}

if(! "nodeDeps" %in% tables) {
  sql("CREATE TABLE IF NOT EXISTS nodeDeps (
      deployID INTEGER NOT NULL,
      nodeDeployID BIGINT PRIMARY KEY NOT NULL, 
      latitude  FLOAT, 
      longitude FLOAT, 
      tsStart FLOAT NOT NULL, 
      tsEnd FLOAT NOT NULL);")
}

updateMotusDb(src, quiet = quiet)
rv = makeAllambigsView(src)
rv = makeAlltagsView(src)
rv = makeAlltagsGPSView(src)
return(rv)
}






# NOT FROM SCHEME ---------------------------------------------------------



# admInfo --------------------------------------------------------------------
admInfo <- tribble(~name,          ~type,
                   "db_version",   "INTEGER",
                   "data_version", "TEXT") %>%
  mutate(table = "admInfo")

# batchRuns --------------------------------------------------------------------
batchRuns <- tribble(~name,     ~type,
                     "batchID", "INTEGER NOT NULL",
                     "runID",   "INTEGER NOT NULL") %>%
  mutate(extra = list(c("create index batchRuns_batchID on batchRuns ( batchID )",
                        "create index batchRuns_runID on batchRuns ( runID )")))


# clarified ------------------------------------------------------------------
clarified <- field_names %>%
  filter(str_detect(table, "clar"),
         !column %in% c("version")) %>%
  mutate(table = "batches",
         keys = column == "batchID")


if (! "clarified" %in% tables) {
  sql("
CREATE TABLE clarified (
   ambigID INTEGER,
   tagID INTEGER,
   tsStart REAL,
   tsEnd REAL)
")
  sql("
CREATE INDEX IF NOT EXISTS clarified_ambigID_tsStart ON clarified(ambigID, tsStart)
")
}


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
         extra = list(c(paste0("CREATE UNIQUE INDEX IF NOT EXISTS ",
                               "filters_filterName_motusProjID ON filters (",
                               "filterName ASC, motusProjID ASC);"))))

# projs -------------------------------------------------------------------
field_names <- dplyr::bind_cols(
  table = "projs", 
  dplyr::tribble(
    ~name,                ~type,
    "id",                 "INTEGER PRIMARY KEY NOT NULL",
    "name",               "TEXT",
    "label",              "TEXT",
    "tagsPermissions",    "INTEGER",
    "sensorsPermissions", "INTEGER"
  )) %>%
  dplyr::bind_rows(field_names, .)



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
  ~column,             ~type,
  "tagDepProjectID", "INTEGER NOT NULL",          # project ID
  "batchID",         "INTEGER NOT NULL",          # unique identifier for batch
  "maxHitID",        "INTEGER NOT NULL",          # unique identifier for largest hit we have for this tagDepProjectID, batchID
  "PRIMARY KEY",     "(tagDepProjectID, batchID)")


# recvs --------------------------------------------------------------------
recvs <- recvDeps %>%
  filter(column %in% c("serno", "deviceID")) %>%
  mutate(table = "recvs",
         keys = column == "deviceID",
         not_nulls = column == "deviceID")




makeTables <- function(type, name = type) {
  if(type == "tagAmbig") {
    s <- paste("CREATE TABLE ", name, " (
    ambigID INTEGER PRIMARY KEY NOT NULL,  # identifier of group of tags which are ambiguous (identical). Will be negative
    masterAmbigID INTEGER,                 # master ID of this ambiguity group, once different receivers have been combined
    motusTagID1 INT,                       # motus ID of tag in group.
    motusTagID2 INT,                       # motus ID of tag in group.
    motusTagID3 INT,                       # motus ID of tag in group.
    motusTagID4 INT,                       # motus ID of tag in group.
    motusTagID5 INT,                       # motus ID of tag in group.
    motusTagID6 INT,                       # motus ID of tag in group.
    ambigProjectID INT                     # negative ambiguity ID of deployment project. refers to key ambigProjectID in table projAmbig
);")
  } else if(type == "nodeData") {
    s <- paste("CREATE TABLE IF NOT EXISTS ", name, " (",
               "nodeDataID BIGINT PRIMARY KEY NOT NULL,",
               "batchID INTEGER NOT NULL,",
               "ts FLOAT NOT NULL,",
               "nodeNum TEXT NOT NULL,",
               "ant TEXT NOT NULL,",
               "sig FLOAT(24),",
               "battery FLOAT,",
               "temperature FLOAT,",
               "nodets FLOAT,",
               "firmware VARCHAR(20),",
               "solarVolt FLOAT,",
               "solarCurrent FLOAT,",
               "solarCurrentCumul FLOAT,",
               "lat FLOAT,",
               "lon FLOAT);")
  } else s <- character()
  s
}
