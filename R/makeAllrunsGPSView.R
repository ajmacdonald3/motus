#' Create allruns view
#'
#' Creates a virtual table (really a 'view') in a motus database that
#' links each tag detection to all metadata available for the tag and
#' receiver. The allruns view is the same as alltags but excludes hits.
#'
#' @param db dplyr src_sqlite to detections database
#' @param name character scalar; name for the virtual table.
#'     Default: 'alltags'.
#'
#' @return a dplyr::tbl which refers to the newly-created virtual table.
#' By default, the columns in the virtual table are:
#' \itemize{
#'    \item{runID} unique motus ID for the run this detection belongs to
#'    \item{batchID} unique motus ID for the processing batch this detection came from
#'    \item{ts} timestamp, in seconds since 1 Jan, 1970 GMT
#'    \item{sig} signal strength, in dB (max) for SG; raw value for Lotek receiver
#'    \item{sigsd} sd among pulses of signal strength (SG); NA for Lotek
#'    \item{noise} noise strength, in dB (max) for SG; NA for Lotek
#'    \item{freq} offset in kHz from listening frequency for SG; NA for Lotek
#'    \item{freqsd} sd among pulses of offset in kHz from listening frequency for SG; NA for Lotek
#'    \item{slop} total absolute difference (milliseconds) in intrer-pulse gaps between registration and detection for SG; NA for Lotek
#'    \item{burstSlop} signed difference (seconds) between detection and registration burst intervals
#'    \item{done} logical: is run finished?
#'    \item{motusTagID} unique motus ID for this physical tag
#'    \item{ambigID} unique ID linking ambiguous tag detections
#'    \item{port} antenna port number
#'    \item{runLen} length of run (# of bursts detected)
#'    \item{bootnum} boot session of receiver for SG; NA for Lotek
#'    \item{tagProjID} unique motus ID for project tag was deployed by
#'    \item{mfgID} manufacturer ID printed on the tag
#'    \item{tagType} type of tag
#'    \item{codeSet} for coded ID tags, the name of the code set
#'    \item{mfg} tag manufacturer
#'    \item{tagModel} manufacturer's model name for tag
#'    \item{tagLifespan} estimated tag lifespan
#'    \item{nomFreq} nominal tag frequency (MHz)
#'    \item{tagBI} tag burst interval (seconds)
#'    \item{pulseLen} tag pulse length (milliseconds) if applicable
#'    \item{tagDeployID} unique motusID for tag deployment
#'    \item{speciesID} unique motusID for species tag was deployed on
#'    \item{markerNumber} number for additional marker placed on organism (e.g. bird band #)
#'    \item{markerType} type of additional marker
#'    \item{tagDepLat} latitude of tag deployment, in decimal degrees N
#'    \item{tagDepLon} longitude of tag deployment, in decimal degrees E
#'    \item{tagDepAlt} altitude of tag deployment, in metres ASL
#'    \item{tagDepComments} additional comments or unclassified metadata for tag (often in JSON format)
#'    \item{fullID} full tag ID as PROJECT#MFGID:BI@NOMFREQ (but this is not necessarily unique over time; see motusTagID for a unique tag id)
#'    \item{deviceID} unique motusID for the device (normally matches with a unique receiver serial number)
#'    \item{recvDeployID} unique motusID for the receiver deployment
#'    \item{recvDeployLat} latitude of receiver deployment, in decimal degrees
#'    \item{recvDeployLon} longitude of receiver deployment, in decimal degrees
#'    \item{recvDeployAlt} altitude of receiver deployment, in metres ASL
#'    \item{recv} serial number of receiver; e.g. SG-1234BBBK5678 or Lotek-12345
#'    \item{recvDeployName} name assigned to the receiver deployment
#'    \item{recvSiteName} name assigned to the site where the receiver deployment is located
#'    \item{isRecvMobile} whether the receiver is mobile or not
#'    \item{recvProjID} unique motus ID for project receiver was deployed by
#'    \item{antType} character; antenna type; e.g. "omni", "yagi-5", ...
#'    \item{antBearing} numeric; direction antenna main axis points in; degrees clockwise from local magnetic north
#'    \item{antHeight} numeric; height (metres) of antenna main axis above ground
#'    \item{speciesEN} species name in english
#'    \item{speciesFR} species name in french
#'    \item{speciesSci} species scientific name
#'    \item{speciesGroup} species group
#'    \item{tagProjName} short label of project that deployed tag
#'    \item{recvProjName} short label of project that deployed receiver
#' }
#'
#' @note The new virtual table replaces any previous virtual table by the same
#' name in \code{db}.  The virtual table is an SQL VIEW, which will persist in \code{db}
#' across R sessions.
#'
#' @noRd
#'

makeAllrunsGPSView <- function(db, name = "allrunsGPS") {
  query = glue::glue("
  CREATE VIEW IF NOT EXISTS {name} 
  AS
  SELECT
    t2.runID as runID,
    t3.batchID as batchID,
    t2.done as done,
    CASE WHEN t12.motusTagID is null then t2.motusTagID else t12.motusTagID end as motusTagID,
    t12.ambigID as ambigID,
    t2.ant as port,
    t2.len as runLen,
    t2.tsBegin as tsBegin,
    t2.tsEnd as tsEnd,
    CASE WHEN t6.utcOffset is null then t2.tsBegin else t2.tsBegin - t6.utcOffset * 60 * 60 end as tsBeginCorrected,
    CASE WHEN t6.utcOffset is null then t2.tsEnd else t2.tsEnd - t6.utcOffset * 60 * 60 end as tsEndCorrected,
    t3.monoBN as bootnum,
    t4.projectID as tagProjID,
    t4.mfgID as mfgID,
    t4.type as tagType,
    t4.codeSet as codeSet,
    t4.manufacturer as mfg,
    t4.model as tagModel,
    t4.lifeSpan as tagLifespan,
    t4.nomFreq as nomFreq,
    t4.bi as tagBI,
    t4.pulseLen as pulseLen,
    t5.deployID as tagDeployID,
    t5.speciesID as speciesID,
    t5.markerNumber as markerNumber,
    t5.markerType as markerType,
    t5.tsStart as tagDeployStart,
    t5.tsEnd as tagDeployEnd,
    t5.latitude as tagDepLat,
    t5.longitude as tagDepLon,
    t5.elevation as tagDepAlt,
    t5.comments as tagDepComments,
    t5.test as tagDeployTest,
    ifnull(t5.fullID, printf('?proj?-%d#%s:%.1f', t5.projectID, t4.mfgID, t4.bi)) as fullID,
    t3.motusDeviceID as deviceID,
    t6.deployID as recvDeployID,
    t6.latitude as recvDeployLat,
    t6.longitude as recvDeployLon,
    t6.elevation as recvDeployAlt,
    t6a.serno as recv,
    t6.name as recvDeployName,
    t6.siteName as recvSiteName,
    t6.isMobile as isRecvMobile,
    t6.projectID as recvProjID,
    t6.utcOffset as recvUtcOffset,
    t7.antennaType as antType,
    t7.bearing as antBearing,
    t7.heightMeters as antHeight,
    t8.english as speciesEN,
    t8.french as speciesFR,
    t8.scientific as speciesSci,
    t8.`group` as speciesGroup,
    t9.label as tagProjName,
    t10.label as recvProjName,
    t11.lat as gpsLat,
    t11.lon as gpsLon,
    t11.alt as gpsAlt
  FROM
     runs AS t2
 
  LEFT JOIN 
    allambigs t12 on t2.motusTagID = t12.ambigID
 
  LEFT JOIN
    batchRuns AS t3a ON t2.runID = t3a.runID
 
  LEFT JOIN
    batches AS t3 ON t3.batchID = t3a.batchID
 
  LEFT JOIN
    tags AS t4 ON t4.tagID = CASE WHEN t12.motusTagID is null then t2.motusTagID else t12.motusTagID end
 
  LEFT JOIN
    tagDeps AS t5 ON t5.tagID = CASE WHEN t12.motusTagID is null then t2.motusTagID else t12.motusTagID end
      AND t5.tsStart =
         (SELECT
             max(t5b.tsStart)
          FROM
             tagDeps AS t5b
          WHERE
             t5b.tagID = CASE WHEN t12.motusTagID is null then t2.motusTagID else t12.motusTagID end
             AND t5b.tsStart <= t2.tsBegin
             AND (t5b.tsEnd IS NULL OR t5b.tsEnd >= t2.tsBegin)
         )
  LEFT JOIN
    recvs as t6a on t6a.deviceID =t3.motusDeviceID
  
  LEFT JOIN
    recvDeps AS t6 ON t6.deviceID = t3.motusDeviceID 
      AND t6.tsStart =
         (SELECT
             max(t6b.tsStart)
          FROM
             recvDeps AS t6b
          WHERE
             t6b.deviceID=t3.motusDeviceID
             AND t6b.tsStart <= t2.tsBegin
             AND (t6b.tsEnd IS NULL OR t6b.tsEnd >= t2.tsBegin)
         )
 
  LEFT JOIN
    antDeps AS t7 ON t7.deployID = t6.deployID AND t7.port = t2.ant
  
  LEFT JOIN
    species AS t8 ON t8.id = t5.speciesID

  LEFT JOIN
    projs AS t9 ON t9.ID = t5.projectID

  LEFT JOIN
    projs AS t10 ON t10.ID = t6.projectID
  
  LEFT JOIN
    gps AS t11 ON t11.batchID = t3.batchID
      AND t11.ts =
         (SELECT
             max(t11b.ts)
          FROM
             gps AS t11b
          WHERE
             t11b.batchID=t3.batchID
             AND t11b.ts >= t2.tsBegin
         )")
  
  DBI::dbExecute(db$con, query)
  dplyr::tbl(db, name)
}
