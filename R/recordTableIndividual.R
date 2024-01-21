#' Generate a single-species record table with individual identification from
#' camera trap images
#'
#' The function generates a single-species record table containing individual
#' IDs, e.g. for (spatial) capture-recapture analyses. It prepares input for
#' the function \code{\link{spatialDetectionHistory}}.
#'
#' The function can handle a number of different ways of storing images.
#' In every case, images need to be stored in a species directory first
#' (e.g. using function \code{\link{getSpeciesImages}}). Station subdirectories
#' are optional.
#'
#' As with species identification, individuals can be identified in 2 different
#' ways: by moving images into individual directories
#' ("Species/Station/Individual/XY.JPG" or "Species/Individual/XY.JPG") or by
#' metadata tagging (without the need for individual directories:
#' "Species/XY.JPG" or "Species/Station/XY.JPG").
#'
#' \code{minDeltaTime} is a criterion for temporal independence of records of
#' an individual at the same station/location. Setting it to 0 will make the
#' function return all records. \code{camerasIndependent} defines if the
#' cameras at a station are to be considered independent (e.g. \code{FALSE} if
#' both cameras face each other and possibly \code{TRUE} if they face different
#' trails). \code{stationCol} is the station column name to be used in the
#' resulting table. Station IDs are read from the station directory names if
#' \code{hasStationFolders = TRUE}. Otherwise, the function will try to extract
#' station IDs from the image filenames (requires images renamed with
#' \code{\link{imageRename}}.
#'
#' If individual IDs were assigned with image metadata tags,
#' \code{metadataIDTag} must be set to the name of the metadata tag group used
#' for individual identification. \code{metadataHierarchyDelimitor} is "|" for
#' images tagged in DigiKam and images tagged in Adobe Bridge/ Lightroom with
#' the default settings. Manufacturer-specific Exif metadata tags such as
#' "AmbientTemperature" or "MoonPhase" can be extracted if specified in
#' \code{additionalMetadataTags}. Multiple names can be specified as a
#' character vector as: \code{c(Tag1, Tag2, ...)}. Because they are not
#' standardized, function \code{\link{exifTagNames}} provides a vector of all
#' available tag names. The metadata tags thus extracted may be used as
#' individual covariates in spatial capture-recapture models.
#'
#' \code{eventSummaryColumn} and \code{eventSummaryFunction} can be used to
#' extract summary statistics for independent sampling events. See documentations
#' for these functions in \code{camtrapR}.
#'
#' @param individualIDfrom character. Read individual ID from image metadata
#' ("metadata") of from directory names ("directory")
#' @param metadataIDTag character. In custom image metadata, the individual ID
#' tag name.
#' @param hasStationFolders Logical. Whether station column is taken from directory
#' @inheritParams recordTableAssist
#' @inheritParams addStationCameraID
#'
#' #' Af you use image metadata tags for identification, the tags must be written
#' to the image metadata. The function cannot read tags from .xmp sidecar
#' files. Make sure you set the preferences of your image management software
#' accordingly. In DigiKam, go to Settings/Configure digiKam/Metadata. There,
#' make sure "Write to sidecar files" is unchecked.
#'
#' Please note the section about defining argument \code{timeZone} in the
#' vignette on data extraction (accessible via
#' \code{vignette("DataExtraction")} or online
#' (\url{https://cran.r-project.org/package=camtrapR/vignettes/camtrapr3.pdf})).
#'
#' @author Juergen Niedballa
#'
#' @references Phil Harvey's ExifTool \url{https://exiftool.org/}
#'
#' @examples
#'
#'
#' \dontrun{   # the examples run too long to pass CRAN tests
#'
#'  wd_images_ID_individual <- system.file("pictures/sample_images_indiv_tag/LeopardCat",
#'                                         package = "camtrapR")
#'  # missing space in species = "LeopardCat" is because of CRAN package policies
#'  # note argument additionalMetadataTags: contains tag names as returned by function exifTagNames
#'
#'  if (Sys.which("exiftool") != ""){        # only run these examples if ExifTool is available
#'
#'  rec_table_pbe <- recordTableIndividualAssist(inDir                  = wd_images_ID_individual,
#'                                         minDeltaTime           = 60,
#'                                         deltaTimeComparedTo    = "lastRecord",
#'                                         hasStationFolders      = FALSE,
#'                                         IDfrom                 = "metadata",
#'                                         camerasIndependent     = FALSE,
#'                                         writecsv               = FALSE,
#'                                         metadataIDTag          = "individual",
#'                                         additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
#'                                         timeZone               = "Asia/Kuala_Lumpur"
#'  )
#'
#'
#'
#' } else {
#' # show function output if ExifTool is not available
#' message("ExifTool is not available. Cannot test function. Loading recordTableSample instead")
#' data(recordTableSample)
#' }
#' }
#'
#'
#' @export
recordTableIndividualAssist <- function(inDir,
                                  cameraID,
                                  hasStationFolders,
                                  IDfrom,
                                  StationIDfrom="directory",
                                  speciesIDfrom,
                                  cameraIDfrom,
                                  camerasIndependent,
                                  individualIDfrom,
                                  exclude,
                                  minDeltaTime = 0,
                                  deltaTimeComparedTo,
                                  timeZone,
                                  stationCol,
                                  writecsv = FALSE,
                                  outDir,
                                  metadataHierarchyDelimitor = "|",
                                  metadataSpeciesTag,
                                  metadataIDTag,
                                  additionalMetadataTags,
                                  removeDuplicateRecords = TRUE,
                                  stationIDposition = NULL,
                                  speciesPosition = NULL,
                                  cameraIDposition = NULL,
                                  directoryInfoPositions,
                                  directoryInfoNames)
{
  # dealing with users with older arguments
  if (!missing(IDfrom)) {
    warning("argument IDfrom is deprecated; please use speciesIDfrom and
            individualIDfrom instead.",
            call. = FALSE)
    individualIDfrom = IDfrom
      speciesIDfrom = "directory"
  }

  if (!missing(hasStationFolders)) {
    warning("argument hasStationFolders is deprecated; please use StationIDfrom instead.",
            call. = FALSE)
    if(hasStationFolders) {
      StationIDfrom <- "directory"
    } else {
      StationIDfrom <- "filename"
    }
  }

  if (!missing(cameraID)) {
    warning("argument cameraID is deprecated; please use cameraIDfrom instead.",
            call. = FALSE)
    cameraIDfrom = cameraID
  }


  #fix class check
  if(!is.character(individualIDfrom)){stop("IDfrom must be of class 'character'")}
  if(individualIDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

 if(individualIDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")

    if(!hasArg(metadataIDTag)) {stop("'metadataIDTag' must be defined if IDfrom = 'metadata'")}
    #fix class check
    if(!is.character(metadataIDTag)) {stop("metadataIDTag must be of class 'character'")}
    if(length(metadataIDTag) != 1) {stop("metadataIDTag must be of length 1")}
  }

  if(hasArg(metadataIDTag)){
    #fix class check
    if(!is.character(metadataIDTag)){stop("metadataIDTag must be of class 'character'", call. = FALSE)}
    if(length(metadataIDTag) != 1){stop("metadataIDTag must be of length 1", call. = FALSE)}
  }

recTable <- recordTableFUN(inDir=inDir,
                           StationIDfrom=StationIDfrom,
                           speciesIDfrom=speciesIDfrom,
                           cameraIDfrom=cameraIDfrom,
                           camerasIndependent=camerasIndependent,
                           individualIDfrom=individualIDfrom,
                           exclude=exclude,
                           minDeltaTime=minDeltaTime,
                           deltaTimeComparedTo=deltaTimeComparedTo,
                           timeZone=timeZone,
                           stationCol=stationCol,
                           writecsv=writecsv,
                           outDir=outDir,
                           metadataHierarchyDelimitor=metadataHierarchyDelimitor,
                           metadataSpeciesTag=metadataSpeciesTag,
                           metadataIDTag=metadataIDTag,
                           additionalMetadataTags=additionalMetadataTags,
                           removeDuplicateRecords=removeDuplicateRecords,
                           stationIDposition=stationIDposition,
                           speciesPosition=speciesPosition,
                           cameraIDposition=cameraIDposition,
                           directoryInfoPositions=directoryInfoPositions,
                           directoryInfoNames=directoryInfoNames)

  return(recTable)
}
