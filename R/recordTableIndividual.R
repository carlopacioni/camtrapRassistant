#' Generate a single-species record table with individual identification from
#' camera trap images or videos
#'
#' The function generates a single-species record table containing individual
#' IDs, e.g. for (spatial) capture-recapture analyses. It prepares input for
#' the function \code{\link{spatialDetectionHistory}}.
#'
#' The function can handle a number of different ways of storing images and
#' videos. In every case, images need to be stored in a species directory first
#' (e.g. using function \code{\link{getSpeciesImages}}). Station subdirectories
#' are optional. Camera subdirectories are not supported. This directory
#' structure can be created easily with function
#' \code{\link{getSpeciesImages}}.
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
#' extract summary statistics for independent sampling events. For example, you
#' assigned a "count" tag to your images, indicating the number of individuals
#' in a picture. In a sequence of pictures taken within 1 minute, most pictures
#' show one individual, but one image shows two individuals. You tagged the
#' images accordingly (count = 1 or count = 2) and run \code{recordTable}. Set
#' \code{eventSummaryColumn = "count"} and \code{eventSummaryFunction = "max"}
#' to obtain the maximum number of \code{count} in all images within
#' \code{minDeltaTime} minutes of a given record. The results is in a new
#' column, in this example \code{count_max}. You can also calculate several
#' statistics at the same time, by supplying vectors of values, e.g.
#' \code{eventSummaryColumn = c("count", "count", "camera")} and
#' \code{eventSummaryFunction = c("min", "max", "unique")} to get minimum and
#' maximum count and all unique camera IDs for that event. Note that
#' \code{eventSummaryColumn} and \code{eventSummaryFunction} must be of same
#' length.
#'
#' Argument \code{video} is analogous to \code{\link{recordTable}}, a named
#' list with 2 or 4 items. 2 items (\code{file_formats}, \code{dateTimeTag})
#' are always required, and are sufficent if \code{IDfrom = "directory"}. In
#' that case, no digiKam tags will be returned.  To return digiKam tags, two
#' additional items are required (\code{db_directory}, \code{db_filename}).
#' This is essential when using \code{IDfrom = "metadata"}. When using
#' \code{IDfrom = "directory"}, it is optional, but allows to extract metadata
#' tags assigned to videos in digiKam. This workaround is necessary because
#' digiKam tags are not written into video metadata, but are only saved in the
#' digiKam database. So in contrast to JPG images, they can not be extracted
#' with ExifTool. It also requires that \code{inDir} is in your digiKam
#' database.
#'
#' @inheritParams recordTable
#' @export
recordTableIndividual <- function(inDir,
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
                           IDfrom=IDfrom,
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
