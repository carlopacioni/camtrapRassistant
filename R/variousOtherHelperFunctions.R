# Version Check for .onAttach()
# adapted from http://thecoatlessprofessor.com/programming/automatically-check-if-r-package-is-the-latest-version-on-package-load/. Thank you!


.pkgVersionCRAN <- function(pkg, cran_url="http://cran.r-project.org/web/packages/")
{
  # Create URL
  cran_pkg_loc <- paste0(cran_url,pkg)

  # Try to establish a connection
  suppressWarnings( conn <- try( url(cran_pkg_loc) , silent=TRUE ) )

  # If connection, try to parse values, otherwise return NULL
  if ( all( class(conn) != "try-error") ) {
    suppressWarnings( cran_pkg_page <- try( readLines(conn) , silent=TRUE ) )
    close(conn)
  } else {
    return(NULL)
  }

  # Extract version info
  version_line = cran_pkg_page[grep("Version:",cran_pkg_page)+1]
  gsub("<(td|\\/td)>","",version_line)
}


# for all functions in which user specifies column names: error if spaces in column names
checkForSpacesInColumnNames <- function(...){

  z <- list(...)

  # if all arguments are of length 1, do
  if(all(sapply(z, FUN = length) == 1)){
    if(any(grepl(pattern = " ", x = unlist(z), fixed = TRUE))) stop("column names may not contain spaces: \n ",
                                                                     paste(names(z)[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], "=",
                                                                           z[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], collapse = "\n "),
                                                                     call. = FALSE)
  }

  # if the argument is of length >1, do
  if(any(sapply(z, FUN = length) > 1)){
    if(length(z) != 1) stop("this is a bug in 'checkForSpacesInColumnNames'. I'm sorry. Please report it.")
    if(any(grepl(pattern = " ", x = unlist(z[[1]]), fixed = TRUE))) stop("column names in '", names(z) ,"' may not contain spaces: \n ",
                                                                         paste(names(unlist(z))[which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], "=",
                                                                               z[[1]][which(grepl(pattern = " ", x = unlist(z), fixed = TRUE))], collapse = "\n "),
                                                                         call. = FALSE)
  }
}



# for functions reading out and tabulating image metadata

runExiftool <- function(command.tmp,
                        colnames.tmp)
{
  tmp1 <- strsplit(system(command.tmp, intern=TRUE), split = "\t")

  if(length(tmp1) == 0) return(NULL) # if nothing returned (no images, no metadata)

  metadata.tmp <- as.data.frame(matrix(unlist(lapply(tmp1, FUN = function(X){X[2]})),
                                     ncol = length(colnames.tmp),
                                     byrow = TRUE),
                              stringsAsFactors = FALSE)
  colnames(metadata.tmp) <- colnames.tmp

  # find and remove ._ files created on Macs
  strangeMacFiles <- grep("^[._]", metadata.tmp$FileName, fixed = FALSE)
  if(length(strangeMacFiles) >= 1)  {
    warning(paste("found", length(strangeMacFiles), "JPG files beginning with '._' in", paste(unique(metadata.tmp$Directory[strangeMacFiles]), collapse = ","), ". Will ignore them."), call. = FALSE, immediate. = TRUE)
    metadata.tmp <- metadata.tmp[-strangeMacFiles,]
    }
  return(metadata.tmp)
}

runExiftool.par <- function(i, command.tmp, colnames.tmp) {
  metadata.tmp <- runExiftool(command.tmp=command.tmp[i], colnames.tmp=colnames.tmp)
  return(metadata.tmp)
}


addMetadataAsColumns <- function(intable,
                                 metadata.tagname,
                                 metadataHierarchyDelimitor,
                                 multiple_tag_separator)
{
  intable[,metadata.tagname] <- as.character(intable[,metadata.tagname])
  tmp2 <- strsplit(intable[,metadata.tagname], split = ",")      # split items of "HierarchicalSubject" at comma
  tmp3 <- lapply(tmp2, FUN = function(X){X[grep(pattern = metadataHierarchyDelimitor, x = X, fixed = TRUE)]})   # get only the ones with values


  # find all metadata categories, remove spaces
  list.tmp <- vector()
  for(xy in 1:length(tmp3)){
    list.tmp <- c(list.tmp, gsub(pattern = " ",
                                 replacement = "",
                                 x =  unlist(lapply(strsplit(tmp3[[xy]],
                                                             split = metadataHierarchyDelimitor,
                                                             fixed = TRUE),
                                                    FUN = function(Y){Y = Y[1]}))))
  }
  cols2add <- unique(list.tmp)    # these are the columns to add

  # add as columns
  if(length(cols2add) >= 1){    # if anything to add
    intable <- data.frame(intable, matrix(NA, ncol = length(cols2add), nrow = nrow(intable)))
    colnames(intable)[seq((ncol(intable) - length(cols2add) + 1),ncol(intable))] <- cols2add

    # fill metadata columns
    for(xyz in 1:length(cols2add)){
      intable[,cols2add[xyz]] <- unlist(lapply(lapply(tmp3, FUN = function(X) {sapply(strsplit(X[grep(x = X,
                                                                                                      pattern = paste(cols2add[xyz],
                                                                                                                      metadataHierarchyDelimitor,
                                                                                                                      collapse = "",
                                                                                                                      sep      = ""),
                                                                                                      fixed = TRUE)],
                                                                                               split = metadataHierarchyDelimitor,
                                                                                               fixed = TRUE),
                                                                                      FUN = function(Y){Y[2]})}),
                                               FUN = function(Z){paste(Z, collapse = multiple_tag_separator)}))

      intable[which(intable[,cols2add[xyz]] == ""), cols2add[xyz]] <- NA
    } # end for xyz
  } # end if

  which_cols_to_rename <- which(colnames(intable) %in% cols2add)

  # remove spaces and punctuation in column names
  #colnames(intable) <- gsub(pattern = "[[:blank:]]", replacement = "", x = colnames(intable))
  #colnames(intable) <- gsub(pattern = "[[:punct:]]", replacement = "", x = colnames(intable))

  # REMOVED THE FOLLOWING BECAUSE INTERFERES WITH COUNT FUNCTION, NOT SURE WHY WE NEED THIS, BUT LEFT IN CASE WE HAVE TO REINSTATE
  # rename metadata columns with prefix "metadata_"
  # colnames(intable)[which_cols_to_rename] <- paste("metadata_", colnames(intable)[which_cols_to_rename], sep = "")

  return(intable)
}



assignSpeciesID <- function(intable,
                            IDfrom,
                            metadataSpeciesTag,
                            speciesCol,
                            speciesPosition=NULL,
                            dirs_short,
                            i_tmp,
                            multiple_tag_separator)
{

  file.sep <- .Platform$file.sep


  if(IDfrom == "directory"){
    intable[,speciesCol] <-  sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE),
                                    FUN = function(X){if(is.null(speciesPosition)) {
                                                            X[length(X)]
                                                          } else {
                                                            X[speciesPosition]
                                                          }
                                      })
    return(intable)
  } else {
    if(hasArg(metadataSpeciesTag)){
      if(metadataSpeciesTag %in% colnames(intable)){

        intable[,speciesCol] <- intable[,metadataSpeciesTag]
        nrow.intable <- nrow(intable)
        species_records_to_remove <- which(is.na(intable[,speciesCol]))
        if(length(species_records_to_remove) >= 1){
          intable <- intable[-species_records_to_remove,]      #remove records without species tag
          warning(paste( dirs_short[i_tmp],":  removed", length(species_records_to_remove), "records out of", nrow.intable,
                         "because of missing", speciesCol, "metadata tag"), call. = FALSE, immediate. = TRUE)
        }

        intable <- separateMultipleSpecies (intable                = intable,
                                            speciesCol             = speciesCol,
                                            multiple_tag_separator = multiple_tag_separator)

        return(intable)
      } else {
        warning(paste(dirs_short[i_tmp], ":   metadataSpeciesTag '", metadataSpeciesTag, "' not found in image metadata tag 'HierarchicalSubject'.", sep = ""), call. = FALSE, immediate. = TRUE)
        return("found no species tag")
      }
    } else {
      stop(paste("station", dirs_short[i_tmp], ":   cannot figure out species names. Is metadataSpeciesTag defined?"), call. = FALSE)
    }
  }
}


# find and separate multiple species in same image (only if using metadata ID)
separateMultipleSpecies <- function(intable,
                                    speciesCol,
                                    multiple_tag_separator)
{

  records0                 <- intable[,speciesCol]
  records_duplicate        <- strsplit(intable[,speciesCol], split = multiple_tag_separator, fixed = TRUE)
  records_duplicate_length <- sapply(records_duplicate, length)

  if(any(records_duplicate_length > 1)){
    intable <- intable[rep(row.names(intable), records_duplicate_length), ]                # replicate rows with >1 species
    intable[,speciesCol] <- unlist(strsplit (records0, split = multiple_tag_separator))    # assign species anew
  }
  return(intable)
}
# add station and camera id to metadata table
#' Add station camera ID
#'
#' @inheritParams recordTableAssist
#' @importFrom methods hasArg

addStationCameraID <- function(intable,
                               dirs_short,
                               stationCol,
                               cameraCol,
                               cameraID,
                               hasStationFolders,
                               i,
                               IDfrom,
                               stationIDposition,
                               cameraIDposition=NULL)
{

  file.sep <- .Platform$file.sep

  # append station ID

  if(isTRUE(hasStationFolders)) {       # take station ID from station directories
    if(is.null(stationIDposition)) {
    intable <- cbind(intable, dirs_short[i])
    } else {
      intable <- cbind(intable,
                       sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE),
                              FUN = function(X){
                                X[stationIDposition]
                                }
                              ))
    }
    colnames(intable)[ncol(intable)] <- stationCol

  } else  {                             # take station ID from image filenames

    station.tmp  <- try(sapply(strsplit(as.character(intable$FileName), split = "__"), FUN = function(X){X[1]}))      # assumes filenames: STATION__Camera__Date/Time(Number).JPG)
    if(length(station.tmp) == nrow(intable)){
      intable <- cbind(intable, station.tmp)
      colnames(intable)[ncol(intable)] <- stationCol
    } else {
      stop(paste(dirs_short[i], ": numbers of images and station ID extracted from image names do not match. Do image filenames begin with station IDs?"))
    }
  }

  # append camera ID

  if(hasArg(cameraID)){
    if(cameraID == "filename"){
      camera.tmp  <- try(sapply(strsplit(as.character(intable$FileName), split = "__"), FUN = function(X){X[2]}))      # assumes filenames: Station__CAMERA__Date/Time(Number).JPG)
      if(length(camera.tmp) == nrow(intable)){
        intable <- cbind(intable, camera.tmp)
        colnames(intable)[ncol(intable)]     <- cameraCol
      }
    }
    if(cameraID == "directory"){
      if(IDfrom == "directory"){
        intable <- cbind(intable,
                         sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE),
                                FUN = function(X){
                                          if(is.null(cameraIDposition)) {
                                            X[length(X) - 1]
                                          } else {
                                            X[cameraIDposition]
                                          }
                                  }))
        } else {
        intable <- cbind(intable,
                         sapply(strsplit(intable$Directory, split = file.sep, fixed = TRUE), FUN = function(X){X[length(X)]}))
        }
      colnames(intable)[ncol(intable)]     <- cameraCol
    }
  }
  return(intable)
}

# check if date/time information is present and was readable

checkDateTimeOriginal <- function (intable, dirs_short, i){
     # if all date/time information is missing, go to next station
    if(all(intable$DateTimeOriginal == "-")){
      warning(paste(dirs_short[i], ": no readable date/time information. Skipping"), call. = FALSE,  immediate. = TRUE)
      intable <- NULL
    } else {

    # if date/time information is missing for some records only
     if(any(intable$DateTimeOriginal == "-")){
      which_no_time <- which(intable$DateTimeOriginal == "-")
      warning(paste(dirs_short[i], ": omitting", length(which_no_time), "images because of missing/unreadable date/time information."), call. = FALSE,  immediate. = TRUE)
     intable <-  intable[-which_no_time,]    # removing rows with missing date/time information
    }
    }
    return(intable)
    }
  # remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
  # Note to self: this may also be done outside the station loop, after the final record table is assembled. Saves a few executions of this function.

  removeDuplicatesOfRecords <- function(metadata.tmp, removeDuplicateRecords, camerasIndependent, stationCol, speciesCol, cameraCol){

    #Resolve no visible global variable
    countsName <- NULL

          if(isTRUE(removeDuplicateRecords)){
            if(isTRUE(camerasIndependent)){
              remove.tmp <- which(
                duplicated(metadata.tmp[, c("DateTimeOriginal", stationCol,
                                           speciesCol, cameraCol,
                                           if(hasArg(countsName)) countsName)]))
              if(length(remove.tmp >= 1)){
                metadata.tmp <- metadata.tmp[-remove.tmp,]
                message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ": removed ", length(remove.tmp), " duplicate records")
              }
            } else {
              remove.tmp <- which(
                duplicated(metadata.tmp[, c("DateTimeOriginal", stationCol, speciesCol,
                                            if(hasArg(countsName)) countsName)]))
              if(length(remove.tmp >= 1)) {
                metadata.tmp <- metadata.tmp[-remove.tmp,]
                message(paste(unique(metadata.tmp[,stationCol]), collapse = ", "), ": removed ", length(remove.tmp), " duplicate records")
              }
            }
          }
          return(metadata.tmp)
        }



# add potential new columns to global record.table

addNewColumnsToGlobalTable <- function(intable,
                                       i,
                                       record.table)
{

  if( nrow(record.table) >= 1){
    which_cols_to_add_to_d1 <- seq(1, ncol(record.table))[-which(colnames(record.table) %in% colnames(intable))]   # columns in record.table but not in intable

    # if intable lacks columns present in record.table, add them here (filled with NA)
    if(length(which_cols_to_add_to_d1) >= 1){
      intable <- data.frame(intable, as.list(rep(NA, each = length(which_cols_to_add_to_d1))))
      colnames(intable)[(ncol(intable) - length(which_cols_to_add_to_d1) + 1) :  ncol(intable)] <- colnames(record.table)[which_cols_to_add_to_d1]
    }

    # now check which columns are present in intable but not in record.table (new tag groups) and add these (filled with NA)
    which_cols_to_add_to_record.table <- seq(1, ncol(intable))[-which(colnames(intable) %in% colnames(record.table))]  # columns present in intable but not in record.table
    if(length(which_cols_to_add_to_record.table) >= 1){
      record.table <- data.frame(record.table, as.list(rep(NA, each = length(which_cols_to_add_to_record.table))))
      colnames(record.table)[(ncol(record.table) - length(which_cols_to_add_to_record.table) + 1) :  ncol(record.table)] <- colnames(intable)[which_cols_to_add_to_record.table]
    }
    outtable <- intable[,match(colnames(record.table), colnames(intable))]
  } else {
    outtable <- intable
  }
  return(list(outtable, record.table))
}



#####################################################
# for detectionHistory functions

checkCamOpColumnNames <- function(cameraOperationMatrix){
camopTest <- try(as.Date(colnames(cameraOperationMatrix)), silent = TRUE)
#fix class check
if(inherits(camopTest, "try-error")) stop(paste('could not interpret column names in camOp as Dates. Desired format is YYYY-MM-DD, e.g. "2016-12-31". First column name in your camera operation matrix is "', colnames(cameraOperationMatrix)[1], '"', sep = '' ), call. = FALSE)
}


##########################################################################################################
splitDir = function(x, directoryInfoPosition) {
  tmp <- unlist(strsplit(x, split = "/", fixed = TRUE))
  return(tmp[directoryInfoPosition])
}

parseDir <- function(intable, directoryInfoPositions) {
  return(as.data.frame(
    t(data.frame(lapply(intable$Directory, splitDir, directoryInfoPositions))))
  )
}

#CP version
#' Assess temporal independence and generate a record table
#'
#' This function is intended for situations where a detection
#'table has already been generated, for examples a \code{recordTable} has been
#'generated and has been manipulated based on specific user's need, and there is
#'the need to assess the independence of the detections.
#'
#'@param intable data.frame or data.table. A data frame or a data.table with the
#' detections (e.g. the output from recordTable)
#'@param columnOfInterest Character. Either the name of the species or individual
#'column ID. The latter to obtain a recordTableIndividual
#'@param cameraCol Character. The name of the camera column (e.g. "Camera")
#'@inheritParams recordTableAssist
#'@import data.table
#'@import utils
#'@export
assessTemporalIndependence <- function(intable,
                                       deltaTimeComparedTo,
                                       columnOfInterest,     # species/individual column
                                       cameraCol,
                                       camerasIndependent,
                                       stationCol,
                                       minDeltaTime,
                                       countsName) {
  # Resolve no visible global variable
  rn <- independent <- IndepRecStartTime  <- delta.time.mins <- delta.time.hours <- delta.time.days <- NULL
  # Resolve no visible global function
  DateTimeOriginal <- J <- delta.time.secs <- NULL
  ############################ Helper function #################################
  extact_sel_groups <- function(i, sel.groups) return(as.list(sel.groups[, i]))


  sel_independent <- function(sel.group, intable, deltaTimeComparedTo,
                              countsName) {
    ref <- 1
    setkeyv(intable, cols = c(stationCol, if(camerasIndependent) cameraCol,
                              columnOfInterest))
    subtable <- intable[sel.group, ]

    if(deltaTimeComparedTo == "lastIndependentRecord") {
      repeat {
        setkey(subtable, rn)
        ref.time <- subtable[J(ref), DateTimeOriginal]
        subtable[J(ref:max(rn)),
                  delta.time.secs := difftime(DateTimeOriginal, ref.time, units="secs")]
        if(hasArg(countsName)) {
          max.count <- max(subtable[rn >= ref & delta.time.secs <= (minDeltaTime * 60),
                                countsName, with=FALSE])

          setkeyv(subtable, c("rn", countsName))
          ref.rn <- subtable[rn >= ref & get(countsName) == max.count, min(rn)]
          setkey(subtable, rn)
        } else {
          ref.rn <- ref
          setkey(subtable, rn)
        }
        subtable[J(ref.rn), independent := TRUE]
        subtable[J(ref.rn), IndepRecStartTime := ref.time]
        if(sum(subtable[J(ref:max(rn)), delta.time.secs] > minDeltaTime * 60, na.rm=TRUE)) {
          ref <- subtable[rn > ref & delta.time.secs > (minDeltaTime * 60), min(rn)]
          } else {
          break
        }
      }
      subtable[independent == TRUE,
               delta.time.secs := c(0, difftime(tail(DateTimeOriginal, -1),
                                                head(DateTimeOriginal, -1),
                                                units = "secs"))]
    } else { # if "lastRecord"
      subtable[ , delta.time.secs := c(0, difftime(tail(DateTimeOriginal, -1),
                                                   head(DateTimeOriginal, -1),
                                                   units = "secs"))]

      if(hasArg(countsName)) {
        repeat {
          if(ref == subtable[, max(rn)] |
            !isTRUE(as.logical(sum(subtable[rn > ref, delta.time.secs] > minDeltaTime * 60, na.rm=TRUE)))) {
            ref.lim <-  subtable[, max(rn)] + 1
          } else {
            if(sum(subtable[rn > ref, delta.time.secs] > minDeltaTime * 60, na.rm=TRUE)) {
              ref.lim <- subtable[rn > ref & delta.time.secs > (minDeltaTime * 60), min(rn)]
            }
          }
          max.count <- max(subtable[rn %in% ref:(ref.lim - 1), countsName, with=FALSE])

          setkeyv(subtable, c("rn", countsName))
          ref.rn <- subtable[rn %in% ref:(ref.lim - 1) & get(countsName) == max.count, min(rn)]
          setkey(subtable, rn)
          subtable[ref.rn, independent := TRUE]
          ref.time <- subtable[J(ref), DateTimeOriginal]
          subtable[J(ref.rn), IndepRecStartTime := ref.time]
          if(ref.lim <= subtable[, max(rn)]) {
            ref <- ref.lim
          } else {
            break
          }
        }
      } else {
        subtable[1, independent := TRUE]
        subtable[delta.time.secs > (minDeltaTime * 60), independent := TRUE]
        subtable[independent == TRUE, IndepRecStartTime := DateTimeOriginal]
      }
    }
    subtable[, IndepRecStartTime := NULL]
    return(subtable[independent == TRUE, ])
  }
  ##############################################################################

  if(hasArg(stationCol) == FALSE) stationCol <- "Station"
  stopifnot(is.character(stationCol))

  if(is.data.table(intable)) setDF(intable)

  # check if all Exif DateTimeOriginal tags were read correctly
  if(any(is.na(intable$DateTimeOriginal))){
    which.tmp <- which(is.na(intable$DateTimeOriginal))
    if(length(which.tmp) == nrow(intable))
      stop("Could not read any Exif DateTimeOriginal tag at station: ",
           paste(unique(intable[which.tmp, stationCol])),
           " Consider checking for corrupted Exif metadata.")
    warning(paste("Could not read Exif DateTimeOriginal tag of", length(which.tmp),
                  "image(s) at station",
                  paste(unique(intable[which.tmp, stationCol]), collapse = ", "),
                  ". Will omit them. Consider checking for corrupted Exif metadata. \n",
                  paste(file.path(intable[which.tmp, "Directory"],
                                  intable[which.tmp, "FileName"]), collapse = "\n")),
            call. = FALSE, immediate. = TRUE)
    intable <- intable[-which.tmp ,]
    rm(which.tmp)
  }

  intable[, stationCol] <- as.character(intable[, stationCol])

  # Ensure that the table is sorted correctly (sort by station, (camera), species or individualID and time)
    if(camerasIndependent == TRUE) {
      intable <- intable[order(intable[, stationCol], intable[, columnOfInterest], intable[, cameraCol], intable$DateTimeOriginal),]
    } else {
      intable <- intable[order(intable[, stationCol], intable[, columnOfInterest], intable$DateTimeOriginal),]
    }

  # prepare to add time difference between observations columns
  intable.dt <- data.table(intable)

  # introduce column specifying independence of records
  if(minDeltaTime == 0) {
    intable.dt[, independent := TRUE]    # all independent if no temporal filtering
  } else {
    intable.dt[, independent := FALSE]
  }

  intable.dt[, rn := 1:.N, by=c(columnOfInterest, stationCol,
                             if(camerasIndependent) cameraCol)]

  if(camerasIndependent){
    sel <- intable.dt[, unique(.SD), .SDcols=columnOfInterest, by=c(stationCol, cameraCol)]
  } else {
    sel <- intable.dt[, unique(.SD), .SDcols=columnOfInterest, by=stationCol]
  }

  sel.groups <- sel[, apply(.SD, 1, c), .SDcols=c(stationCol,
                                                if(camerasIndependent) cameraCol,
                                                columnOfInterest)]
  sel.groups <- lapply(1:ncol(sel.groups), extact_sel_groups, sel.groups)

  loutTable <- lapply(sel.groups, sel_independent, intable.dt, deltaTimeComparedTo,
                 countsName=countsName)
  # keep only independent records
  outtable <- rbindlist(loutTable)
  outtable[, independent := NULL]
  outtable[, rn := NULL]

  # compute delta time in hours and days
  outtable[, delta.time.secs := as.numeric(round(delta.time.secs, digits = 0))]
  outtable[, delta.time.mins := round(delta.time.secs / 60, digits = 0)]
  outtable[, delta.time.hours := round(delta.time.mins  / 60, digits = 1)]
  outtable[, delta.time.days  := round(delta.time.hours / 24, digits = 1)]

  return(outtable)
}
