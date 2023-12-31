recordTable.par <- function(inDir,
                        IDfrom,
                        cameraID,
                        camerasIndependent,
                        exclude,
                        minDeltaTime = 0,
                        deltaTimeComparedTo,
                        timeZone,
                        stationCol,
                        writecsv = FALSE,
                        outDir,
                        metadataHierarchyDelimitor = "|",
                        metadataSpeciesTag,
                        additionalMetadataTags,
                        removeDuplicateRecords = TRUE,
                        speciesPosition = NULL,
                        cameraIDposition = NULL,
                        directoryInfoPositions,
                        directoryInfoNames,
                        countsName,
                        ncores
)
{
  # Resolve no visible global variable issue
  Time <- Date <- DateTimeOriginal <- delta.time.secs <- delta.time.mins <- delta.time.hours <- NULL
  delta.time.days <- independent <- rn <- NULL

  wd0 <- getwd()
  on.exit(setwd(wd0))

  if(hasArg(stationCol) == FALSE) stationCol <- "Station"
  stopifnot(is.character(stationCol))
  speciesCol <- "Species"

  checkForSpacesInColumnNames(stationCol = stationCol)

  if(!is.character(IDfrom)){stop("IDfrom must be of class 'character'")}
  if(IDfrom %in% c("metadata", "directory") == FALSE) stop("'IDfrom' must be 'metadata' or 'directory'")

 if(IDfrom == "metadata"){
    if(metadataHierarchyDelimitor %in% c("|", ":") == FALSE) stop("'metadataHierarchyDelimitor' must be '|' or ':'")

    if(!hasArg(metadataSpeciesTag)) {stop("'metadataSpeciesTag' must be defined if IDfrom = 'metadata'")}
   #fix character class check
   if(!is.character(metadataSpeciesTag)){stop("metadataSpeciesTag must be of class 'character'")}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1")}
  }

  multiple_tag_separator <- "_&_"

  # check input
  if(hasArg(timeZone) == FALSE) {
    warning("timeZone is not specified. Assuming UTC", call. = FALSE,  immediate. = TRUE)
    timeZone <- "UTC"
  }
  if(!is.element(timeZone , OlsonNames())){
    stop("timeZone must be an element of OlsonNames()", call. = FALSE)
  }
  if(Sys.which("exiftool") == "") stop("cannot find ExifTool", call. = FALSE)

  if(hasArg(metadataSpeciesTag)){
    #fix character class check
    if(!is.character(metadataSpeciesTag)){stop("metadataSpeciesTag must be of class 'character'", call. = FALSE)}
    if(length(metadataSpeciesTag) != 1){stop("metadataSpeciesTag must be of length 1", call. = FALSE)}
  }

  if(hasArg(cameraID)){
    ##fix character class check
    if(!is.character(cameraID)){stop("cameraID must be of class 'character'", call. = FALSE)}
    if(cameraID %in% c("filename", "directory") == FALSE) {stop("cameraID can only be 'filename', 'directory', or missing", call. = FALSE)}
    if(!hasArg(camerasIndependent)){stop("camerasIndependent is not defined. It must be defined if cameraID is defined", call. = FALSE)}
    #fix class check
    if(!is.logical(camerasIndependent)){stop("camerasIndependent must be of class 'logical'", call. = FALSE)}
  } else { camerasIndependent <- FALSE}

  cameraCol <- "Camera"


  if(hasArg(outDir)){
    ##fix character class check
    if(!is.character(outDir)){stop("outDir must be of class 'character'", call. = FALSE)}
    if(file.exists(outDir) == FALSE) stop("outDir does not exist", call. = FALSE)
  }

  if(hasArg(exclude)){
    #fix character class check
    if(!is.character(exclude)){stop("exclude must be of class 'character'", call. = FALSE)}
  }

  stopifnot(is.logical(removeDuplicateRecords))

  metadata.tagname <- "HierarchicalSubject"    # for extracting metadata assigned in tagging software

  if(hasArg(additionalMetadataTags)){
    ##fix character class check
    if(!is.character(additionalMetadataTags)){stop("additionalMetadataTags must be of class 'character'", call. = FALSE)}
    if(any(grep(pattern = " ", x = additionalMetadataTags, fixed = TRUE))) stop("In argument additionalMetadataTags, spaces are not allowed")
    if("HierarchicalSubject" %in% additionalMetadataTags & IDfrom == "metadata")  {
      warning("'HierarchicalSubject' may not be in 'additionalMetadataTags' if IDfrom = 'metadata'. It will be ignored because the function returns it anyway.", call. = FALSE)
      additionalMetadataTags <- additionalMetadataTags[-grep(pattern = "HierarchicalSubject", x = additionalMetadataTags)]  # remove it
    }
  }

  minDeltaTime <- as.integer(minDeltaTime)
  stopifnot(class(minDeltaTime) == "integer")

  if(minDeltaTime != 0){
    stopifnot(hasArg(deltaTimeComparedTo))
    stopifnot(class(deltaTimeComparedTo) == "character")
    stopifnot(deltaTimeComparedTo %in% c("lastRecord", "lastIndependentRecord"))
    if(!hasArg(deltaTimeComparedTo)) stop(paste("minDeltaTime is not 0. deltaTimeComparedTo must be defined"), call. = FALSE)
  } else {
    if(hasArg(deltaTimeComparedTo)) {warning(paste("minDeltaTime is 0. deltaTimeComparedTo = '", deltaTimeComparedTo, "' will have no effect", sep = ""), call. = FALSE, immediate. = TRUE)
    } else {
      deltaTimeComparedTo <- "lastRecord"
      }
  }


  stopifnot(class(writecsv) == "logical")

#  if(!hasArg(inDir)){stop("inDir must be defined", call. = FALSE)}
  ##fix character class check
  if(!is.character(inDir)){stop("inDir must be of class 'character'", call. = FALSE)}
  if(length(inDir) != 1){stop("inDir may only consist of 1 element only", call. = FALSE)}
  if(!dir.exists(inDir)) stop("Could not find inDir:\n", inDir, call. = FALSE)


  # find image directories
  dirs <- list.dirs(inDir, full.names = TRUE, recursive = FALSE)
  dirs_short <- list.dirs(inDir, full.names = FALSE, recursive = FALSE)
  dir_full <- list.dirs(inDir, full.names = TRUE, recursive = TRUE)

  record.table.list <- vector("list", length = length(dirs))

  if(hasArg(additionalMetadataTags)){
    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject',
                          paste(" -",additionalMetadataTags,  collapse = "", sep = ""), ' -ext JPG "', dirs, '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject", additionalMetadataTags)
  } else {
    command.tmp  <- paste('exiftool -q -f -t -r -Directory -FileName -EXIF:DateTimeOriginal -HierarchicalSubject -ext JPG "',
                          dirs, '"', sep = "")
    colnames.tmp <- c("Directory", "FileName", "DateTimeOriginal", "HierarchicalSubject")
  }

  i <- seq_along(dirs)

  if(!hasArg(ncores)) ncores <- parallel::detectCores() - 1
  if(ncores == 1) {
    metadata.tmp.list <- lapply(i, runExiftool.par,
                                command.tmp=command.tmp,
                                colnames.tmp=colnames.tmp)
  } else{
    if(length(dirs) < ncores) ncores <- length(dirs)
    cl <- parallel::makeCluster(ncores)
    i <- seq_along(dir_full)
    parallel::clusterExport(cl,
                            varlist=c("i", "command.tmp", "colnames.tmp"),
                            envir=environment())

    metadata.tmp.list <- parallel::parLapply(cl, i, runExiftool.par,
                                             command.tmp=command.tmp, colnames.tmp=colnames.tmp)   # parallel execution
    parallel::stopCluster(cl)
  }

  for(i in 1:length(dirs)){   # loop through station directories
    metadata.tmp <- metadata.tmp.list[[i]]

    #fix class check
    if(is.null(metadata.tmp)){            # omit station if no images found

      length.tmp <- length(list.files(dirs[i], pattern = ".jpg$|JPG$", ignore.case = TRUE, recursive = TRUE))
      warning(paste(dirs_short[i], "contains no images;", " found", length.tmp, "JPEGs"), call. = FALSE,  immediate. = TRUE)

    } else {

      message(paste(dirs_short[i], ":", nrow(metadata.tmp), "images"))

      # check presence / consistency of DateTimeOriginal column, go to next station or remove records if necessary
      metadata.tmp <- checkDateTimeOriginal (intable    = metadata.tmp,
                                             dirs_short = dirs_short,
                                             i          = i)
      if(is.null(metadata.tmp)) next

      # now split HierarchicalSubject tags and add as columns to table
      metadata.tmp <- addMetadataAsColumns (intable                    = metadata.tmp,
                                            metadata.tagname           = metadata.tagname,
                                            metadataHierarchyDelimitor = metadataHierarchyDelimitor,
                                            multiple_tag_separator     = multiple_tag_separator)

      # add species names to metadata table (from folders or metadata, otherwise NA)

      metadata.tmp <- assignSpeciesID (intable                = metadata.tmp,
                                       IDfrom                 = IDfrom,
                                       metadataSpeciesTag     = metadataSpeciesTag,
                                       speciesCol             = speciesCol,
                                       dirs_short             = dirs_short,
                                       i_tmp                  = i,
                                       multiple_tag_separator = multiple_tag_separator,
                                       speciesPosition = speciesPosition
      )

      # if no tagged images in current station, go to next one
      #fix class check
      if(!is.data.frame(metadata.tmp))       next

      # remove empty metadata columns (if HierarchicalSubject is all empty or if additionalMetadataTags were not found)
      empty_cols <- which(apply(metadata.tmp, MARGIN = 2, FUN = function(X){all(X == "-")}))
      if(length(empty_cols) >= 1){
        metadata.tmp <-  metadata.tmp[, -empty_cols]
      }

      # add station and camera id to metadata table
      arg.list0 <- list(intable = metadata.tmp, dirs_short = dirs_short, stationCol = stationCol, hasStationFolders = TRUE, cameraCol = cameraCol, i = i, IDfrom = IDfrom)  # assumes station directories

      if(!hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, arg.list0)
      if( hasArg(cameraID)) metadata.tmp <- do.call(addStationCameraID, c(arg.list0, cameraID = cameraID,
                                                                          cameraIDposition=cameraIDposition))

      # remove species in argument "excluded"
        if(any(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude))) {  # if there is anything to remove
          metadata.tmp <- metadata.tmp[-which(tolower(metadata.tmp[,speciesCol]) %in% tolower(exclude)),]
        }

      if(nrow(metadata.tmp) >= 1){   # if anything left after excluding species, do

        # convert character vector extracted from images to time object and format for outfilename
        metadata.tmp$DateTimeOriginal <- as.POSIXct(strptime(x = metadata.tmp$DateTimeOriginal, format = "%Y:%m:%d %H:%M:%S", tz = timeZone))

        # sort by (camera), species and time
        if(camerasIndependent == TRUE) {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp[,cameraCol], metadata.tmp$DateTimeOriginal),]
        } else {
          metadata.tmp <- metadata.tmp[order(metadata.tmp[,stationCol], metadata.tmp[,speciesCol], metadata.tmp$DateTimeOriginal),]
        }



        #remove duplicate records of same species taken in same second at the same station (by the same camera, if relevant)
        metadata.tmp2 <- removeDuplicatesOfRecords(metadata.tmp           = metadata.tmp,
                                                  removeDuplicateRecords = removeDuplicateRecords,
                                                  camerasIndependent     = camerasIndependent,
                                                  stationCol             = stationCol,
                                                  speciesCol             = speciesCol,
                                                  cameraCol              = cameraCol)

        # extact info from path if relevant
        if(hasArg(directoryInfoPositions))  {
          dirPath <- parseDir(metadata.tmp2, directoryInfoPositions = directoryInfoPositions)
          names(dirPath) <- directoryInfoNames
          metadata.tmp2 <- cbind(metadata.tmp2, dirPath)
        }

        if(hasArg(countsName))  {
        metadata.tmp2[, countsName] <- as.numeric(metadata.tmp2[, countsName])
        }

        # assess independence between records and calculate time differences
        record.table.list[[i]] <- assessTemporalIndependence(intable= metadata.tmp2,
                                          deltaTimeComparedTo = deltaTimeComparedTo,
                                          camerasIndependent  = camerasIndependent,
                                          columnOfInterest    = speciesCol,
                                          cameraCol           = cameraCol,
                                          minDeltaTime        = minDeltaTime,
                                          stationCol          = stationCol,
                                          countsName          = countsName)


        ################ TO-DO ###################
        # delete if it works

        # d1, d2 and record.table steps can be done all at once with rbindlist,
        # only once, outside the loop

      # add potential new columns to global record.table
      #  d2 <- addNewColumnsToGlobalTable (intable      = d1,
      #                                    i            = i,
      #                                    record.table = record.table)



      # append table of station i's images metadata to global record table
      #  record.table <- rbind(d2[[2]], d2[[1]])


      #  suppressWarnings(rm(d1, d2))
      }  # end      if(nrow(metadata.tmp) >= 1){} else {...}   # i.e. not all species were excluded
    }    # end      if(nrow(metadata.tmp) == 0){} else {...}   # i.e. directory i contained images
  }      # end      for(i in 1:length(dirs)){   # loop through station directories

  record.table <- rbindlist(record.table.list, use.names=TRUE, fill=TRUE)
  if(nrow(record.table) == 0){
    stop(paste("something went wrong. I looked through all those", length(dirs)  ,"folders and now your table is empty. Did you exclude too many species? Or were date/time information not readable?"), call. = FALSE)
  }

  # rearrange table, add date and time as separate columns. add additional column names as needed.

  #record.table2  <-  data.frame(record.table[,c(stationCol, speciesCol, "DateTimeOriginal")],
   #                             Date = as.Date (record.table$DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone),
   #                             Time = strftime(record.table$DateTimeOriginal, format = "%H:%M:%S", tz = timeZone),
   #                            record.table[,c("delta.time.secs", "delta.time.mins", "delta.time.hours", "delta.time.days",
   #                                             "Directory", "FileName")])
  record.table[, Date := as.Date(DateTimeOriginal, format = "%Y/%M/%d", tz = timeZone)]
  record.table[, Time := strftime(DateTimeOriginal, format = "%H:%M:%S", tz = timeZone)]

  # metadata_columns <- which(colnames(record.table) %in% colnames(record.table2) == FALSE)

  # add metadata columns
  #if(length(metadata_columns) >= 1){
  #  record.table3 <- cbind(record.table2, record.table[,metadata_columns])
  #  colnames(record.table3)[(ncol(record.table2) + 1) : ncol(record.table3)] <- colnames(record.table)[metadata_columns]
  #} else {record.table3 <- record.table2}


  # add camera column (if present)
  #if(hasArg(cameraID)){
   # record.table3 <- data.frame(record.table3[,stationCol],
   #                             record.table[,cameraCol],
   #                             record.table3[,-which(colnames(record.table3) %in% c(stationCol, cameraCol))])
   # colnames(record.table3)[1] <- stationCol
   # colnames(record.table3)[2] <- cameraCol
  #}

  #
  #rownames(record.table3) <- NULL

  # compute delta time in hours and days
  record.table[, delta.time.secs := round(delta.time.secs, digits = 0)]
  record.table[, delta.time.mins := round(delta.time.secs / 60, digits = 0)]
  record.table[, delta.time.hours := round(delta.time.mins  / 60, digits = 1)]
  record.table[, delta.time.days  := round(delta.time.hours / 24, digits = 1)]
  record.table[, independent := NULL]
  record.table[, rn := NULL]

  # warning if additionalMetadataTags were not found
  if(hasArg(additionalMetadataTags)){
    whichadditionalMetadataTagsFound <- which(gsub(additionalMetadataTags, pattern = ":", replacement = ".") %in% colnames(record.table))   # replace : in additionalMetadataTags (if specifying tag groups) with . as found in column names
    if(length(whichadditionalMetadataTagsFound) < length(additionalMetadataTags)){
      if(length(whichadditionalMetadataTagsFound) == 0) {  # if none of the additionalMetadataTags was found
        warning(paste("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags, collapse = ", ")), call. = FALSE)
        } else {                                                            # if only some of the additionalMetadataTags was found
        warning(paste("metadata tag(s)  not found in image metadata:  ", paste(additionalMetadataTags[-whichadditionalMetadataTagsFound], collapse = ", ")), call. = FALSE)
      }
    }
  }

  # remove "independent" column
  #cols_to_remove <- which(colnames(record.table3) %in% c("independent"))
  #if(length(cols_to_remove) >= 1){
  #  record.table3 <- record.table3[,-cols_to_remove]
  #}

  # set columns order
  new.order <- c(stationCol, if(hasArg(cameraID)) cameraCol, speciesCol,
                 "Directory", "FileName", "DateTimeOriginal", "Date", "Time", "delta.time.secs",
                 "delta.time.mins", "delta.time.hours", "delta.time.days",
                 if(hasArg(countsName)) countsName,
                 if(hasArg(additionalMetadataTags)) "HierarchicalSubject")
  metadata.cols <- names(record.table)[-which(names(record.table) %in% new.order)]

  setcolorder(record.table,
              if(hasArg(additionalMetadataTags)) {
                c(head(new.order, -1), metadata.cols, "HierarchicalSubject")
              } else {
                new.order
              })

  # make column "HierarchicalSubject" the last column
  #col_to_move <- which(colnames(record.table3) %in% metadata.tagname)
  #if(length(col_to_move) >= 1){
  #   record.table3 <- cbind(record.table3, record.table3[,col_to_move])
	#  record.table3 <- record.table3[,-col_to_move]
  #  colnames(record.table3)[ncol(record.table3)] <- metadata.tagname
  #}

  # save table
  if(writecsv == TRUE){
    outtable_filename <- paste("record_table_", minDeltaTime, "min_deltaT_", Sys.Date(), ".csv", sep = "")
    if(hasArg(outDir) == FALSE){
      setwd(inDir)
    } else {
      setwd(outDir)
    }
  write.csv(record.table, file = outtable_filename)
  }
  return(record.table)
}
