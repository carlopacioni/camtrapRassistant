#context("recordTable")
library(camtrapRassistant)

# run recordTable

wd_images_ID_species <- system.file("pictures/sample_images_species_dir", package = "camtrapR")

rec_table0 <- recordTableAssist(inDir               = wd_images_ID_species,
                          speciesIDfrom             = "directory",
                          timeZone = "UTC"
)


rec_table1 <- recordTableAssist(inDir               = wd_images_ID_species,
                          speciesIDfrom                 = "directory",
                          minDeltaTime           = 60,
                          deltaTimeComparedTo    = "lastRecord",
                          writecsv               = FALSE,
                          additionalMetadataTags = c("EXIF:Model", "EXIF:Make"),
                          timeZone               = "Asia/Kuala_Lumpur"
)


# with additionalMetadataTags, event Summary and exclude

rec_table2_1 <- recordTableAssist(inDir               = wd_images_ID_species,
                            speciesIDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastRecord",
                            exclude                = "UNID",
                            writecsv               = FALSE,
                            timeZone               = "Asia/Kuala_Lumpur",
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
)

# with additionalMetadataTags and multiple event summaries

rec_table2_2 <- recordTableAssist(inDir               = wd_images_ID_species,
                            speciesIDfrom                 = "directory",
                            minDeltaTime           = 60,
                            deltaTimeComparedTo    = "lastRecord",
                            exclude                = "UNID",
                            writecsv               = FALSE,
                            timeZone               = "Asia/Kuala_Lumpur",
                            additionalMetadataTags = c("EXIF:Model", "EXIF:Make")
)


# with removeDuplicateRecords

rec_table3a <- recordTableAssist(inDir              = wd_images_ID_species,
                           speciesIDfrom                 = "directory",
                           minDeltaTime           = 0,
                           exclude                = "UNID",
                           timeZone               = "Asia/Kuala_Lumpur",
                           removeDuplicateRecords = FALSE
)

rec_table3b <- recordTableAssist(inDir              = wd_images_ID_species,
                           speciesIDfrom                 = "directory",
                           minDeltaTime           = 0,
                           exclude                = "UNID",
                           timeZone               = "Asia/Kuala_Lumpur",
                           removeDuplicateRecords = TRUE
)



# Test section

test_that("recordTable output has correct class", {
  expect_type(rec_table0,   "data.frame")
  expect_type(rec_table1,   "data.frame")
  expect_type(rec_table2_1, "data.frame")
  expect_type(rec_table2_2, "data.frame")
  expect_type(rec_table3a,  "data.frame")
  expect_type(rec_table3b,  "data.frame")
})

test_that("recordTable output has correct dimensions", {
  expect_equal(dim(rec_table0),   c(56,11))
  expect_equal(dim(rec_table1),   c(40,13))
  expect_equal(dim(rec_table2_1), c(39,13))
  expect_equal(dim(rec_table2_2), c(39,13))
  expect_equal(dim(rec_table3a),  c(67,11))
  expect_equal(dim(rec_table3b),  c(55,11))
})

test_that("removeDuplicateRecords works", {
  expect_equal(anyDuplicated(rec_table3a[, c("Station", "Species", "DateTimeOriginal")]), 12)   # 12 duplicates
  expect_equal(anyDuplicated(rec_table3b[, c("Station", "Species", "DateTimeOriginal")]), 0)    # 0 duplicates
})

test_that("errors are correct", {
  expect_error(recordTableAssist(inDir               = wd_images_ID_species,
                           speciesIDfrom                 = "Directory",
                           timeZone               = "Asia/Kuala_Lumpur"),
               "speciesIDfrom' must be 'metadata' or 'directory")
  expect_error(recordTableAssist(inDir               = wd_images_ID_species,
                           speciesIDfrom              = "directory",
                           cameraIDfrom            = "directory",
                           timeZone               = "Asia/Kuala_Lumpur"),
               "camerasIndependent is not defined. It must be defined if cameraIDfrom is defined")
  expect_error(recordTableAssist(inDir               = wd_images_ID_species,
                           speciesIDfrom              = "directory",
                           cameraIDfrom            = "Filename",
                           camerasIndependent  = TRUE,
                           timeZone               = "Asia/Kuala_Lumpur"),
               "cameraIDfrom can only be 'filename', 'directory', or missing")
})

test_that("time zone message works", {
  expect_warning(recordTableAssist(inDir     = wd_images_ID_species,
                             speciesIDfrom    = "directory"),
                 "timeZone is not specified. Assuming UTC"

                 )})


test_that("warnings are correct", {

  expect_warning(recordTableAssist(inDir               = wd_images_ID_species,
                             speciesIDfrom                 = "directory",
                             additionalMetadataTags = c("EXIF:Model", "NonExistingTag"),
                             timeZone               = "Asia/Kuala_Lumpur"),
                 "not found in image metadata:   NonExistingTag")

})
