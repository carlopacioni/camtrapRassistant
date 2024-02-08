#context("imageRename")
library(camtrapRassistant)


wd_images_ID_species <- system.file("pictures/sample_images_species_dir", package = "camtrapR")


test_that("imageRename throws error if inDir doesn't exist", {
  expect_error(imageRename(inDir = paste(wd_images_ID_species, "blabla", sep = ""),hasCameraFolders= FALSE),
               "Could not find inDir")
})


test_that("imageRename throws error if inDir doesn't have subdirectories", {
  expect_error(imageRename(inDir = list.dirs(wd_images_ID_species)[4], hasCameraFolders= TRUE),
               "is not TRUE")
})
