# virtualspecies_thesis
This repository records the code of the research for the graduation thesis of the author from the Master of Global Change Ecology and Sustainable Development Goals of the University of Bologna.
This research aims at assessing high suitable methodology for real species distribution modelling, using virtual species in SDM algorithms and for different temporal settings (overlapping and non-overlapping). Please find further information of the study in the attached abstract.[Abstract of Master Graduation Thesis.docx](https://github.com/Doriblue/virtualspecies_thesis/files/10984610/Abstract.of.Master.Graduation.Thesis.docx)

Composition of the repository:
- Set_up.r: the loading of the necessary packages being used in the research; the set up of the study area and the environmental values of the study area; and the Creating functions for GAM and RF calibration with split train/test dataset
- Virtual_Species.r: The generation of virtual species with the same number of occurrences, species prevalence, sample prevalence as the real species.
- Real_Species.R:The upload of GBIF occurrences for each real species and creating a dataframe of modSpecies being the material for SDM calibration and evaluation of SDM
- Pixel_variability_between_2_temporal_settings.R: The CV test of rasters of SDM of all species to compare the pixel variability between 2 temporal settings
- shapiro_kruskal_dunn_test.r: The statistical tests to testify Effect of the sampling method on the predictions
