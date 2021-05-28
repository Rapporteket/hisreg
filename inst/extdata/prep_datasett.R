
klokebok_hisreg <- read.table(system.file(file.path('extdata', 'Hisreg_klokeboken_2021-01-07.csv'), package = 'hisreg'),
                                             header = TRUE, sep = ";", fileEncoding = "UTF-8-BOM")

klokebok_hisreg$fysisk_feltnavn[klokebok_hisreg$skjemanavn %in% c("Pasient 6 mnd", "Lege 6 mnd")] <-
  paste0(klokebok_hisreg$fysisk_feltnavn[klokebok_hisreg$skjemanavn %in% c("Pasient 6 mnd", "Lege 6 mnd")], "_kir")
klokebok_hisreg$fysisk_feltnavn[klokebok_hisreg$skjemanavn %in% c("Pasient 3 mnd", "Lege 3 mnd")] <-
  paste0(klokebok_hisreg$fysisk_feltnavn[klokebok_hisreg$skjemanavn %in% c("Pasient 3 mnd", "Lege 3 mnd")], "_med")



usethis::use_data(klokebok_hisreg, overwrite = TRUE, internal = FALSE)


