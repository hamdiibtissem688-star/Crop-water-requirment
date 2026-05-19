rm(list=ls())

library(dplyr)

library(readr)
library(purrr)
library(stringr)




df1 <- readr::read_delim(
  "C:/Users/hamdi/Desktop/TFM/Datos/FAO/FAO/2018 FAO/MONEGROS_2018/Monegros_Pac_2018.csv",
  delim = ";"
)

df2 <- readr::read_delim(
  "C:/Users/hamdi/Desktop/TFM/Datos/FAO/FAO/2018 FAO/MONEGROS_2018/Monegros_Sup_cumunidades/Comunidad_Candasnos.csv",
  delim = ";"
)



df_merged <- df1 %>%
  left_join(df2, by = c("CULTIVO PAC" = "CULTIVO"))



write.csv(df_merged, "C:/Users/hamdi/Desktop/TFM/Datos/FAO/FAO/2018 FAO/MONEGROS_2018/resultat_eya.csv")
