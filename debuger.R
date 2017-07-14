library(xlsx)
library(leaflet)

filedata <- read.xlsx("C:/Users/INER-TRANSPORTE11/Documents/DatosVRPv3.xlsx",sheetIndex = 1, header = F)
colnames(filedata) <- c("Name","Latitude","Longitude","Demand")

source('get_matrix_alt.R', local = T)
datavrp <- get_matrix_alt(filedata)

source('savings_alg_alt.R', local = T)
optimal <- savings_alg_alt(datavrp, 10000, "D") 

source('get_geometry.R', local = T)
nesarcs<-get_geometry(datavrp, optimal)

source("mapgen.R", local = T)
m <- map(filedata, nesarcs)
  
  
