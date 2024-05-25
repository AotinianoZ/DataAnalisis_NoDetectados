excel_data <- readxl::read_xlsx(path = "BD.xlsx", col_names = TRUE)
csv_data <- utils::read.csv(file="data_compilada/data_final2.csv", header = TRUE, sep=",")
library(nasapower)
server_nasa_data <- get_power(
  community = "ag",
  lonlat = c(-77.02824, -12.04318),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2021-01-01", "2021-12-31"),
  temporal_api = "hourly"
)
shapefile_data <- sf::st_read(dsn="data_compilada/shapefile/Amazonas (Boletín 39C)/")
raster_data <- terra::rast(x = "RasterBase/DEM.tif")
tratamiento <- readxl::read_xlsx(path=file.choose())
