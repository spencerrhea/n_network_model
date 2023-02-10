library(tidyverse)
library(nhdplusTools)
library(sf)

nhdplusTools::get_huc8(ID == '')

cpp <- st_read('../vpp') %>%
    mutate(huc4 = substr(HUC_12, 0,4)) %>%
    st_transform(., crs = 4269)


dataRetrieval::get_nldi_sources()$source
#>  [1] "ca_gages"       "geoconnex-demo" "gfv11_pois"     "huc12pp"
#>  [5] "nmwdi-st"       "nwisgw"         "nwissite"       "ref_gage"
#>  [9] "vigil"          "wade"           "WQP"            "comid"

nldi_feature <- list(featureSource = "huc12pp",
                     featureID = '010900020301')

test_look <- nhdplusTools::get_nldi_feature(nldi_feature)



start_point <- st_sfc(st_point(c(test_look$X, test_look$Y)), crs = 4269)
start_comid <- discover_nhdplus_id(start_point)

flowline <- navigate_nldi(list(featureSource = "comid",
                               featureID = this_comid),
                          mode = "upstreamTributaries",
                          distance_km = 8000)

# Download all the NHD data for that network
subset_file <- tempfile(fileext = ".gpkg")
subset <- subset_nhdplus(comids = as.numeric(flowline$UT$nhdplus_comid),
                         output_file = subset_file,
                         nhdplus_data = "download",
                         flowline_only = FALSE,
                         return_data = TRUE,
                         overwrite = TRUE)


look_ <- jsonlite::read_json('../huc12pp.json')





flowline <- navigate_nldi(list(featureSource = "huc12pp",
                               featureID = '01090002'),
                          mode = "upstreamTributaries",
                          distance_km = 1000)
