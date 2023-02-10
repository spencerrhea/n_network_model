library(nhdplusTools)
library(igraph)
library(tidyverse)
library(sf)

# Manual for NHD (has column names for NHD flow lines): https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/Documentation/NHDPlusV2_User_Guide.pdf

# THese are the OG ones from Ashly and Lauren but two were weird so I changed the start comid
# start_comid <- c("3819343","4112750","8607673","9064520","14930395","921061",
#                  "1173978","11122885","15998224","16015578","3179276","7226838",
#                  "8422212","11445989","12377407")
start_comid <- c("3819343","4112750","8608649","9064520","14930395","921061",
                 "1173978","11122885","15998224","16015578","3179276","7226838",
                 "8422238","11445989","12377407")

if(!dir.exists('data/nhd_files')){
    dir.create('data/nhd_files/', recursive = T)
}

for(i in 1:length(start_comid)){

    # Get the starting flowline for the network (flowline segments are called comids)
    this_comid <- start_comid[i]
    # Get the flowlines upstream of the starting point
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

    # Save all the components separately
    flownetwork <- subset$NHDFlowline_Network
    catchment <- subset$CatchmentSP
    nhd_area <- subset$NHDArea
    waterbody <- subset$NHDWaterbody
    non_flowline <- subset$NHDFlowline_NonNetwork

    if(!dir.exists(paste0('data/nhd_files/', this_comid))){
        dir.create(paste0('data/nhd_files/', this_comid), recursive = T)
    }

    write_rds(x = flownetwork,
             file = paste0('data/nhd_files/',this_comid,'/flowlines.rds'))

    write_rds(x = catchment,
              file = paste0('data/nhd_files/',this_comid,'/catchment.rds'))

    if(!is.null(nhd_area)){
        write_rds(x = nhd_area,
                  file = paste0('data/nhd_files/',this_comid,'/nhd_area.rds'))
    }

    if(!is.null(waterbody)){
        write_rds(x = waterbody,
                  file = paste0('data/nhd_files/',this_comid,'/waterbody.rds'))
    }

    if(!is.null(non_flowline)){
        write_rds(x = non_flowline,
                  file = paste0('data/nhd_files/',this_comid,'/non_flowline.rds'))
    }
}


#### Create igraphe ####
# Convert nhd flowlines into igraph objects

networks <- list.files('data/nhd_files/')
for(i in 1:length(networks)){

    # Read in NHD flowlines
    this_network_name <- networks[i]
    this_network <- read_rds(paste0('data/nhd_files/', this_network_name, '/flowlines.rds'))

    # this_network %>%
    #     # mutate(col = ifelse(hydroseq == 800017035, 'this', 'not')) %>%
    #     mapview::mapview(., zcol = 'hydroseq')

    # What Lauren has in their nodes data:
    # [1] "group.comid"        "COMID"              "vpu"                "X"
    # [5] "Y"                  "LengthKM"           "StreamOrde"         "TotDASqKM"
    # [9] "AreaSqKM"           "Q0001E"             "Outlet.ID"          "ID"
    # [13] "NHD.VPU.ID"         "XCoord"             "YCoord"             "ReachLength_km"
    # [17] "StreamOrder"        "UpstreamDA_km2"     "DirectDA_km2"       "MeanAnnualFlow_cfs"

    # Create the node metadata for igraph
    this_network_nodes <- this_network %>%
        select(id = hydroseq,
               to = dnhydroseq,
               comid,
               vpuid,
               length_km = lengthkm,
               stream_order = streamorde,
               total_drain_area_km = totdasqkm,
               area_km = areasqkm,
               qe_01,
               nhd_id = id,
               reachcode)

    # Create links between nodes
    this_network_links <- this_network %>%
        as.data.frame() %>%
        select(from = hydroseq,
               to = dnhydroseq,
               id = hydroseq,
                      comid,
                      vpuid,
                      length_km = lengthkm,
                      stream_order = streamorde,
                      total_drain_area_km = totdasqkm,
                      area_km = areasqkm,
                      qe_01,
                      nhd_id = id,
                      reachcode,
                      -geometry) %>%
        mutate(n_catch = n_catchemnt(area_km),
               q_catch = q_catchment(area_km)) %>%
        arrange(desc(total_drain_area_km))

    ## Remove outlet/nodes that are not in the network
    # Need to do this because every to and from needs to be in the nodes file

    ## One network has a mistake, manually fixing that here
    if(networks[i] == '14930395'){
        # Node that is messed up is hydroseq:10022252. 3 hydroseq flow into it 10024738,
        # 10022487, 10066483. This line flows into 10021731. hydroseq: 10021731.
        # properly identifies hydroseq:10022252. as its upstream reach uphydroseq.
        # hydroseq:10022252 does not poerply idenitfy hydroseq: 10021731. as its
        # down stream reach. Will manually change hydroseq:10022252. downasteam rach
        # (dnhydroseq) to 10021731
        this_network[this_network$comid == '14930431', 'dnhydroseq'] <- 10021731
        this_network_links[this_network_links$comid == '14930431', 'to'] <- 10021731
    }
    outlet_code <- this_network$dnhydroseq[!this_network$dnhydroseq %in% this_network$hydroseq]
    this_network_links <- this_network_links[!this_network_links$to %in% outlet_code,]

    # Convert NHD to igraph
    net <- graph_from_data_frame(d=this_network_links, vertices=this_network_nodes, directed=T)

    # Save file
    write_rds(net, paste0('data/nhd_files/', this_network_name, '/igraph_network.rds'))

}

# plot(net,
#      edge.arrow.size=0.25,edge.width=0.5, vertex.color="orange", vertex.size=10,
#      vertex.frame.color="white",vertex.label=NA,edge.color="grey70",edge.curved=0)
