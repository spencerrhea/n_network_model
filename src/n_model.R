## Functions for network model. Adpaed from Hare 2022: https://github.com/Haredkb/NetworkCarbon/blob/main/R/igraphNetwork_MB.R
# This script is a steady-state nitrate model for NHD networks. Model is from
# Helton et al., 2017.


# comids_change <- tibble(comid = c('11122885', '11445989', '1173978', '12377407',
#                                   '14930395', '15998224', '16015578', '3179276',
#                                   '3819343', '4112750', '7226838', '8422212',
#                                   '7226838', '8607673'),
#                         status = c('good', 'good', 'good', 'good', 'good', 'good',
#                                    'good', 'good', 'good', 'good', 'good', 'bad',
#                                    'good', 'bad'),
#                         new_comid = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
#                                       NA, '8422238', NA, '8608649'))


# Step up
library(igraph)
library(lubridate)
library(tidyverse)

comid <- list.files('data/nhd_files/')

# kg/km/day
n_loading_rate <- 0.00001

# Loop though all networks
for(f in 1:length(comid)){

    # Read in igraph network created in pull_nhd_convr_igraph
    this_comid <- comid[f]

    igraph_net <- read_rds(paste0('data/nhd_files/', this_comid, '/igraph_network.rds'))
    network <- igraph_net

    # Convert igraph to dataframe
    net_data_frame <- igraph::as_data_frame(network)

    # Get the outlet of the basin
    last_flowline <- net_data_frame %>%
        filter(total_drain_area_km == max(total_drain_area_km)) %>%
        pull(from)

    all_ids <- net_data_frame$id

    ## ---Function identifies all nodes upstream of a selected node--- ##
    get_number_upstream_nodes <- function(ids){

        network_connections <- tibble(id=all_ids,
                                      upstream_connections = NA)
        get_length <- function(ids) {
            length(head(unlist(ego(network,order=length(V(network)),nodes=as.character(ids),mode=c("in"),mindist=0)),-1))
        }

        connections <- unlist(map(ids, get_length))

        network_connections$upstream_connections <- connections

        return(network_connections)
    }
    network_order <- get_number_upstream_nodes(all_ids) %>%
        arrange(upstream_connections)

    # network_order <- rbind(network_order, tibble(id = as.double(last_flowline), upstream_connections = NA))


    ## ------------- Function for reach Q ------------------- ##
    q_catchment <- function(area, yeild = (7.69e-09 * 86400)){
        #  7.69 × 10−9 m3 m−2 s−1 is the yield from Helton et al. 2017
        # Area must be in km2

        # Convert from km to m2 and multiply times yield to get m3/d of water
        q_catch <- area*1e+6*yeild

        return(q_catch)
    }

    ## ------------- Function for reach N ------------------- ##
    n_catchemnt <- function(area, yeild = n_loading_rate){
        # 0.0001 to 100 kg N km−2 day−1 from helton et al 2017
        # Area must be in km

        n_catch <- area*yeild

        return(n_catch)
    }

    ## ------------- N removal Model ------------------------------- ##
    n_uptake_velocity <- function(discharge, n_load, c, d){

        Vfp <- c * (n_load/discharge)^d
    }

    n_uptake <- function(n_load, uptake_velocity, surface_area, discharge){

        # Ratio of discharge to streambed surface area (width * length)
        Hlp <- discharge/surface_area

        Nrp = n_load * (1 - exp(1)^(-uptake_velocity/Hlp))
    }


    ## ---------- Functions used in the mass balance model --------- ##

    # Function to implement the N mass-balance model:
    solveMB_N <- function(network, network_order){
        ##debug
        # network <- net#net_BF
        # Define input parameters:

        # Inflow discharge from local catchment (m3/d):
        V(network)$Qlocal <- 0
        # Inflow discharge from upstream reaches (m3/d):
        V(network)$Qnet <- 0
        # Outflow discharge from each reach (m3/d):
        V(network)$Qout <- NA
        # N load from upstream catchment (kg/d):
        V(network)$Nload <- 0
        # N load from upstream reaches (kg/d):
        V(network)$Nnet <- 0
        # Total N in reach (kg/d)
        V(network)$Nloc <- 0
        # Surface Area (m2)
        V(network)$SurfArea <- 0
        # N Uptake Velocity (m/d)
        V(network)$Nv <- 0
        # N removed (kg/d)
        V(network)$Nlost <- NA
        # Exported N load from each reach (kg/d):
        V(network)$Nout <- 0


        #hedrology/n removal Parameters Heton 2017
        a=7.17
        b=0.348
        c=3.196e-4
        d=-0.49311

        # Calculate mass-balance for each reach moving down the network from headwaters to mouth:
        for(i in 1:length(network_order$id)){
            # for(i in 1:52){

            # Get location in igraph network
            p <- grep(network_order$id[i], names(V(network)))
            n_ID <- as.character(network_order$id[i]) #node ID being run
            e_ID <- incident(network, n_ID, mode = c("in")) #get edges so the data is shared - this should replace the mutate to nodes.
            #e_ID$SEGMENT_ID
            #e_ID <-
            # length_reach <- sum(n_ID$length_km)
            # length_reach <- ifelse(length_reach == 0, 10, length_reach) # locations within any edges in are springs and thus have 10 meter contributing
            # V(network)$length_reach[i] <- length_reach

            # Find neighboring reaches upstream that flow in to the reach:
            up <- igraph::neighbors(network, p, mode=c("in")) #only single up
            up.all.nodes <- head(unlist(ego(network,order=length(V(network)),nodes=n_ID,mode=c("in"),mindist=0)),-1)

            # Define hydrologic inflows/outflows for each reach (m3 d-1)
            # Discharge inflow from local catchment (m3 d-1):
            #V(network)$Qlocal[i] <- V(network)$runoff_mday[i] * (V(network)$areasqkm[i]*10^6)

            if(V(network)$area_km[p] == 0){
                V(network)$area_km[p] <- 0.1
            }

            # m3/d
            V(network)$Qlocal[p] <-  q_catchment(V(network)$area_km[p])

            # Discharge inflow from upstream network (m3 d-1):
            if(length(up)>0){
                V(network)$Qnet[p] <- sum(V(network)$Qout[up]) #only one or junction will have two
            }

            #w = aQb
            # Discharge outflow to downstream reach (m3 d-1):
            V(network)$Qout[p] <- sum(V(network)$Qlocal[p], V(network)$Qnet[p], na.rm = T)

            # Q muct be in m3/s
            V(network)$width_m[p] <- a * ((V(network)$Qout[p])/86400)^b

            # Calc surface area and convert length in km to m (m2)
            V(network)$SurfArea[p] <-  V(network)$width_m[p] * (V(network)$length_km[p]) * 1000

            # Define nitrogen inflows/outflows for each reach (kg d-1)
            # nitrogen inflow from local catchment (kg d-1):
            V(network)$Nload[p] <- n_catchemnt(V(network)$area_km[p])

            # Nitrogen inflow from upstream network (kg d-1):
            if(length(up)>0){
                V(network)$Nnet[p] <- sum(V(network)$Nout[up])
            }

            # Total N in reach
            V(network)$Nloc[p] <- V(network)$Nload[p] +  V(network)$Nnet[p]

            ## Nitrogen Removed
            # N uptake velocity
            V(network)$Nv[p] <- n_uptake_velocity(V(network)$Qout[p],  V(network)$Nloc[p], c = c, d = d)

            # Removed from channel
            n_removed <- n_uptake(n_load = V(network)$Nloc[p],
                                  uptake_velocity =  V(network)$Nv[p],
                                  surface_area = V(network)$SurfArea[p],
                                  discharge =  V(network)$Qout[p])

            V(network)$Nlost[p] <- n_removed

            # Total N exported downstream
            V(network)$Nout[p] <- V(network)$Nloc[p] - V(network)$Nlost[p]

        }

        # Get list with attributes
        out <- get.vertex.attribute(network)

        # Export network:
        return(out)

    }

    # Run network model
    n_network <- solveMB_N(network = igraph_net,
                           network_order = network_order)

    n_net_data <- as.data.frame(do.call(cbind, n_network)) %>%
        select(-geometry)

    n_net_data <- as.data.frame(lapply(n_net_data, unlist, recursive = TRUE))


    # Add in final flowline
    # For some reason, the last nodel has to be treated sepratly
    final_line <- n_net_data %>%
        filter(total_drain_area_km == max(total_drain_area_km))

    final_id <- final_line %>%
        pull(name)

    input_lines <- n_net_data %>%
        filter(to == !!final_id)

    final_line$Qlocal <-  q_catchment(final_line$area_km)

    final_line$Qnet <- sum(input_lines$Qout)

    final_line$Qout <- final_line$Qnet + final_line$Qlocal

    # N model
    a=7.17
    b=0.348
    c=3.196e-4
    d=-0.49311
    final_line$width_m <- a * (final_line$Qout/86400)^b

    # Calc surface area and convert length in km to m (m2)
    final_line$SurfArea <- final_line$width_m * (final_line$length_km) * 1000

    final_line$Nload <- n_catchemnt(final_line$area_km)

    # Nitrogen inflow from upstream network (kg d-1):
    final_line$Nnet <- sum(input_lines$Nout)

    # Total N in reach (kg d-1)
    final_line$Nloc <- final_line$Nload + final_line$Nnet

    ## Nitrogen Removed
    # N uptake velocity
    final_line$Nv <- n_uptake_velocity(final_line$Qout,  final_line$Nloc, c = c, d = d)

    # Removed from channel
    n_removed <- n_uptake(n_load = final_line$Nloc,
                          uptake_velocity =  final_line$Nv,
                          surface_area = final_line$SurfArea,
                          discharge = final_line$Qout)

    final_line$Nlost <- n_removed

    final_line$Nout <- final_line$Nloc - n_removed

    n_net_data[grep(final_id, n_net_data$name),] <- final_line

    this_path <- paste0('data/model_output/load_', n_loading_rate, '/', this_comid)
    dir.create(this_path, recursive = T)

    write_rds(n_net_data, paste0(this_path, '/model_outpu.rds'))

    # flowlines <- read_rds(paste0('data/nhd_files/', this_comid, '/flowlines.rds')) %>%
    #     select(comid, geometry)

    # n_net_data_sf <- merge(flowlines, n_net_data, by = 'comid')

    # mapview::mapview(n_net_data_sf, zcol = 'Nout')
}
