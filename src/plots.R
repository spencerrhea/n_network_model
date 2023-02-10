library(tmap)
library(tidyverse)
library(ggthemes)

network_attr <- read_csv('data/network_attr.csv') %>%
    mutate(comid = as.character(comid))

comids <- list.files('data/model_output/load_0.001/')
all_networks <- tibble()
for(s in 1:length(comids)){
    this_comid <- comids[s]
    network_data <- read_rds(paste0('data/model_output/load_0.001/', this_comid, '/model_outpu.rds'))

    outlet_info <- network_data %>%
        filter(total_drain_area_km == max(total_drain_area_km))

    n_load_tot <- sum(network_data$Nload)
    n_out <- outlet_info$Nout

    frac_removed <- 1-(n_out/n_load_tot)

    stream_surface_area <- sum(network_data$SurfArea)
    stream_length <- sum(network_data$length_km)

    this_data_info <- tibble(comid = this_comid,
           area_km = outlet_info$total_drain_area_km,
           stream_length = stream_length,
           stream_surface_area = stream_surface_area,
           n_load_kg = n_load_tot,
           n_removed = n_load_tot - n_out,
           frac_removed = frac_removed)

    all_networks <- rbind(all_networks, this_data_info)
}

all_networks <- all_networks %>%
    mutate(comid = ifelse(comid == '8608649', '8607673', comid),
           comid = ifelse(comid == '8422238', '8422212', comid))

final_table <- left_join(all_networks, network_attr) %>%
    mutate(surface_area_to_area = ((stream_surface_area)/(area_km*1e+6))*100) %>%
    mutate(percent_removed = frac_removed * 100)


#### Prob network ####
# 14930395
# Problem seems to be with comid 14930431, name 10022252
model_outut <- read_rds('data/model_output/load_1/14930395/model_outpu.rds') %>%
    mutate(frac_removed = (Nlost/Nloc)*100)

flowlines <- read_rds(paste0('data/nhd_files/', 14930395, '/flowlines.rds')) %>%
    select(comid, geometry)

n_net_data_sf <- merge(flowlines, model_outut, by = 'comid')

mapview::mapview(n_net_data_sf, zcol = 'Qout')

blank_map <- tm_shape(n_net_data_sf) +
    tm_lines() +
    tm_scale_bar()

frac_removed_map <- tm_shape(n_net_data_sf) +
    tm_lines(col = 'frac_removed', lwd = 2, title.col = 'N Percent Removed') +
    tm_scale_bar()

#### Plots ####
model_outut <- read_rds('data/model_output/load_1/4112750/model_outpu.rds') %>%
    mutate(frac_removed = (Nlost/Nloc)*100)

flowlines <- read_rds(paste0('data/nhd_files/', 4112750, '/flowlines.rds')) %>%
    select(comid, geometry)

n_net_data_sf <- merge(flowlines, model_outut, by = 'comid')


blank_map <- tm_shape(n_net_data_sf) +
    tm_lines() +
    tm_scale_bar()

tmap_save(blank_map, 'figs/4112750_blank.png', width = 6, height = 6)


frac_removed_map <- tm_shape(n_net_data_sf) +
    tm_lines(col = 'frac_removed', lwd = 2, title.col = 'N Percent Removed') +
    tm_scale_bar()

tmap_save(frac_removed_map, 'figs/4112750_frac_removed.png', width = 6, height = 6)

n_exported_map <- tm_shape(n_net_data_sf) +
    tm_lines(col = 'Nout', lwd = 2, title.col = 'N exported (kg/d)') +
    tm_scale_bar()

tmap_save(n_exported_map, 'figs/4112750_N_export.png', width = 6, height = 6)

mapview::mapview(n_net_data_sf, zcol = 'Nout')





model_outut <- read_rds('data/model_output/load_1/3819343/model_outpu.rds') %>%
    mutate(frac_removed = (Nlost/Nloc)*100)

flowlines <- read_rds(paste0('data/nhd_files/', 3819343, '/flowlines.rds')) %>%
    select(comid, geometry)

n_net_data_sf <- merge(flowlines, model_outut, by = 'comid')

blank_map <- tm_shape(n_net_data_sf) +
    tm_lines() +
    tm_scale_bar()

tmap_save(blank_map, 'figs/3819343_blank.png', width = 6, height = 6)


frac_removed_map <- tm_shape(n_net_data_sf) +
    tm_lines(col = 'frac_removed', lwd = 2, title.col = 'N Percent Removed') +
    tm_scale_bar()

tmap_save(frac_removed_map, 'figs/3819343_frac_removed.png', width = 6, height = 6)

n_exported_map <- tm_shape(n_net_data_sf) +
    tm_lines(col = 'Nout', lwd = 2, title.col = 'N exported (kg/d)') +
    tm_scale_bar()

tmap_save(n_exported_map, 'figs/3819343_N_export.png', width = 6, height = 6)

mapview::mapview(n_net_data_sf, zcol = 'Nout')


#### GGplots ####
ggplot(final_table, aes(RI, percent_removed)) +
    geom_point() +
    stat_smooth(method = 'lm') +
    theme_few() +
    labs(x = 'Hortons length ratio',
         y = 'Percent of N removed')

summary(lm(RI~frac_removed, final_table))


ggplot(final_table, aes(Ra, frac_removed)) +
    geom_point() +
    stat_smooth(method = 'lm') +
    theme_few() +
    labs(x = 'Hortons area ratio',
         y = 'Percent of N removed')



ggplot(final_table, aes(surface_area_to_area, percent_removed)) +
    geom_point() +
    theme_few() +
    labs(x = 'Percent watershed is stream',
         y = 'Percent N removed')

