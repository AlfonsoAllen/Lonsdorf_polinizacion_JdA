
library(sf)
library(terra)
library(dplyr)
library(raster)
library(readr)
library(foreach)
library(doParallel)

source("R-scripts/aux_functions/load_expert_tables.R")
source("R-scripts/aux_functions/load_and_fix_geometries.R")
source("R-scripts/aux_functions/features_within_circle.R")
source("R-scripts/aux_functions/create_df_features_within_circle_sf.R")
source("R-scripts/aux_functions/df_percentage4codigo_inside_circle.R")

# Spatial parameters to compute the Lonsdorf model -----------------------------
typical_foraging_dist_big = 1500
typical_foraging_dist_small = 500

# Raster resolution_raster
resolution_raster <- 100  # pixel area: resolution_raster x resolution_raster

compute_validation_points <- TRUE
compute_other_points <- FALSE

# Load expert tables -----------------------------------------------------------
path_file_expert_table <- "Results/expert_table_JdA_V1.csv"
list_expert_tables <- load_expert_tables(path_file_expert_table)
NS_table_big <- list_expert_tables[[1]]
NS_table_small <- list_expert_tables[[2]]
HF_table_big <- list_expert_tables[[3]]
HF_table_small <- list_expert_tables[[4]]


# Load sites' coordinates-------------------------------------------------------
selected_sites_coordinates <- NULL

if(compute_validation_points){

  path_file_validation_coordinates <- "Results/fixed_Junta_validation_data.csv"
  validation_sites_coordinates <- readr::read_csv(path_file_validation_coordinates) %>%
    dplyr::select(Sitio,Ano,Latitud, Longitud) %>%
    rename(site_id = Sitio,latitude = Latitud, longitude = Longitud, refYear = Ano)

  selected_sites_coordinates <- validation_sites_coordinates
}else if(compute_other_points){

  path_file_coordinates <- "Data/sites_AAP2.csv"
  other_sites_coordinates <- readr::read_csv(path_file_coordinates) %>%
    dplyr::select(site_id,latitude, longitude,refYear)

  selected_sites_coordinates <- dplyr::bind_rows(selected_sites_coordinates,
                                          other_sites_coordinates)
}

# Load JdA's geodatabase -------------------------------------------------------
gdb_path <- "Ecosistemas_2023.gdb"
layer2read <- "Ecosistemas2023"

ecosistemas2023_validos2 <- load_and_fix_geometries(gdb_path, layer2read)
# This operation may take up to 10 minutes depending on the CPU's features.


# Prepare cores for tasks parallelization --------------------------------------
nCores <- 9 # detectCores() - 1
registerDoParallel(cores = nCores)


# Compute Lonsdorf Pollinator service for selected sites------------------------

selected_sites_coordinates$Lonsdorf_big <- NA
selected_sites_coordinates$Lonsdorf_small <- NA

estimation_time <- Sys.time()
formatted_time <- format(estimation_time, "%Y-%m-%d_%H-%M-%S")

for(site_i in 1:nrow(selected_sites_coordinates)){

  # inicio <- system.time({

  latitud_i <- selected_sites_coordinates$latitude[site_i] %>% as.numeric()
  longitud_i <-  selected_sites_coordinates$longitude[site_i] %>% as.numeric()

  # Distancia desde el centro hasta los bordes del cuadrado (la mitad del lado del cuadrado)
  circle_radius <- 2 * 2 * typical_foraging_dist_big

  features_dentro_circulo_sf <- features_within_circle(ecosistemas2023_validos2,
                                                       latitud_i,longitud_i,
                                                       circle_radius)

  df_features_within_circle_sf <- create_df_features_within_circle_sf(features_dentro_circulo_sf,
                                                                      resolution_raster)

  # Sanity check
  df_features_within_circle_sf %>% group_by(x,y) %>%
    count(wt = weight) %>% mutate(n=round(n,2)) %>% filter(n>1) %>% nrow()

  any(duplicated(df_features_within_circle_sf[,c("x","y","codigo")]))

  # Calcular el centro del bounding box
  bbox <- st_bbox(features_dentro_circulo_sf)
  x_centro <- (bbox["xmax"] + bbox["xmin"]) / 2
  y_centro <- (bbox["ymax"] + bbox["ymin"]) / 2


  df_features_within_circle_sf$latitud <- latitud_i
  df_features_within_circle_sf$longitud <- longitud_i
  df_features_within_circle_sf$x_centro <- x_centro
  df_features_within_circle_sf$y_centro <- y_centro
  df_features_within_circle_sf$distancia_centro <-
    sqrt((df_features_within_circle_sf$x - df_features_within_circle_sf$x_centro)^2+
           (df_features_within_circle_sf$y - df_features_within_circle_sf$y_centro)^2)


  coordinates4big <- df_features_within_circle_sf %>%
    filter(distancia_centro <= 2 * typical_foraging_dist_big) %>% #dplyr::select(x,y) %>%
    unique() %>%
    mutate(exp_decay = exp(-distancia_centro/typical_foraging_dist_big))

  sum_exp_distances_big <- coordinates4big %>%
    dplyr::select(x,y,exp_decay) %>%
    unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    summarise_all(sum) %>% pull()

  # Sanity check
  sum(coordinates4big$weight*coordinates4big$exp_decay)==sum_exp_distances_big

  coordinates4big$relative_exp_decay <-
    coordinates4big$exp_decay/sum_exp_distances_big


  coordinates4small <- df_features_within_circle_sf %>%
    filter(distancia_centro <= 2 * typical_foraging_dist_small) %>% # dplyr::select(x,y) %>%
    unique() %>%
    mutate(exp_decay = exp(-distancia_centro/typical_foraging_dist_big))

  sum_exp_distances_small <- coordinates4small %>%
    dplyr::select(x,y,exp_decay) %>%
    unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    summarise_all(sum) %>% pull()

  # Sanity check
  sum(coordinates4small$weight*coordinates4small$exp_decay)==sum_exp_distances_small

  coordinates4small$relative_exp_decay <-
    coordinates4small$exp_decay/sum_exp_distances_small

  # Adding nesting sutitability

  HN_pixel_big <- coordinates4big %>% rename(code = codigo) %>%
    left_join(NS_table_big, by=c("code")) %>%
    dplyr::select(latitud,longitud,x,y,species,weight,nesting_suitability) %>%
    mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(latitud,longitud,x,y,species) %>% count(wt = HN_LC_pixel) %>%
    rename(HN_pixel = n) %>%
    ungroup()

  HN_pixel_small <- coordinates4small %>% rename(code = codigo) %>%
    left_join(NS_table_small, by=c("code")) %>%
    dplyr::select(latitud,longitud,x,y,species,weight,nesting_suitability) %>%
    mutate(HN_LC_pixel = weight * nesting_suitability) %>%
    group_by(latitud,longitud,x,y,species) %>% count(wt = HN_LC_pixel) %>%
    rename(HN_pixel = n) %>%
    ungroup()


  # Sanity check
  any(HN_pixel_big$HN_pixel >= 1)
  any(HN_pixel_small$HN_pixel >= 1)

  HF_pixel_big <- unique(coordinates4big[,c("x","y","relative_exp_decay")])

  results_big <- vector("list", nrow(HF_pixel_big))

  results_big <- foreach(cell_i = 1:nrow(HF_pixel_big), .packages = c("dplyr")) %dopar% {

    x_centro <- HF_pixel_big$x[cell_i]
    y_centro <- HF_pixel_big$y[cell_i]

    df_temp <- df_features_within_circle_sf
    df_temp$x_centro <- x_centro
    df_temp$y_centro <- y_centro
    df_temp$distancia_centro <- sqrt((df_temp$x - x_centro)^2 + (df_temp$y - y_centro)^2)

    coordinates4big_cell_i <- df_temp %>%
      filter(distancia_centro <= 2 * typical_foraging_dist_big) %>%
      unique()

    coordinates4big_cell_i$exp_decay <- exp(-coordinates4big_cell_i$distancia_centro / typical_foraging_dist_big)

    # sum_exp_distances_big <- coordinates4big_cell_i %>%
    #   dplyr::select(x,y,exp_decay) %>%
    #   unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    #   summarise_all(sum) %>% pull()

    coordinates4big_cell_i$relative_exp_decay <- coordinates4big_cell_i$exp_decay / sum_exp_distances_big

    HF_pixel_big_cell_i <- coordinates4big_cell_i %>% rename(code = codigo) %>%
      left_join(HF_table_big, by=c("code")) %>%
      mutate(product_pixel = foraging_suitability * weight * relative_exp_decay) %>%
      dplyr::select(species,season,fl_resource_weight,product_pixel) %>%
      group_by(species,season,fl_resource_weight) %>% count(wt = product_pixel) %>%
      rename(sum_product_pixel = n) %>%
      ungroup() %>% mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
      group_by(species) %>% count(wt = product_season_i) %>%
      rename(sum_product_season_i = n) %>%
      ungroup() %>% dplyr::select(-species) %>% pull()

    return(HF_pixel_big_cell_i)
  }

  #coordinates4big$HF_pixel <- unlist(results_big)
  HF_pixel_big$HF_pixel <- unlist(results_big)




  HF_pixel_small <- unique(coordinates4small[,c("x","y","relative_exp_decay")])

  results_small <- vector("list", nrow(HF_pixel_small))

  # results_small <- vector("list", nrow(coordinates4small))

  results_small <- foreach(cell_i = 1:nrow(HF_pixel_small), .packages = c("dplyr")) %dopar% {

    x_centro <- HF_pixel_small$x[cell_i]
    y_centro <- HF_pixel_small$y[cell_i]

    df_temp <- df_features_within_circle_sf
    df_temp$x_centro <- x_centro
    df_temp$y_centro <- y_centro
    df_temp$distancia_centro <- sqrt((df_temp$x - x_centro)^2 + (df_temp$y - y_centro)^2)

    coordinates4small_cell_i <- df_temp %>%
      filter(distancia_centro <= 2 * typical_foraging_dist_small) %>%
      unique()

    coordinates4small_cell_i$exp_decay <- exp(-coordinates4small_cell_i$distancia_centro / typical_foraging_dist_small)

    # sum_exp_distances_small <- coordinates4small_cell_i %>%
    #   dplyr::select(x,y,exp_decay) %>%
    #   unique() %>% ungroup() %>% dplyr::select(exp_decay) %>%
    #   summarise_all(sum) %>% pull()

    coordinates4small_cell_i$relative_exp_decay <- coordinates4small_cell_i$exp_decay / sum_exp_distances_small

    HF_pixel_small_cell_i <- coordinates4small_cell_i %>% rename(code = codigo) %>%
      left_join(HF_table_small, by=c("code")) %>%
      mutate(product_pixel = foraging_suitability * weight * relative_exp_decay) %>%
      dplyr::select(species,season,fl_resource_weight,product_pixel) %>%
      group_by(species,season,fl_resource_weight) %>% count(wt = product_pixel) %>%
      rename(sum_product_pixel = n) %>%
      ungroup() %>% mutate(product_season_i = sum_product_pixel * fl_resource_weight) %>%
      group_by(species) %>% count(wt = product_season_i) %>%
      rename(sum_product_season_i = n) %>%
      ungroup() %>% dplyr::select(-species) %>% pull()

    return(HF_pixel_small_cell_i)
  }

  HF_pixel_small$HF_pixel <- unlist(results_small)

  # POS_big <- coordinates4big %>%
  #   left_join(HN_pixel_big, by = c("latitud","longitud","x","y")) %>%
  #   mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay) %>%
  #   group_by(latitud,longitud) %>% count(wt = PO_pixel_decay) %>%
  #   rename(POs_big = n)

  POS_big_df <- HF_pixel_big %>%
    left_join(HN_pixel_big, by = c("x","y")) %>%
    mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_big <- sum(POS_big_df$PO_pixel_decay)


  # POS_small <- coordinates4small %>%
  #   left_join(HN_pixel_small, by = c("latitud","longitud","x","y")) %>%
  #   mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay) %>%
  #   group_by(latitud,longitud) %>% count(wt = PO_pixel_decay) %>%
  #   rename(POs_small = n)

  POS_small_df <- HF_pixel_small %>%
    left_join(HN_pixel_small, by = c("x","y")) %>%
    mutate(PO_pixel_decay = HN_pixel * HF_pixel * relative_exp_decay)

  POS_small <- sum(POS_small_df$PO_pixel_decay)

  # selected_sites_coordinates$Lonsdorf_big[site_i] <- POS_big$POs_big[1]
  # selected_sites_coordinates$Lonsdorf_small[site_i] <- POS_small$POs_small[1]

  selected_sites_coordinates$Lonsdorf_big[site_i] <- POS_big
  selected_sites_coordinates$Lonsdorf_small[site_i] <- POS_small

  # Save results

  readr::write_csv(selected_sites_coordinates, paste0("Results/Lonsdorf_results_",formatted_time,".csv"))

}




# })
#
# print(inicio)


# typical_foraging_dist_big = 1500 m and resolution = 100 m --> Approx. execution time: 148.30 s/site x 2500 sites (JdA) = 4.28 days (9 cores i7 10th gen)
# typical_foraging_dist_big = 2000 m and resolution = 100 m --> Approx. execution time: 283.13 s/site x 2500 sites (JdA) = 8.19 days (9 cores i7 10th gen)
# typical_foraging_dist_big = 1500 m and resolution = 50 m --> Approx. execution time: 613.10 s/site x 2500 sites (JdA) = 17.40 days (9 cores i7 10th gen)
# typical_foraging_dist_big = 2000 m and resolution = 50 m --> Approx. execution time: 1627.75 s/site x 2500 sites (JdA) = 47.10 days (9 cores i7 10th gen)
