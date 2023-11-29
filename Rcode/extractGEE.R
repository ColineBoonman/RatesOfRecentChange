######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

#--------- mblm slope function
calc_mblm_slope_pval = function(df, row_grep_id, plot=FALSE){
  df$mblm_slope = df$mblm_slope_pval = NA
  print(grep(row_grep_id,names(df),value=TRUE))
  for(i in 1:nrow(df)){
    y = as.numeric(df[i,grep(row_grep_id,names(df))])
    x = 1:length(y)
    #print(i)
    if(sum(y,na.rm = TRUE)>0 | sum(y,na.rm = TRUE)<0){
      lm=mblm::mblm(y~x, repeated = TRUE)
      df$mblm_slope[i] = as.numeric(coef(lm)[2])
      df$mblm_slope_pval[i] = summary(lm)$coefficients[2,4]
    }
    if(plot) {
      plot(x,y)
      abline(lm)
    }
  }
  return(df)
}

#--------- extract sp data function
extract_sp_data = function(env_data,shp,n_batch,scale_set=1000,funct="sum",tileScale=1){
  n_sp=shp$size()$getInfo()
  idx_start = c(seq(0,n_sp-1,by=n_batch)) 
  extract_df = data.frame()
  n_loop = length(idx_start)
  for(i in 1:n_loop){
    print(paste(i,"/",n_loop))
    if(i==n_loop){
      n_batch_set = n_sp-idx_start[i]
    }else{
      n_batch_set = n_batch
    }
    batch_List = shp$toList(count=n_batch_set, offset=idx_start[i])
    shp_batch = ee$FeatureCollection(batch_List)
    #if(shp_batch$size()$getInfo()==0) next
    if(funct=="sum"){
      tmp_df = ee_extract(x = env_data, y = shp_batch, fun = ee$Reducer$sum(), scale = scale_set, tileScale)
    }else if(funct=="mean"){
      tmp_df = ee_extract(x = env_data, y = shp_batch, fun = ee$Reducer$mean(), scale = scale_set, tileScale)
    }
    extract_df = rbind(extract_df,tmp_df)
  }
  return(extract_df)
}

#--------- extract sp data function
extract_sp_data_per_climregion_chunk = function(env_data,shp,scale_set=1000,funct="sum",tileScale=1,
                                                clim_region_image,clim_region_occ_species){
  
  # params for testing
  # clim_region_image = kg2
  # env_data = cl
  # shp = polys
  # clim_region_occ_species = clim_occ
  # scale_set = 1000
  # funct="mean"
  # tileScale=1
  # color = viridis::viridis_pal(option = "D")(31)
  # color = sample(color,31)
  
  # clim region chunk code --------------
  extract_df = data.frame()
  n_chunk = length(unique(clim_region_occ_species$occ_chunk))
  n_sp_done = 0
  counter = 1
  
  for(chunk in unique(clim_region_occ_species$occ_chunk)){
    
    cat(counter,"/",n_chunk,"\n")
    
    # filter sp polygons by chunk
    chunk_sp_to_filter = clim_region_occ_species$species[which(clim_region_occ_species$occ_chunk==chunk)]
    if(length(chunk_sp_to_filter)==1) {
      shp_chunk = polys$filter(ee$Filter$eq("id",chunk_sp_to_filter))
    }else{
      filter_chunk = ee$Filter$inList(opt_leftField = "id", opt_rightValue = chunk_sp_to_filter)
      shp_chunk = shp$filter(filter_chunk)
    }
    cat("n species:",length(chunk_sp_to_filter),"\n")
    
  
    # create kg2 mask specific to chunk
    kg2_occ_regions = colSums(clim_region_occ_species[clim_region_occ_species$occ_chunk==chunk,grep("kg",names(clim_region_occ_species))])
    kg2_occ_regions_names = unlist(strsplit(names(kg2_occ_regions),"_"))
    kg2_occ_regions_names = as.numeric(kg2_occ_regions_names[kg2_occ_regions_names!="kg2"])
    kg2_occ_regions_mask = kg2_occ_regions_names[which(kg2_occ_regions==0)]
    kg2_filtered = clim_region_image
    for(not_occ in (kg2_occ_regions_mask)) kg2_filtered = kg2_filtered$updateMask(clim_region_image$neq(not_occ))
    
    # mask env data
    env_data_masked = env_data$updateMask(kg2_filtered$neq(0))
  
    # plot for debugging
    # Map$centerObject(shp_chunk, zoom = 6)
    # m=Map$addLayer(env_data_masked$select("polygon_m2"),list(palette = color),"env_data_mask")
    # m=m+Map$addLayer(env_data$select("polygon_m2"),list(palette = color),"env_data")
    # m=m+Map$addLayer(kg2_filtered, list(min = 0, max = 31, palette= color), name = "kg2_masked")
    # m=m+Map$addLayer(ee$Image()$paint(shp_chunk, 0, 2),list(palette = "yellow"),"Selected")
    # print(m)

    # extract values
    tic()
    if(funct=="sum"){
      tmp_df = tryCatch({ 
        ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$sum(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$sum(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$sum(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$sum(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      cat("extraction resulting in nrow:",nrow(tmp_df),"\n")
      
    }else if(funct=="mean"){
      tmp_df = tryCatch({ 
        ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$mean(), scale = scale_set, tileScale)
      },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$mean(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$mean(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      if(nrow(tmp_df)==0) {
        cat("extraction failed trying again...","\n")
        tmp_df = tryCatch({ 
          ee_extract(x = env_data_masked, y = shp_chunk, fun = ee$Reducer$mean(), scale = scale_set, tileScale) 
        },warning = function(w) {data.frame()}, error = function(e) {data.frame()})
      }
      cat("extraction resulting in nrow:",nrow(tmp_df),"\n")
    }
    toc()
    
    if(nrow(tmp_df)>0){
      # add chunk index, order and occ kg2
      tmp_df = tmp_df[order(tmp_df$id),]
      tmp_df$chunk_index = chunk
      rowidx_clim_occ = match(tmp_df$id,clim_occ$species)
      colidx = c("species",grep("kg",names(clim_occ),value=TRUE))
      tmp_df[,colidx] = clim_occ[rowidx_clim_occ,colidx]
      
      # insert into output data.frame
      extract_df = rbind(extract_df,tmp_df)
      n_sp_done = n_sp_done+nrow(tmp_df)
      cat("n species done:",n_sp_done,"\n")
    }else{
      cat("no data extracted for chunk:",chunk,"\n")
    }
    counter = counter+1
    cat(" ","\n")
    
  }

  return(extract_df)
}

crop_geometry = function(min_long,max_long,min_lat,max_lat){
  geometry = ee$Geometry$Rectangle(coords = c(min_long,min_lat,max_long,max_lat),proj="EPSG:4326",geodesic=FALSE)
  return(geometry)
}

merge_downloaded_rasters = function(rasts){
  for(i in 1:length(rasts)){
    print(i)
    if(i==1){
      rt = terra::rast(rasts[i])
    }else{
      rt = terra::merge(rt,terra::rast(rasts[i]))
    }
  }
  return(rt)
}