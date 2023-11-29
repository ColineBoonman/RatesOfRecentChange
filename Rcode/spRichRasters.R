######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

plotm = function(x,name) {
    png(name)
    plot(x)
    dev.off()
}

# functions
get_sp_occ_regions = function(sp_chunk,cols) {
    occ = sp_chunk[1,cols]
    occ$geometry = NULL
    occ = as.numeric(str_split(names(occ)[which(occ==1)],"_",simplify=TRUE)[,2])
    return(occ)
}

get_occ_cell_indices = function(sp,i,resDir,out_raster,kg2,colselect) {
    sp_chunk = sp[sp$chunk==i,]
    sp_file = glue("{resDir}/speciesCellOccIndices_chunk{i}.fst")
    sp_raster = fasterize(sp_chunk,out_raster,fun="sum")
    idx = which(sp_raster[]>0)
    idx = data.table(idx=idx,kg2=kg2[idx])
    names(idx) = c("idx","kg2")
    setkey(idx,idx)
    occ = get_sp_occ_regions(sp_chunk,colselect)
    idx = idx[kg2%in%occ,]
    idx$val = sp_raster[idx[,idx]]
    write_fst(idx,sp_file)
}

get_occ_cell_indices_loop = function(sp,ivec,resDir,out_raster,kg2,colselect){
    for(i in ivec) {
        if(!file.exists(glue("{resDir}/speciesCellOccIndices_chunk{i}.fst"))) {
            get_occ_cell_indices(sp,i,resDir,out_raster,kg2,colselect)
        }
    }
}