######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

downloadChelsa_monthly = function(timeperiod = NULL, var = NULL, climmodel = NULL, spp = NULL, outdir = NULL,
                                  ignore.stdout = FALSE, ignore.stderr = FALSE){
  
  wget = "/usr/bin/wget" 
  if(Sys.info()["sysname"]=="Darwin") wget = "/opt/homebrew/bin/wget"
  
  baseURL = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/"
  tprds = c("1981-2010","2011-2040","2041-2070","2071-2100")
  vars = c("pr","tas","tasmax","tasmin")
  cmls = c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
  spps = c("ssp126","ssp370","ssp585")
  
  if(is.null(timeperiod) | is.null(var)){
    stop("select timeperiod, var, climmodel and spp","\n",
         "timeperiod: ",paste(tprds,collapse="; "),"\n",
         "var: ",paste(vars,collapse="; "),"\n",
         "climmodel: ",paste(cmls,collapse="; "),"\n",
         "spp: ",paste(spps,collapse="; "),"\n")
  }else{
    
    if(is.numeric(timeperiod)) timeperiod = tprds[timeperiod]
    if(is.numeric(var)) var = vars[var]
    if(is.numeric(climmodel)) climmodel = cmls[climmodel]
    if(is.numeric(spp)) spp = spps[spp]
    
    if(timeperiod=="1981-2010"){
      
      cat("downloading current data for:",timeperiod,var,"\n")
      for(mm in c("01","02","03","04","05","06","07","08","09","10","11","12")){
        url = paste0(baseURL,timeperiod,"/",var,"/CHELSA_",var,"_",mm,"_",timeperiod,"_V.2.1.tif")
        filename = paste0("monthly_",var,"_",mm,"_",timeperiod,".tif")
        print(url)
        cmd = paste0(wget," --continue -O ",outdir,filename," ",url)
        print(cmd)
        out=system(cmd,intern=TRUE,ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
      }

      
    }else{
      
      cat("downloading future data for:",timeperiod,var,climmodel,spp,"\n")
      for(mm in c("01","02","03","04","05","06","07","08","09","10","11","12")){
        url = paste0(baseURL,timeperiod,"/",climmodel,"/",spp,"/",var,"/CHELSA_",tolower(climmodel),"_r1i1p1f1_w5e5_",spp,"_",var,"_",mm,"_",gsub("-", "_", timeperiod),"_norm.tif")
        filename = paste0("monthly_",var,"_",mm,"_",timeperiod,"_",climmodel,"_",spp,".tif")
        print(url)
        cmd = paste0(wget," --continue -O ",outdir,filename," ",url)
        print(cmd)
        out=system(cmd,intern=TRUE,ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
      }
      
    }
  }
}

downloadChelsa_bioclim = function(timeperiod = NULL, var = NULL, climmodel = NULL, spp = NULL, outdir = NULL){
  
  wget = "/usr/bin/wget" 
  if(Sys.info()["sysname"]=="Darwin") wget = "/opt/homebrew/bin/wget"
  
  baseURL = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/"
  tprds = c("1981-2010","2011-2040","2041-2070","2071-2100")
  vars = c(paste0("bio",1:19),paste0("kg",0:5))
  cmls = c("GFDL-ESM4","IPSL-CM6A-LR","MPI-ESM1-2-HR","MRI-ESM2-0","UKESM1-0-LL")
  spps = c("ssp126","ssp370","ssp585")
  
  if(is.null(timeperiod) | is.null(var)){
    stop("select timeperiod, var, climmodel and spp","\n",
         "timeperiod: ",paste(tprds,collapse="; "),"\n",
         "var: ",paste(vars,collapse="; "),"\n",
         "climmodel: ",paste(cmls,collapse="; "),"\n",
         "spp: ",paste(spps,collapse="; "),"\n")
  }else{
    
    if(is.numeric(timeperiod)) timeperiod = tprds[timeperiod]
    if(is.numeric(var)) var = vars[var]
    if(is.numeric(climmodel)) climmodel = cmls[climmodel]
    if(is.numeric(spp)) spp = spps[spp]
    
    if(timeperiod=="1981-2010"){
      cat("downloading current data for:",timeperiod,var,"\n")
      # https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio10_1981-2010_V.2.1.tif
      url = paste0(baseURL,timeperiod,"/bio/CHELSA_",var,"_",timeperiod,"_V.2.1.tif")
      filename = paste0("bioclim_",var,"_",timeperiod,".tif")
      print(url)
      cmd = paste0(wget," --continue -O ",outdir,filename," ",url)
      print(cmd)
      system(cmd)
      
    }else{
      cat("downloading future data for:",timeperiod,var,climmodel,spp,"\n")
      # https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/2011-2040/MPI-ESM1-2-HR/ssp126/bio/CHELSA_bio10_2011-2040_mpi-esm1-2-hr_ssp126_V.2.1.tif
      url = paste0(baseURL,timeperiod,"/",climmodel,"/",spp,"/bio/CHELSA_",var,"_",timeperiod,"_",tolower(climmodel),"_",spp,"_V.2.1.tif")
      filename = paste0("bioclim_",var,"_",timeperiod,"_",climmodel,"_",spp,".tif")
      print(url)
      cmd = paste0(wget," --continue -O ",outdir,filename," ",url)
      print(cmd)
      system(cmd)
      
    }
  }
}

downloadChelsa_monthlyYearSpecific = function(var = NULL, outdir = NULL, year = 2000, ignore.stdout = TRUE, ignore.stderr = TRUE){
  
  wget = "/usr/bin/wget" 
  if(Sys.info()["sysname"]=="Darwin") wget = "/opt/homebrew/bin/wget"
  
  baseURL = "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/"

  for(mm in c("01","02","03","04","05","06","07","08","09","10","11","12")){
    url = paste0(baseURL,var,"/CHELSA_",var,"_",mm,"_",year,"_V.2.1.tif")
    filename = paste0("monthly_",var,"_",mm,"_",year,".tif")
    print(url)
    cmd = paste0(wget," --continue -O ",outdir,filename," ",url)
    print(cmd)
    out=system(cmd,intern=TRUE,ignore.stdout = ignore.stdout, ignore.stderr = ignore.stderr)
  }
  
}

# dl_dir = "/Volumes/milkunarc/shoeks/Project_TreeChange2/RawData/"
# downloadChelsa_monthly(timeperiod = 1, var = 1, outdir = dl_dir)
# downloadChelsa_monthly(timeperiod = 2, var = 1, climmodel = 1, spp = 1, outdir = dl_dir)

# dl_dir = "/Volumes/milkunarc/shoeks/Project_TreeChange2/RawData/"
# downloadChelsa_bioclim(timeperiod = 1, var = 1, outdir = dl_dir)
# downloadChelsa_bioclim(timeperiod = 2, var = 1, climmodel = 1, spp = 1, outdir = dl_dir)


grepcol = function(x,dt){
    colnames <- colnames(dt)
    grep(x,colnames,value=TRUE)
}
