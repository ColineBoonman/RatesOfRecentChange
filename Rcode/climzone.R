######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

extentWithPadding = function(sf_obj){
  e = ext(sf_obj)
  e[1] = e[1] - 0.1
  e[2] = e[2] + 0.1
  e[3] = e[3] - 0.1
  e[4] = e[4] + 0.1
  if((e[2]-e[1])<0.3 | e[4]-e[3]<0.3) {
    e[1] = e[1] - 0.25
    e[2] = e[2] + 0.25
    e[3] = e[3] - 0.25
    e[4] = e[4] + 0.25
  }
  return(e)
}

extentWithPaddingCustom_floor = function(sf_obj,pad=0.01){
  e = ext(sf_obj)
  e[1] = floor(e[1] - pad)
  e[2] = ceiling(e[2] + pad)
  e[3] = floor(e[3] - pad)
  e[4] = ceiling(e[4] + pad)
  return(e)
}

extentWithPaddingCustom = function(sf_obj,pad=0.01){
  e = ext(sf_obj)
  e[1] = e[1] - pad
  e[2] = e[2] + pad
  e[3] = e[3] - pad
  e[4] = e[4] + pad
  return(e)
}

replaceNAwith0 = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=0]
}

numPad = function(num,width=7){
 return(stringr::str_pad(num,width,"left","0"))
}

mostFreqVal = function(vector) {
  vector = vector[complete.cases(vector)]
  return(as.numeric(names(sort(table(vector),decreasing=TRUE))[1]))
}

getChunkIndices = function(n,by=1e3) {
  strt = seq(1,n,by=by)
  stop = strt+(by-1)
  stop[length(stop)] = n
  strt_name = stringr::str_pad(strt,5,"left","0")
  stop_name = stringr::str_pad(stop,5,"left","0")
  chunk_index = 1:length(strt)
  chunk_name = paste0(strt_name,"_",stop_name)
  return(data.frame(chunk_index,chunk_name,strt,stop,strt_name,stop_name,chunk_name))
}