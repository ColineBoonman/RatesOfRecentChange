######################
## This file is created by S. Hoeks - any questions should be directed to him, s.hoeks@science.ru.nl
######################

mblm_slopes_pval = function(data,cols) {
    slopes = rep(NA,nrow(data))
    pvals = rep(NA,nrow(data))
    for(i in 1:nrow(data)){
        y = as.numeric(data[i,.SD,.SDcols=cols])
        x = 1:length(y)
        if(sum(y,na.rm = TRUE)>0 | sum(y,na.rm = TRUE)<0){
        lm=mblm(y~x, repeated = TRUE)
        slopes[i] = as.numeric(coef(lm)[2])
        pvals[i] = summary(lm)$coefficients[2,4]
        }
    }
    out = data.table(species=data$species,slopes=slopes,pvals=pvals)
    return(out)
}

histm = function(x,xlim,breaks){
    png()
    hist(x,xlim=xlim,breaks=breaks)
    dev.off()
}

match_no_na = function(x,y){
    m = match(x,y)
    m = m[complete.cases(m)]
    return(m)
}