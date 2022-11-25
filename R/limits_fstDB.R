limits_fstDB = function(fstDB, columns=NULL, cores=length(fstDB), na.rm=TRUE){

  if(is.null(columns)){
    columns = colnames(fstDB[[1]])
  }

  if(is.integer(columns)){
    columns = colnames(fstDB[[1]])[columns]
  }

  if(cores > length(fstDB)){
    cores = length(fstDB)
  }

  if(cores > detectCores()){
    cores = detectCores()
  }

  registerDoParallel(cores=cores)

  col = NULL
  i = NULL

  limits_fstDB = foreach(col = columns)%do%{
    tempmat = foreach(i = 1:length(fstDB), .combine='rbind')%dopar%{
      range(fstDB[[i]][,col], na.rm=na.rm)
    }
    rownames(tempmat) = NULL
    colnames(tempmat) = c('min', 'max')
    return(tempmat)
  }

  names(limits_fstDB) = columns

  class(limits_fstDB) = 'limits_fstDB'

  return(limits_fstDB)
}

check_limits_fstDB = function(limits_list, limits_fstDB){
  limit = NULL
  output = foreach(limit = 1:length(limits_list), .combine='&')%do%{
    limit_name = names(limits_list)
    sel = limits_fstDB[[limit_name[limit]]][,2] >= limits_list[[limit]][1] & limits_fstDB[[limit_name[limit]]][,1] <= limits_list[[limit]][2]
    return(sel)
  }
  return(which(output))
}
