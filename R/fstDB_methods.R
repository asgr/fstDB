`[.fstDB` = function(x, i, j, j_logic='auto', limit_sub=NULL, cores=length(x), log_sub=TRUE){

  start_time = proc.time()[3]

  has_i = !missing(i)
  has_j = !missing(j)

  subN = NULL

  if(!is.null(limit_sub)){
    x = foreach(subN = limit_sub)%do%{x[[subN]]}
  }

  if(cores > length(x)){
    cores = length(x)
  }

  if(cores > detectCores()){
    cores = detectCores()
  }

  registerDoParallel(cores=cores)

  if(!missing(i)){
    if(is.character(i)){
      i = str2expression(i)
    }

    if(j_logic[1] == 'auto'){
      colname = NULL
      j_logic = foreach(colname = colnames(x[[1]]), .combine='c')%do%{
        temp = grep(colname, as.character(i))
        if(length(temp) > 0){
          return(colname)
        }else{
          return(NULL)
        }
      }
      if(length(j_logic) == 0){
        stop('Cannot find any likely columns to use for specified logic!')
      }
    }
  }

  output = foreach(subN = 1:length(x))%dopar%{
    if(!has_i & !has_j){
      tempDT = as.data.table(x[[subN]][,,drop=FALSE])
    }else if(!has_i & has_j){
      tempDT = as.data.table(x[[subN]][,j,drop=FALSE])
    }else if(has_i & !has_j){
      tempDT = as.data.table(x[[subN]][,,drop=FALSE])
      tempDT = tempDT[eval(i),,drop=FALSE]
    }else if(has_i & has_j){
      if(!is.null(j_logic)){
        tempDT = as.data.table(x[[subN]][,j_logic,drop=FALSE])
        sel = tempDT[eval(i),.I]
        if(length(sel) > 0){
          tempDT = as.data.table(x[[subN]][sel,j,drop=FALSE])
        }else{
          tempDT = NULL
        }
      }else{
        tempDT = as.data.table(x[[subN]][,j,drop=FALSE])
        tempDT = tempDT[eval(i),,drop=FALSE]
      }
    }

    if(log_sub & !is.null(tempDT)){
      .log_sub = NULL
      tempDT[,.log_sub:=subN]
    }

    return(tempDT)
  }

  output = rbindlist(output)

  if(log_sub & !is.null(output)){
    attributes(output)$.log_sub = output$.log_sub
    output$.log_sub = NULL
  }

  if(has_i){
    attributes(output)$.log_i = as.character(i)
  }

  attributes(output)$.time = proc.time()[3] - start_time

  return(output)
}

dim.fstDB = function(x){
  colN = dim(x[[1]])[2]
  i = NULL
  rowN = foreach(i = 1:length(x), .combine='sum')%do%{dim(x[[i]])[1]}
  return(c(rowN, colN))
}
