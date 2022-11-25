read_fstDB = function(dirlist=".", filelist=NULL, pattern="^.*sub.*\\.fst$",
                      recursive=TRUE, combine=FALSE){
  if(missing(filelist)){
    if(is.null(dirlist)){
      stop('Missing filelist and dirlist')
    }
    filelist = {}
    for(i in 1:length(dirlist)){
      filelist = c(filelist,
                   list.files(dirlist[i], pattern=pattern, full.names=TRUE, recursive=recursive))
    }
  }

  filelist = grep(pattern=pattern, filelist, value=TRUE)
  filelist = normalizePath(filelist)

  output = foreach(i = 1:length(filelist))%do%{
    fst(filelist[i])
  }
  class(output) = 'fstDB'
  if(combine){
    return(output[,])
  }else{
    return(output)
  }
}

write_fstDB = function(x, name='example_fstDB', sub='sub', sub_col='.log_sub',
                       compress=50, uniform_encoding=TRUE, cores=1, append=FALSE,
                       breakby=NULL, breaks=10, trim=FALSE){
  is_fstDB = testClass(x, 'fstDB')
  is_list = testList(x)
  is_DT = testDataTable(x)
  is_DF = testDataFrame(x)

  is_list_like = (is_fstDB | is_list) & !is_DF & !is_DT

  if(is.null(breakby)){
    if(is_list_like){
      Nlist_position = length(x)
    }else{
      if(sub_col %in% names(x)){
        sub_vec = x[[sub_col]]
      }else if(sub_col %in% names(attributes(x))){
        sub_vec = attributes(x)[[sub_col]]
      }
    }
  }else{
    if(!is_list_like){
      col_data = x[[breakby]]
      if(length(breaks) == 1){
        break_range = range(col_data, na.rm=TRUE)
        breaks = seq(break_range[1], break_range[2], len=breaks + 1L)
      }
      if(trim){
        if(anyNA(col_data)){
          x = x[!is.na(col_data),]
        }
        x = x[col_data >= breaks[1] & col_data <= max(breaks, na.rm=TRUE),]
      }else{
        if(anyNA(col_data)){
          col_data[is.na(col_data)] = max(breaks, na.rm=TRUE)
        }
      }
      sub_vec = findInterval(col_data, breaks, all.inside=TRUE)
      sub_vec_unique = sort(unique(sub_vec))
      Nlist_position = length(sub_vec_unique)
    }else{
      stop('Breaks only work on data.table / data.frame!')
    }
  }

  path = paste0(name,'/')

  if(dir.exists(path) == FALSE){
    dir.create(path, recursive = TRUE)
  }

  if(append){
    current = list.files(path)
    offset = as.integer(strsplit(strsplit(current[length(current)], paste0(sub,'_'))[[1]][2], '.fst')[[1]][1])
  }else{
    offset = 0L
    current = list.files(path, full.names = TRUE)
    file.remove(current)
  }

  registerDoParallel(cores=cores)

  i = NULL
  dummy = foreach(i = 1:Nlist_position)%dopar%{
    filename = paste0(path,sub,'_',formatC(i + offset,width=4,flag=0),'.fst')
    filename = path.expand(filename)

    assertPathForOutput(filename, overwrite=TRUE)

    if(is_fstDB){
      write.fst(x=x[[i]][,],
                path=filename,
                compress=compress,
                uniform_encoding=uniform_encoding
      )
    }else if(is_DT | is_DF){
      write.fst(x=x[sub_vec == sub_vec_unique[i],],
                path=filename,
                compress=compress,
                uniform_encoding=uniform_encoding
      )
    }else if(is_list){
      if(is.data.frame(x[[i]])){
        write.fst(x=x[[i]],
                  path=filename,
                  compress=compress,
                  uniform_encoding=uniform_encoding
        )
      }else{
        stop('Elements of x list must be data.frame / data.table!')
      }
    }
  }
}
