remCor.lowintensity_RT <- function(d, cutoff = 0.9, measure = median, RT = 20) {
  #This function returns indices of columns that should be retained, 
  #after dropping correlated columns that have lower intensity & if 
  #they're within a 20 sec time window
  
  #After getting list of indices that are correlated, 
  # it checks their median values, and drops the one with lower value.
  #You can supply your own measure - maybe mean, sd or var in the function call.
  
  
  if(cutoff > 1 || cutoff <= 0) {
    stop("cutoff should be 0 < cutoff <= 1")
  }
  if(!is.matrix(d) && !is.data.frame(d)){
    stop("Please supply a data.frame or a matrix.")
  }
  r2cut = sqrt(cutoff)
  cormat <- cor(d[-1,]) #1st row has Time 
  measure.index <-apply(d[-1,],2,measure) #calculate a measure for all columns
  
  bad.id <- which(abs(cormat)>r2cut, arr.ind=T);
  bad.id <- matrix(bad.id[bad.id[,1] > bad.id[,2]], ncol=2)
  bad.id <- data.frame(bad.id)
  bad.id$time_1 <-d[1, bad.id[,1]]
  bad.id$time_1_ms <-dec_time_2_time(bad.id$time_1)
  bad.id$time_2 <-d[1, bad.id[,2]]
  bad.id$time_2_ms <-dec_time_2_time(bad.id$time_2)
  bad.id$time_diff <-abs(bad.id$time_1 - bad.id$time_2)
  drop.id <-ifelse((measure.index[bad.id[,1]] < measure.index[bad.id[,2]]) &
                     bad.id$time_diff <= 20, bad.id[,1], bad.id[,2])
  drop.id <-ifelse((measure.index[bad.id[,2]] < measure.index[bad.id[,1]]) &
                     bad.id$time_diff <= 20, bad.id[,2], bad.id[,1])
  #if ((measure.index[bad.id[,1]]<measure.index[bad.id[,2]]) &
   #                  bad.id$time_diff <=20){
  #  drop.id <- bad.id[,2]
  #} else if ((measure.index[bad.id[,2]]<measure.index[bad.id[,1]]) &
   #                  bad.id$time_diff <=20) {
  #  drop.id <- bad.id[,1]
  #}
  
   
  #check measure for both columns in bad.id and 
  #assign to drop.id the one with higher value
  if(length(drop.id)==0){
    1:ncol(d)
  }else{
    (1:ncol(d))[-unique(drop.id)]
  }
}
