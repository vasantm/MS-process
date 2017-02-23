dec_time_2_time <-function(x){
  #strptime(
  as.POSIXct(x * 60, origin = Sys.Date(), tz = "GMT")
  #format = "%M:%S")
}

cor_checker <-function(d,idx, cutoff=0.9){
  r2cut = sqrt(cutoff)
  df <-t(d[idx,-c(1:3)])
  df.cormat <- cor(df)
  df.cormat.lower <-abs(df.cormat[lower.tri(df.cormat)])
  df.results <- data.frame(min = min(df.cormat.lower, na.rm = T),
                           max = max(df.cormat.lower, na.rm = T))
  df.results$passes = if (df.results$max < r2cut) TRUE
  df.results$Corr_Frames <-list(which(df.cormat ==
                                        max(df.cormat[lower.tri(df.cormat)],
                                            na.rm = T), arr.ind = T)[,1])
  return(df.results)

}
cor_checker(pos_data,pos_data_lowcorr.0.9.index)
