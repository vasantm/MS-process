remCor.lowintensity <- function(d, cutoff=0.8, measure=median) {
	#This function returns indices of columns that should be retained, after dropping correlated columns (that have lower intensity).
	#After getting list of indices that are correlated, 
	# it checks their median values, and drops the one with lower value.
	#You can supply your own measure - maybe mean, sd or var in the function call.
  #This script is based off of Rajarshi Guha's code  http://www.rguha.net/code/R/#r2test 

	if(cutoff > 1 || cutoff <= 0) {
		stop("cutoff should be 0 < cutoff <= 1")
	}
	if(!is.matrix(d) && !is.data.frame(d)){
		stop("Please supply a data.frame or a matrix.")
	}
	r2cut = sqrt(cutoff)
	cormat <- cor(d)
	measure.index <-apply(d,2,measure) #calculate a measure for all columns
	bad.id <- which(abs(cormat)>r2cut, arr.ind=T);
	bad.id <- matrix(bad.id[bad.id[,1] > bad.id[,2]], ncol=2);
	drop.id <-ifelse(measure.index[bad.id[,1]] < measure.index[bad.id[,2]],
	                 bad.id[,1], bad.id[,2]) # this was inverted, fixed - 6 Sep 2016, changed it back
	#the above line assigns to drop id the peak with lower id
	#we then exclude this from cols to be retained by -ve subsetting
	#below in the return part, thereby getting a matrix with the higher intensity peak
	
	#check measure for both columns in bad.id and assign to drop.id the 
	#one with higher value
	if(length(drop.id)==0){
	  1:ncol(d)
	} else {
	  (1:ncol(d))[-unique(drop.id)]
	}
}


