#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
mdl_L_127 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = -1.844
		c01 = 12.669
		10000 / sqrt(exp(c01+c02 * log(p02)))
	}

	xmdl.2<-function(p02){
		c02 = -1.301
		c01 = 11.414
		10000 / sqrt(exp(c01+c02 * log(p02)))
	}

	xmdl.3<-function(p02){
		c02 = -2.331
		c01 = 13.981
		10000 / sqrt(exp(c01+c02 * log(p02)))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		NA
	))))
}
