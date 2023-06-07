#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
mdl_L_128 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 58.0
		c01 = 13.5
		c01 * p02+c02
	}

	xmdl.2<-function(p02){
		c02 = 84.2
		c01 = 12.5
		c01 * p02+c02
	}

	xmdl.3<-function(p02){
		c02 = 73.8
		c01 = 13.8
		c01 * p02+c02
	}

	xmdl.4<-function(p02){
		c02 = 74.3
		c01 = 13.6
		c01 * p02+c02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		NA
	)))))
}
