#' @title stand (stem), number of trees 
#' @return stand (stem), number of trees 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
mdl_N_131 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = -300
		c03 = 6.7
		c01 = 3800
		c01+c02 * p02+c03 * p02^2
	}

	xmdl.2<-function(p02){
		c02 = -145
		c03 = 6.7
		c01 = 2500
		c01+c02 * p02+c03 * p02^2
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
	)))
}
