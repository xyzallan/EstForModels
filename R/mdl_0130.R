#' @title stand (stem), number of trees 
#' @return stand (stem), number of trees 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @export
#' @rdname mdl_N_130
mdl_N_130 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = -340
		c03 = 3.7
		c01 = 4950
		c01+c02 * p02+c03 * p02^2
	}

	xmdl.2<-function(p02){
		c02 = -200
		c03 = 8.1
		c01 = 3300
		c01+c02 * p02+c03 * p02^2
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
	)))
}
