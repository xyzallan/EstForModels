#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), age 
mdl_V_39 <- function(p01, p02, p03){

	xmdl.7<-function(p02, p03){
		c02 = 3.9
		c01 = 1.9
		c01+c02 * p02^2 / sqrt(p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		NA
	))
}
