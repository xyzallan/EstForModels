#' @title stand (stem), distance between 
#' @return stand (stem), distance between 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
mdl_L_126 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.928
		c03 = 19.94
		c04 = -0.22
		c01 = -5.2
		c01+c02 * p02+(c03+c04 * p03) * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
	))
}
