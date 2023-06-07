#' @title single tree (stem), biomass (above ground)
#' @return single tree (stem), biomass (above ground)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
mdl_BM_213 <- function(p01, p02){

	xmdl.3<-function(p02){
		c01 = 0.0
		c02 = 0.0
		(c02 * 0.00001)*p02^c03
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('NA'), xmdl.3(p02), 
		NA
	))
}
