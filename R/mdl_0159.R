#' @title single tree (stem), form 
#' @return single tree (stem), form 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @export
#' @rdname mdl_F_159
mdl_F_159 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.93964
		c03 = 1.5464
		c04 = -2.0482
		c05 = -5.7305
		c06 = 17.444
		c01 = 0.35096
		c01+c02 / p02+c03 / p03+c04 / (p02 * p02)+c05 / (p02 * p03)+c06 / (p03 * p02 * p02)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
	))
}
