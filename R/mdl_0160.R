#' @title single tree (stem), form 
#' @return single tree (stem), form 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @export
#' @rdname mdl_F_160
mdl_F_160 <- function(p01, p02, p03){

	xmdl.15<-function(p02, p03){
		c03 = -5.59827
		c04 = -0.2101
		c05 = 0.12363
		c06 = 21.92938
		c01 = 0.583
		c02 = 4.52132
		c01+c02 / (p03 * p03)+c03 / (p03 * p02)+c04 * log(p02) / log(10.0)+c05 * log(p03) / log(10.0)+c06 / (p02 * p03 * p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03), 
		NA
	))
}
