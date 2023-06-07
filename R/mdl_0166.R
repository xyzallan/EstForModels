#' @title single tree (stem), form 
#' @return single tree (stem), form 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @export
#' @rdname mdl_F_166
mdl_F_166 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 3.34262
		c03 = -1.73375
		c04 = -0.26215
		c05 = 0.18736
		c06 = 11.34436
		c01 = 0.5848
		c01+c02 / (p03 * p03)+c03 / (p03 * p02)+c04 * log(p02) / log(10.0)+c05 * log(p03) / log(10.0)+c06 / (p02 * p03 * p03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
