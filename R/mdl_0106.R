#' @title single tree (stem), diameter (breast height)
#' @return single tree (stem), diameter (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 single tree (stem), diameter (breast height)
#' @param p04 species level (stem), basal area (breast height)
#' @param p05 stand (stem), basal area (breast height)
#' @export
#' @rdname mdl_D_106
mdl_D_106 <- function(p01, p02, p03, p04, p05){

	xmdl.2<-function(p02, p03, p04, p05){
		c02 = -1.6749
		c03 = 0.0720
		c04 = -0.0067
		c05 = -0.048
		c01 = 6.5105
		(c01+c02 * log(p02)+c03 * p03+c04 * p04+c05 * p04 / p05)+p03
	}

	xmdl.3<-function(p02, p03, p04, p05){
		c02 = -0.9277
		c03 = 0.0202
		c04 = -0.0048
		c05 = -0.0055
		c01 = 5.0330
		(c01+c02 * log(p02)+c03 * p03+c04 * p04+c05 * p04 / p05)+p03
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05), 
		NA
	)))
}
