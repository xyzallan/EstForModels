#' @title single tree (stem), diameter (breast height)
#' @return single tree (stem), diameter (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (crown), height 
#' @param p04 stand (stem), basal area (breast height)
#' @param p05 stand (larger trees), basal area (breast height)
#' @param p06 stand (stem), height (dominant)
#' @param p07 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_D_136
mdl_D_136 <- function(p01, p02, p03, p04, p05, p06, p07){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07){
		c02 = 0.7917
		c03 = 0.5557
		c04 = -0.0010
		c05 = -0.00075
		c06 = -0.6470
		c07 = 1.4995
		c08 = -0.4349
		c01 = 0.0504
		c01 * p02^c02 * p03^c03 * exp(c04 * p02^2+c05 * p05^2) * p06^c06 * p07^c07 * p04^c08
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07), 
		NA
	))
}
