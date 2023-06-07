#' @title single tree (stem), formheight 
#' @return single tree (stem), formheight 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @param p04 single tree (crown), height 
#' @param p05 stand (stem), height (dominant)
#' @param p06 stand (stem), basal area (breast height)
#' @export
#' @rdname mdl_HF_135
mdl_HF_135 <- function(p01, p02, p03, p04, p05, p06){

	xmdl.1<-function(p02, p03, p04, p05, p06){
		c02 = -0.0381
		c03 = 2.4243
		c04 = 6.5014
		c05 = -1.6114
		c06 = -0.0638
		c07 = -0.0021
		c08 = -0.0020
		c01 = 0.8217
		1-exp(-(c01+c02 * (p02 / p03)^c03+c04 * p03^c05+c06 * p04+c07 * p05+c08 * p06))
	}

	with(data.frame( p01, p02, p03, p04, p05, p06 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06), 
		NA
	))
}
