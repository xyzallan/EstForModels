#' @title single tree (stem), formheight 
#' @return single tree (stem), formheight 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), height 
#' @param p03 single tree (stem), diameter (breast height)
mdl_HF_134 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = -0.0263
		c03 = 3.0028
		c04 = 3.7094
		c05 = -1.2138
		c01 = 0.6390
		p02 * (1-exp(-(c01+c02 * (p03 / p02)^c03+c04 * p02^c05)))
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
	))
}
