#' @title single tree (stem), volume 
#' @return single tree (stem), volume 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @export
#' @rdname mdl_V_185
mdl_V_185 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.00001142
		c03 = 2.61614
		c04 = 0.76489
		c01 = 0.000019
		c01+c02 * (p02+2)^c03 * p03^c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA','KU','KS','HB','LM','LV','TA','SA'), xmdl.1(p02, p03), 
		NA
	))
}
