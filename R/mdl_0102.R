#' @title single tree (stem), diameter (breast height)
#' @return single tree (stem), diameter (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @param p04 stand (stem), diameter (breast height)
#' @param p05 stand (stem), height 
#' @param p06 single tree (stem), age 
#' @param p07 stand (stem), age 
#' @param p08 stand (stem), basal area (breast height)
#' @export
#' @rdname mdl_D_102
mdl_D_102 <- function(p01, p02, p03, p04, p05, p06, p07, p08){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = 0.6675
		c03 = 0.4758
		c04 = 0.1773
		c05 = 0.9442
		c06 = 0.3631
		c07 = 0.7762
		c01 = 5.4625
		p04+(p04-p02 * (1+exp(c01-c02 * log(p07)-c03 * log(p08)+c04 * log(p04)-c05 * log(p05)-c06 * log(p02)+c07 * log(p03)) / 100)) / 5 * (p06-p07)
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08){
		c02 = 0.8808
		c03 = 0.4982
		c04 = 0.4159
		c05 = 0.3865
		c06 = 0.6267
		c07 = 0.1287
		c01 = 6.9342
		p04+(p04-p02 * (1+exp(c01-c02 * log(p07)-c03 * log(p08)+c04 * log(p04)-c05 * log(p05)-c06 * log(p02)+c07 * log(p03)) / 100)) / 5 * (p06-p07)
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07, p08 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07, p08), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07, p08), 
		NA
	)))
}
