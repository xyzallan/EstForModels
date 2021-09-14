#' @title single tree (stem), volume 
#' @return single tree (stem), volume 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height ()
mdl_V_175 <- function(p01, p02, p03){

	xmdl.5<-function(p02, p03){
		c02 = 1.9291
		c03 = 1.004
		c01 = 0.000049
		c01 * p02^c02 * p03^c03
	}

	xmdl.6<-function(p02, p03){
		c02 = 1.5771
		c03 = 1
		c01 = 0.000128
		c01 * p02^c02 * p03^c03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		NA
	)))
}
