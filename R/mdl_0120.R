#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_G_120
mdl_G_120 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c03 = -0.0286
		c04 = 0.447
		c01 = -6.54
		c02 = 1.72
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	xmdl.2<-function(p02, p03){
		c02 = 2.52
		c03 = -0.0479
		c04 = 0.142
		c01 = -3.86
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
