#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_G_121
mdl_G_121 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c04 = 0.343
		c01 = -4.75
		c02 = 1.31
		c03 = -0.0183
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	xmdl.2<-function(p02, p03){
		c04 = 0.204
		c01 = -2.62
		c02 = 1.77
		c03 = -0.0268
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	)))
}
