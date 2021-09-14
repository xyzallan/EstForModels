#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), height (100 years old)
mdl_G_117 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 2.12
		c03 = -0.0445
		c04 = 0.468
		c01 = -13.8
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	xmdl.2<-function(p02, p03){
		c02 = 2.42
		c03 = -0.0473
		c04 = 0.292
		c01 = -12.9
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	xmdl.3<-function(p02, p03){
		c02 = 1.75
		c03 = -0.028
		c04 = 0.0
		c01 = -7.94
		c01+p02 * (c02+p02 * c03)+p03 * c04
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
