#' @title stand (stem), basal area (breast height)
#' @return stand (stem), basal area (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age (breast height)
#' @param p03 stand (stem), basal area (breast height)
mdl_G_93 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 88.88
		c03 = -1
		c04 = 0.9961
		c05 = 4.232
		c01 = 0.6192
		c01+c02 * p02^c03+(c04+c05 / p02) * p03
	}

	xmdl.2<-function(p02, p03){
		c02 = 200.02
		c03 = -1
		c04 = 0.998
		c05 = 4.84
		c01 = -0.873
		c01+c02 * p02^c03+(c04+c05 / p02) * p03
	}

	xmdl.3<-function(p02, p03){
		c03 = 1
		c04 = 0.98
		c05 = 8.6
		c01 = 1.1
		c02 = -0.013
		c01+c02 * p02^c03+(c04+c05 / p02) * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
