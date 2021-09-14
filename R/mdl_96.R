#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age (breast height)
#' @param p03 stand (stem), volume 
mdl_V_96 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.3412
		c03 = -0.004744
		c04 = 0.9724
		c05 = 9.0243
		c06 = 2
		c01 = 35.38
		c01+c02 * p02+c03 * p02^c06+(c04+c05 / p02) * p03
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.0
		c03 = 969.2
		c04 = 0.9205
		c05 = 13.005
		c06 = -1
		c01 = 15.66
		c01+c02 * p02+c03 * p02^c06+(c04+c05 / p02) * p03
	}

	xmdl.3<-function(p02, p03){
		c02 = 0.0
		c03 = -0.21
		c04 = 0.93
		c05 = 16
		c06 = 1
		c01 = 17
		c01+c02 * p02+c03 * p02^c06+(c04+c05 / p02) * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		NA
	))))
}
