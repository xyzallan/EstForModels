#' @title stand (stem), diameter (breast height)
#' @return stand (stem), diameter (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @param p03 stand (stem), age 
#' @param p04 stand (stem), age (random point)
mdl_D_183 <- function(p01, p02, p03, p04){

	xmdl.15<-function(p02, p03, p04){
		c02 = 1.00
		c01 = 811
		ipf1 = c01*10^(-c02)
		ipf2 = sqrt((p02-ipf1)^2+4*c01*p02*p03^(-c02))
		(p02+ipf1+ipf2) / (2+4 * c01 * (p04^(-c02)) / (p02-ipf1+ipf2))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('LH'), xmdl.15(p02, p03, p04), 
		NA
	))
}
