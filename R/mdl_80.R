#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), age (breast height)
#' @param p03 stand (stem), age (breast height)
#' @param p04 stand (stem), height 
mdl_H_80 <- function(p01, p02, p03, p04){

	xmdl.3<-function(p02, p03, p04){
		c02 = -0.28
		c03 = 0.029
		c04 = 0.201
		c05 = 5.5
		c01 = 2.393
		ipf1 = (p03-50)/10
		ipf2 = (p02-50)/10
		((((p04 - (23 + ipf2 * (c01+ipf2*(c02+c03*ipf2)))) / (1-0.01*ipf2+c04*ipf2 / (ipf2+c05))+23)*10) / 10-23)* (1-0.01*ipf1+c04*ipf1 / (ipf1+c05) )+ (23+ipf1*(c01+ipf1*(c02+c03*ipf1)))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))
}
