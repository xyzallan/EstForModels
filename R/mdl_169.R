#' @title stand (stem), formheight 
#' @return stand (stem), formheight 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
mdl_HF_169 <- function(p01, p02){

	xmdl.2<-function(p02){
		c02 = 0.9401
		c03 = -0.01063
		c01 = -4.258
		c01+c02 * (p02+c03 * p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
	))
}
