#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @param p03 stand (stem), age 
#' @export
#' @rdname mdl_V_33
mdl_V_33 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 2.806
		c03 = 4.74
		c01 = 6.631
		c01+(c02 * p02 / sqrt(p03)+c03) * p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
	))
}
