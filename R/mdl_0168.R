#' @title stand (stem), number of trees ()
#' @return stand (stem), number of trees ()
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), number of trees 
#' @param p03 stand (stem), height 
#' @param p04 stand (stem), height 
#' @export
#' @rdname mdl_N_168
mdl_N_168 <- function(p01, p02, p03, p04){

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.000264
		c03 = 2.341983
		c01 = -0.618365
		1000 * ((p02 / 1000)^c01+c02 * (p03^c03-p04^c03))^(1 / c01)
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
	))
}
