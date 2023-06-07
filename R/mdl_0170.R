#' @title stand (stem), height 
#' @return stand (stem), height 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (dominant)
#' @export
#' @rdname mdl_H_170
mdl_H_170 <- function(p01, p02){

	xmdl.2<-function(p02){
		c02 = 1.1206
		c01 = 0.5981
		c01 * p02^c02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		NA
	))
}
