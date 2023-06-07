#' @title stand (stem), distance between ()
#' @return stand (stem), distance between ()
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @param p03 stand (stem), diameter (breast height)
#' @export
#' @rdname mdl_L_173
mdl_L_173 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.00928
		c03 = 0.195
		c04 = 0.0022
		c01 = -0.033
		c01+c02 * p02+(c03-c04 * p02) * p03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		NA
	))
}
