#' @title species level (stem), basal area (breast height)
#' @return species level (stem), basal area (breast height)
#' @param p01 species level (stem), species ()
#' @param p02 species level (stem), diameter (breast height)
#' @param p03 stand (stem), height (100 years old)
#' @param p04 species level (stem), basal area (breast height)
#' @export
#' @rdname mdl_G_191
mdl_G_191 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.1712
		c03 = 0.1039
		c04 = -0.0506
		c01 = 12.0422
		c01 * exp(-c02 * p02)+c03 * p03+c04 * p04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		NA
	))
}
