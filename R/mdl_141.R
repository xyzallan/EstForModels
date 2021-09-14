#' @title species level (stem), basal area (breast height)
#' @return species level (stem), basal area (breast height)
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 species level (stem), height (20 years old)
mdl_G_141 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 0.1791
		c03 = 6.37
		c01 = 0.0942
		c01 * p02 * p03+c02 * p02-c03
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
