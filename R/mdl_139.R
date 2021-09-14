#' @title species level (stem), number of trees 
#' @return species level (stem), number of trees 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 species level (stem), basal area (breast height)
#' @param p04 species level (stem), height (20 years old)
mdl_N_139 <- function(p01, p02, p03, p04){

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.4246
		c03 = 3.26
		c04 = 0.32
		c01 = 0.228
		c01 * (p02 * p03)^c02 * (c03 * p04-1)^c04
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		NA
	))
}
