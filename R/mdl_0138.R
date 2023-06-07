#' @title species level (stem), height 
#' @return species level (stem), height 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), age 
#' @param p03 species level (stem), height (20 years old)
#' @export
#' @rdname mdl_H_138
mdl_H_138 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = 0.4245
		c03 = 5.066
		c04 = 3.777
		c05 = 0.692
		c01 = 0.19
		c01+c02 * (p02 / 10) * p03-c03 * (p02 / 10)+c04 * (p02 / 10)^2-c05 * (p02 / 10)^3
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
