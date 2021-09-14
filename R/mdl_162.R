#' @title single tree (commercial wood), form 
#' @return single tree (commercial wood), form 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
mdl_F_162 <- function(p01, p02, p03){

	xmdl.2<-function(p02, p03){
		c02 = -27.56211
		c03 = 1.36195
		c04 = 0.057654
		c01 = 0.04016
		c01+c02 / (p02 * p02)+c03 / log(p02)+c04 * p03 / p02
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		NA
	))
}
