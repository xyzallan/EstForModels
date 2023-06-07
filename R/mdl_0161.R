#' @title single tree (commercial wood), form 
#' @return single tree (commercial wood), form 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @export
#' @rdname mdl_F_161
mdl_F_161 <- function(p01, p02, p03){

	xmdl.16<-function(p02, p03){
		c02 = 0.8734
		c03 = 0.0052
		c04 = 7.3594
		c05 = 0.46155
		c01 = -200.31914
		c01 / (p02 * p02 * p02)+c02 / p02+c03 * log(p02 * p02)+c04 / (p03 * p02)+c05
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('TS'), xmdl.16(p02, p03), 
		NA
	))
}
