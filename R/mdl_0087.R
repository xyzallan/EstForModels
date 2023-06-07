#' @title stand (stem), height (dominant)
#' @return stand (stem), height (dominant)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @export
#' @rdname mdl_H_87
mdl_H_87 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 0.9182
		c03 = -1.66
		c01 = 0.002014
		0.5 * (-c02+sqrt(c02^2-4 * c01 * (c03-p02))) / c01
	}

	xmdl.2<-function(p02){
		c02 = 0.724
		c03 = 0.95
		c01 = 0.005038
		0.5 * (-c02+sqrt(c02^2-4 * c01 * (c03-p02))) / c01
	}

	xmdl.3<-function(p02){
		c02 = 0.775
		c03 = 1.015
		c01 = 0.00375
		0.5 * (-c02+sqrt(c02^2-4 * c01 * (c03-p02))) / c01
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		NA
	))))
}
