#' @title single tree (stem), diameter (breast height)
#' @return single tree (stem), diameter (breast height)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_D_171
mdl_D_171 <- function(p01, p02){

	xmdl.8<-function(p02){
		c02 = 1.5263
		c03 = -0.1839
		c01 = -3.6712
		exp(c01+log(p02) * (c02+log(p02) * c03))
	}

	xmdl.21<-function(p02){
		c02 = 0.7075
		c03 = -0.0230
		c01 = -2.9752
		exp(c01+log(p02) * (c02+log(p02) * c03))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('SA'), xmdl.8(p02), 
		ifelse(p01 %in% c('PO'), xmdl.21(p02), 
		NA
	)))
}
