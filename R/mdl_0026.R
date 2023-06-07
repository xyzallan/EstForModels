#' @title species level (stem), basal area (breast height)
#' @return species level (stem), basal area (breast height)
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height 
#' @export
#' @rdname mdl_G_26
mdl_G_26 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 15.037
		c03 = -0.967
		c01 = -5.011
		c01 * p02^(1 / 3)+c02 * p02^(1 / 2)+c03 * p02
	}

	xmdl.2<-function(p02){
		c02 = 5.384
		c03 = 0.354
		c01 = 0.678
		c01 * p02^(1 / 3)+c02 * p02^(1 / 2)+c03 * p02
	}

	xmdl.3<-function(p02){
		c02 = 32.098
		c03 = -1.234
		c01 = -34.044
		c01 * p02^(1 / 3)+c02 * p02^(1 / 2)+c03 * p02
	}

	xmdl.4<-function(p02){
		c02 = -4.744
		c03 = 1.130
		c01 = 10.643
		c01 * p02^(1 / 3)+c02 * p02^(1 / 2)+c03 * p02
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('NU','KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LV','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LM','HB'), xmdl.4(p02), 
		NA
	)))))
}
