#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height 
#' @param p03 species level (stem), basal area (breast height)
#' @export
#' @rdname mdl_V_21
mdl_V_21 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 13.45585
		c03 = 3.36416
		c01 = 0.63206
		p03 * p02 * (c01+c02 / p02^c03)
	}

	xmdl.2<-function(p02, p03){
		c02 = 55.14159
		c03 = 4.74566
		c01 = 0.68190
		p03 * p02 * (c01+c02 / p02^c03)
	}

	xmdl.3<-function(p02, p03){
		c02 = 46.78153
		c03 = 4.19317
		c01 = 0.59224
		p03 * p02 * (c01+c02 / p02^c03)
	}

	xmdl.4<-function(p02, p03){
		c02 = 24.54082
		c03 = 4.09277
		c01 = 0.62368
		p03 * p02 * (c01+c02 / p02^c03)
	}

	xmdl.5<-function(p02, p03){
		c02 = 40.84489
		c03 = 4.56127
		c01 = 0.64881
		p03 * p02 * (c01+c02 / p02^c03)
	}

	xmdl.6<-function(p02, p03){
		c02 = 28.11858
		c03 = 3.78322
		c01 = 0.59641
		p03 * p02 * (c01+c02 / p02^c03)
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		NA
	)))))))
}
