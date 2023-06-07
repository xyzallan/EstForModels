#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height 
mdl_V_27 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 0.0
		c03 = 0.056150
		c01 = 9.127
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.2<-function(p02){
		c02 = 1.19111
		c03 = -0.054664
		c01 = 5.144
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.3<-function(p02){
		c02 = -0.30645
		c03 = 0.049519
		c01 = 6.355
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.4<-function(p02){
		c02 = 0.0
		c03 = 0.044243
		c01 = 5.739
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.7<-function(p02){
		c02 = 0.0
		c03 = 0.055563
		c01 = 5.421
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('NU','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LV','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('LM','HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.5(p02), 
		NA
	))))))
}
