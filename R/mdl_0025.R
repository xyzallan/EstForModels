#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), height 
#' @export
#' @rdname mdl_V_25
mdl_V_25 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 0.26860
		c03 = -0.003798
		c01 = 11.734
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.2<-function(p02){
		c02 = 0.44338
		c03 = -0.001730
		c01 = 7.667
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.3<-function(p02){
		c02 = 0.63313
		c03 = -0.007461
		c01 = 2.478
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.4<-function(p02){
		c02 = 0.49385
		c03 = -0.002431
		c01 = 5.005
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	xmdl.7<-function(p02){
		c02 = 0.41494
		c03 = -0.002266
		c01 = 6.031
		p02 * (c01+p02 * (c02+p02 * c03))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('NU','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('LV','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('RE','LM','HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.5(p02), 
		NA
	))))))
}
