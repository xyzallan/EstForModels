#' @title stand (stem), form 
#' @return stand (stem), form 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
#' @export
#' @rdname mdl_F_186
mdl_F_186 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 4.0350
		c03 = -0.1204
		c04 = 0.3704
		c01 = -0.2934
		c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02)
	}

	xmdl.2<-function(p02){
		c02 = 0.4713
		c03 = 0.0992
		c04 = -0.3109
		c01 = 0.9672
		c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02)
	}

	xmdl.3<-function(p02){
		c02 = 7.5641
		c03 = -0.3429
		c04 = 1.1006
		c01 = -1.6715
		c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02)
	}

	xmdl.4<-function(p02){
		c02 = -0.5950
		c03 = 0.0437
		c04 = -0.1969
		c01 = 0.8813
		c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02)
	}

	xmdl.7<-function(p02){
		c02 = 0.9350
		c03 = 0.0286
		c04 = -0.1006
		c01 = 0.5993
		c01+c02 / p02+c03 * sqrt(p02)+c04 * log(p02)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA','LH','SD'), xmdl.1(p02), 
		ifelse(p01 %in% c('TS','KU','TO'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS','PN'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','LV','LM'), xmdl.4(p02), 
		ifelse(p01 %in% c('TA','SA','VA','JA','KP','TL'), xmdl.7(p02), 
		NA
	))))))
}
