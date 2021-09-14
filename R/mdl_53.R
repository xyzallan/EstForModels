#' @title stand (stem), volume 
#' @return stand (stem), volume 
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height 
mdl_V_53 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 11.077778
		c03 = 0.255555
		c01 = -11.7778
		(c03 * p02+c02) * p02+c01
	}

	xmdl.2<-function(p02){
		c02 = 24.278565
		c03 = -0.116551
		c01 = -69.8205
		(c03 * p02+c02) * p02+c01
	}

	xmdl.3<-function(p02){
		c02 = 10.46641
		c03 = 0.134102
		c01 = -30.6846
		(c03 * p02+c02) * p02+c01
	}

	xmdl.4<-function(p02){
		c02 = 8.311111
		c03 = 0.315555
		c01 = -15.4444
		(c03 * p02+c02) * p02+c01
	}

	xmdl.5<-function(p02){
		c02 = 10.01371
		c03 = 0.353887
		c01 = -25.304
		(c03 * p02+c02) * p02+c01
	}

	xmdl.6<-function(p02){
		c02 = 11.4
		c03 = 0.265
		c01 = -23.625
		(c03 * p02+c02) * p02+c01
	}

	xmdl.7<-function(p02){
		c02 = 10.196262
		c03 = 0.232291
		c01 = -24.9733
		(c03 * p02+c02) * p02+c01
	}

	xmdl.8<-function(p02){
		c02 = 10.544444
		c03 = 0.122222
		c01 = -25.7778
		(c03 * p02+c02) * p02+c01
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02), 
		NA
	)))))))))
}
