#' @title stand (stem), diameter (breast height)
#' @return stand (stem), diameter (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), height (100 years old)
#' @export
#' @rdname mdl_D_34
mdl_D_34 <- function(p01, p02){

	xmdl.1<-function(p02){
		c03 = 15.25
		c04 = 0.0
		c06 = 0.0
		c01 = 20
		c02 = 0.5
		c05 = 1
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.2<-function(p02){
		c02 = 0.0
		c03 = -29.82
		c04 = 1.568
		c05 = 18.84
		c06 = 0.5
		c01 = 16
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.3<-function(p02){
		c02 = 0.0
		c03 = -3.86
		c04 = 0.386
		c05 = 6.594
		c06 = 0.5
		c01 = 14
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.4<-function(p02){
		c02 = 0.25
		c03 = 18.1
		c04 = 0.0
		c05 = 1
		c06 = 0.0
		c01 = 22
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.5<-function(p02){
		c02 = 0.33
		c03 = 15.5
		c04 = 0.0
		c05 = 1
		c06 = 0.0
		c01 = 18
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.6<-function(p02){
		c06 = 0.0
		c02 = 0.25
		c03 = 7.125
		c01 = 12
		c04 = 0.0
		c05 = 1
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	xmdl.7<-function(p02){
		c02 = 0.25
		c03 = 23.625
		c04 = 0.0
		c05 = 1
		c06 = 0.0
		c01 = 25
		max(c01,c05 * p02 * c02+c05 * p02^c06+c03-p02 * c04)
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02), 
		NA
	))))))))
}
