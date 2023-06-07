#' @title stand (stem), diameter (breast height)
#' @return stand (stem), diameter (breast height)
#' @param p01 stand (stem), species 
#' @param p02 stand (stem), diameter (breast height)
#' @param p03 stand (stem), diameter (100 years old)
#' @param p04 stand (stem), age 
#' @param p05 stand (stem), age (random point)
#' @export
#' @rdname mdl_D_46
mdl_D_46 <- function(p01, p02, p03, p04, p05){

	xmdl.1<-function(p02, p03, p04, p05){
		c02 = 100
		c01 = 3.1
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.2<-function(p02, p03, p04, p05){
		c02 = 100
		c01 = 3.1
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.3<-function(p02, p03, p04, p05){
		c02 = 70
		c01 = 2.1
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.4<-function(p02, p03, p04, p05){
		c02 = 60
		c01 = 2.1
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.5<-function(p02, p03, p04, p05){
		c02 = 70
		c01 = 1.7
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.6<-function(p02, p03, p04, p05){
		c02 = 40
		c01 = 3.7
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.7<-function(p02, p03, p04, p05){
		c02 = 100
		c01 = 2.9
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	xmdl.8<-function(p02, p03, p04, p05){
		c02 = 100
		c01 = 1.0
		p02 * (p03-c01 / 100 * (p04-c02)) / (p03-c01 / 100 * (p05-c02))
	}

	with(data.frame( p01, p02, p03, p04, p05 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04, p05), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04, p05), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04, p05), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04, p05), 
		NA
	)))))))))
}
