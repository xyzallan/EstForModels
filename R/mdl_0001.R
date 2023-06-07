#' @title single tree (stem), height 
#' @return single tree (stem), height 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @export
#' @rdname mdl_H_1
mdl_H_1 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 1.1
		c03 = 9.9241
		c01 = 32.7621
		1.3+c01 * (p02 / (p02+c02))^c03
	}

	xmdl.2<-function(p02){
		c02 = 1.3
		c03 = 10.858
		c01 = 37.2351
		1.3+c01 * (p02 / (p02+c02))^c03
	}

	xmdl.3<-function(p02){
		c02 = 8.0
		c03 = 1.4625
		c01 = 31.9851
		1.3+c01 * (p02 / (p02+c02))^c03
	}

	xmdl.5<-function(p02){
		c02 = 4.3
		c03 = 2.4979
		c01 = 31.6953
		1.3+c01 * (p02 / (p02+c02))^c03
	}

	xmdl.7<-function(p02){
		c02 = 1.6
		c03 = 8.2934
		c01 = 35.8659
		1.3+c01 * (p02 / (p02+c02))^c03
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('SD','LH','MA'), xmdl.1(p02), 
		ifelse(p01 %in% c('NU','TS','KU','TO'), xmdl.2(p02), 
		ifelse(p01 %in% c('TL','PP','HB','KS'), xmdl.3(p02), 
		ifelse(p01 %in% c('PN','RE','LV','LM'), xmdl.5(p02), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.7(p02), 
		NA
	))))))
}
