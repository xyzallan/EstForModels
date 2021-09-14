#' @title single tree (bark), volume 
#' @return single tree (bark), volume 
#' @param p01 single tree (bark), species 
#' @param p02 single tree (stem), diameter (breast height)
mdl_V_172 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 10
		c03 = -17.5
		c01 = 6.0
		c01 * ((p02+c02) / (p02+c02+1))^c03 / 100
	}

	xmdl.2<-function(p02){
		c02 = 2
		c03 = -4.9
		c01 = 8.0
		c01 * ((p02+c02) / (p02+c02+1))^c03 / 100
	}

	xmdl.3<-function(p02){
		c02 = 3
		c03 = -4.9
		c01 = 11.1
		c01 * ((p02+c02) / (p02+c02+1))^c03 / 100
	}

	xmdl.4<-function(p02){
		c02 = 2
		c03 = -3.2
		c01 = 12.0
		c01 * ((p02+c02) / (p02+c02+1))^c03 / 100
	}

	xmdl.5<-function(p02){
		c02 = 2
		c03 = -4.0
		c01 = 10.8
		c01 * ((p02+c02) / (p02+c02+1))^c03 / 100
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA','SD','TO'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU','LH','NU','TS'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS','PN'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','PP','TA','SA','VA','JA','KP','TL'), xmdl.4(p02), 
		ifelse(p01 %in% c('LM','LV','RE'), xmdl.5(p02), 
		NA
	))))))
}
