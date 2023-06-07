#' @title species level (stem), volume ()
#' @return species level (stem), volume ()
#' @param p01 species level (stem), species ()
#' @param p02 species level (stem), height ()
mdl_V_181 <- function(p01, p02){

	xmdl.1<-function(p02){
		c02 = 17.9947
		c03 = -0.0108
		c04 = -0.0003
		c01 = -23.7
		c01+p02 * (c02+p02 * (c03+p02 * c04))
	}

	xmdl.2<-function(p02){
		c02 = 13.4278
		c03 = 0.1440
		c04 = 0.0029
		c01 = -17.3
		c01+p02 * (c02+p02 * (c03+p02 * c04))
	}

	xmdl.3<-function(p02){
		c02 = -4.7970
		c03 = 0.9508
		c04 = -0.0116
		c01 = 50.2
		c01+p02 * (c02+p02 * (c03+p02 * c04))
	}

	xmdl.4<-function(p02){
		c02 = -0.8948
		c03 = 0.8134
		c04 = -0.0078
		c01 = 33.5
		c01+p02 * (c02+p02 * (c03+p02 * c04))
	}

	xmdl.7<-function(p02){
		c02 = 6.3291
		c03 = 0.4181
		c04 = -0.0026
		c01 = -3.8
		c01+p02 * (c02+p02 * (c03+p02 * c04))
	}

	with(data.frame( p01, p02 ),
		ifelse(p01 %in% c('MA','LH','SD'), xmdl.1(p02), 
		ifelse(p01 %in% c('KU','TO','TS'), xmdl.2(p02), 
		ifelse(p01 %in% c('KS','PN'), xmdl.3(p02), 
		ifelse(p01 %in% c('HB','LM','LV'), xmdl.4(p02), 
		ifelse(p01 %in% c('TA','SA','VA','JA','KP','TL'), xmdl.7(p02), 
		NA
	))))))
}
