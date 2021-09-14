#' @title single tree (stem), volume 
#' @return single tree (stem), volume 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
mdl_V_66 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 0.660
		c03 = 2.156
		c04 = -8.312
		c01 = 0.3571
		3.1415926 * p02^2 * p03 * (c01+c02 / p02+c03 / p03+c04 / (p02 * p03)) / 40000
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.181
		c03 = 1.190
		c04 = -1.309
		c01 = 0.4216
		3.1415926 * p02^2 * p03 * (c01+c02 / p02+c03 / p03+c04 / (p02 * p03)) / 40000
	}

	xmdl.3<-function(p02, p03){
		c02 = 0.757
		c03 = 0.801
		c04 = -10.707
		c01 = 0.4080
		3.1415926 * p02^2 * p03 * (c01+c02 / p02+c03 / p03+c04 / (p02 * p03)) / 40000
	}

	xmdl.4<-function(p02, p03){
		c02 = -0.608
		c03 = 0.0
		c04 = 12.724
		c01 = 0.4723
		3.1415926 * p02^2 * p03 * (c01+c02 / p02+c03 / p03+c04 / (p02 * p03)) / 40000
	}

	xmdl.7<-function(p02, p03){
		c02 = 0.0
		c03 = 1.586
		c04 = 1.440
		c01 = 0.4033
		3.1415926 * p02^2 * p03 * (c01+c02 / p02+c03 / p03+c04 / (p02 * p03)) / 40000
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('LH','MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('NU','TS','KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('PN','LV','KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('LM','HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('KP','JA','VA','SA','TA'), xmdl.7(p02, p03), 
		NA
	))))))
}
