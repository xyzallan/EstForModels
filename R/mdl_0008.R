#' @title single tree (bark), diameter ()
#' @return single tree (bark), diameter ()
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), height 
#' @param p03 single tree (stem), height (random point)
mdl_D_8 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c02 = 1.1012
		c03 = -1.163
		c04 = 0.5516
		c05 = 0.8710
		c01 = -0.4096
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.2<-function(p02, p03){
		c02 = 0.1310
		c03 = -0.2037
		c04 = 0.0757
		c05 = 0.9465
		c01 = -0.0545
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.3<-function(p02, p03){
		c02 = 1.3422
		c03 = -1.175
		c05 = 0.9039
		c04 = 0.3992
		c01 = -0.5573
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.4<-function(p02, p03){
		c02 = 0.3707
		c03 = -0.4335
		c04 = 0.1799
		c05 = 0.9243
		c01 = -0.1424
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.5<-function(p02, p03){
		c02 = 0.6245
		c03 = -0.2045
		c04 = -0.0093
		c05 = 0.9166
		c01 = -0.5500
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.6<-function(p02, p03){
		c02 = 0.2197
		c03 = -0.2569
		c04 = 0.1066
		c05 = 0.9498
		c01 = -0.0844
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.7<-function(p02, p03){
		c02 = 1.7896
		c04 = 0.5323
		c05 = 0.8719
		c01 = -0.7431
		c03 = -1.567
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.8<-function(p02, p03){
		c02 = 0.4070
		c03 = -0.4760
		c04 = 0.1975
		c05 = 0.9169
		c01 = -0.1564
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	xmdl.14<-function(p02, p03){
		c03 = -0.1961
		c04 = -0.0089
		c05 = 0.9200
		c01 = -0.5275
		c02 = 0.5989
		ipf1 = p03/p02
		(((c01 * (ipf1)+c02) * (ipf1)+c03) * (ipf1)+c04) * (ipf1)+c05
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		ifelse(p01 %in% c('PN'), xmdl.14(p02, p03), 
		NA
	))))))))))
}
