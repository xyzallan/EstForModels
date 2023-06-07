#' @title single tree (stem), height 
#' @return single tree (stem), height 
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 species level (stem), diameter (breast height)
#' @param p04 species level (stem), height 
mdl_H_82 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c02 = 0.10978
		c03 = 0.43553
		c04 = 1.31378
		c01 = 0.740140
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = 0.419300
		c03 = 0.235770
		c04 = 1.46976
		c01 = 1.215400
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.3<-function(p02, p03, p04){
		c01 = 0.665485
		c02 = 0.070029
		c03 = 0.550304
		c04 = 1.384610
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.4<-function(p02, p03, p04){
		c02 = 0.055123
		c03 = 0.502932
		c04 = 1.204299
		c01 = 0.542643
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.5<-function(p02, p03, p04){
		c02 = 0.000344
		c03 = 1.704617
		c04 = 1.524991
		c01 = 0.337397
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.6<-function(p02, p03, p04){
		c02 = 0.031420
		c03 = 0.759090
		c04 = 1.146170
		c01 = 0.576900
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.7<-function(p02, p03, p04){
		c02 = -0.88247
		c03 = -0.33982
		c04 = 1.327750
		c01 = -0.07923
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	xmdl.8<-function(p02, p03, p04){
		c02 = 0.003984
		c03 = 0.992932
		c04 = 1.186505
		c01 = 0.372699
		1.3+(p04-1.3) / (1-(c01-c02 * p03^c03) * (1-(p03 / p02)^c04))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04), 
		NA
	)))))))))
}
