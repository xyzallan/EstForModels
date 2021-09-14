#' @title single tree (stem), diameter (random point)
#' @return single tree (stem), diameter (random point)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @param p04 single tree (stem), height (random point)
mdl_D_75 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c03 = -1.6082
		c04 = 2.4886
		c05 = -2.4147
		c06 = 2.3619
		c08 = 1.0817
		c01 = 2.1288
		c02 = -0.63157
		c07 = -1.7539
		ipf1 = 1-p04/p03
		ipf2 = 1-1.3/p03
		p02 * (ipf1 * (c01+ipf1 * (c02+ipf1 * (c03+ipf1^2 * (c04+ipf1^3 * (c05+ipf1^5 * (c06+ipf1^8 * (c07+ipf1^13 * (c08))))))))) / (ipf2 * (c01+ipf2 * (c02+ipf2 * (c03+ipf2^2 * (c04+ipf2^3 * (c05+ipf2^5 * (c06+ipf2^8 * (c07+ipf2^13 * (c08)))))))))
	}

	xmdl.2<-function(p02, p03, p04){
		c02 = -3.2684
		c03 = 3.6513
		c04 = -2.2608
		c06 = 2.1501
		c07 = -2.7412
		c08 = 1.8876
		c05 = 0.0
		c01 = 2.3366
		ipf1 = 1-p04/p03
		ipf2 = 1-1.3/p03
		p02 * (ipf1 * (c01+ipf1 * (c02+ipf1 * (c03+ipf1^2 * (c04+ipf1^3 * (c05+ipf1^5 * (c06+ipf1^8 * (c07+ipf1^13 * (c08))))))))) / (ipf2 * (c01+ipf2 * (c02+ipf2 * (c03+ipf2^2 * (c04+ipf2^3 * (c05+ipf2^5 * (c06+ipf2^8 * (c07+ipf2^13 * (c08)))))))))
	}

	xmdl.3<-function(p02, p03, p04){
		c02 = 4.1060
		c03 = -7.8517
		c04 = 7.8993
		c05 = -7.5018
		c06 = 6.3863
		c07 = -4.3918
		c08 = 2.1604
		c01 = 0.93838
		ipf1 = 1-p04/p03
		ipf2 = 1-1.3/p03
		p02 * (ipf1 * (c01+ipf1 * (c02+ipf1 * (c03+ipf1^2 * (c04+ipf1^3 * (c05+ipf1^5 * (c06+ipf1^8 * (c07+ipf1^13 * (c08))))))))) / (ipf2 * (c01+ipf2 * (c02+ipf2 * (c03+ipf2^2 * (c04+ipf2^3 * (c05+ipf2^5 * (c06+ipf2^8 * (c07+ipf2^13 * (c08)))))))))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		NA
	))))
}
