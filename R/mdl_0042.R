#' @title species level (stem), volume 
#' @return species level (stem), volume 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), basal area (breast height)
#' @param p03 stand (stem), basal area (breast height)
#' @param p04 species level (stem), number of trees 
#' @param p05 stand (stem), number of trees 
#' @param p06 stand (area), vegetation layer exists 
#' @param p07 stand (area), thinning exists 
#' @param p08 stand (stem), height (100 years old)
#' @param p09 species level (stem), diameter (breast height)
#' @param p10 stand (area), altitude 
#' @param p11 stand (area), latitude 
#' @param p12 species level (stem), age 
#' @export
#' @rdname mdl_V_42
mdl_V_42 <- function(p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){

	xmdl.1<-function(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){
		c15 = 0.026
		c14 = -2.2
		c13 = -0.075
		c12 = -6.46337
		c11 = 0.00526
		c10 = 0.02291
		c09 = -0.01971
		c08 = 0.03492
		c07 = 1.65136
		c06 = 0.36993
		c05 = -0.1723
		c04 = 1.0197
		c03 = -0.2999
		c02 = 1.21272
		c01 = 0.0
		ipf1 = log(1-exp(c14*p02))
		ipf2 = log(1-exp(c13*p012))
		ipf3 = sqrt((p03-p02)/(p05+0.0001-p04)*4/3.1415926)/p09*(p03-p02)
		exp(p02 * c01+log(p02) * c02+ipf1 * c03+ipf2 * c04+log(p04) * c05+log(p08) * c06+log(p11) * c07+log(p10) * c08+p06 * c09+p07 * c10+ipf3 * c11+c15+c12)
	}

	xmdl.2<-function(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){
		c15 = 0.0325
		c14 = -2.05
		c13 = -0.04
		c12 = 0.5846
		c11 = 0.000548
		c10 = 0.01298
		c09 = 0.0
		c08 = 0.0
		c07 = 0.0
		c06 = 0.33764
		c05 = -0.15205
		c04 = 0.48517
		c03 = -0.34982
		c02 = 1.22886
		c01 = 0.0
		ipf1 = log(1-exp(c14*p02))
		ipf2 = log(1-exp(c13*p012))
		ipf3 = sqrt((p03-p02)/(p05+0.0001-p04)*4/3.1415926)/p09*(p03-p02)
		exp(p02 * c01+log(p02) * c02+ipf1 * c03+ipf2 * c04+log(p04) * c05+log(p08) * c06+log(p11) * c07+log(p10) * c08+p06 * c09+p07 * c10+ipf3 * c11+c15+c12)
	}

	xmdl.3<-function(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){
		c15 = 0.0755
		c14 = -2.05
		c13 = -0.035
		c12 = 8.44862
		c11 = 0.004018
		c10 = 0.03012
		c09 = 0.0
		c08 = -0.0404
		c07 = -1.68251
		c06 = 0.20136
		c05 = -0.17604
		c04 = 0.54042
		c03 = -0.45958
		c02 = 1.26244
		c01 = 0.0
		ipf1 = log(1-exp(c14*p02))
		ipf2 = log(1-exp(c13*p012))
		ipf3 = sqrt((p03-p02)/(p05+0.0001-p04)*4/3.1415926)/p09*(p03-p02)
		exp(p02 * c01+log(p02) * c02+ipf1 * c03+ipf2 * c04+log(p04) * c05+log(p08) * c06+log(p11) * c07+log(p10) * c08+p06 * c09+p07 * c10+ipf3 * c11+c15+c12)
	}

	xmdl.7<-function(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){
		c15 = 0.0756
		c14 = -2.3
		c13 = -0.055
		c12 = 1.40608
		c11 = 0.00188
		c10 = 0.05032
		c09 = 0.0
		c08 = 0.0
		c07 = 0.0
		c06 = 0.15903
		c05 = -0.15708
		c04 = 0.80158
		c03 = -0.46379
		c02 = 1.27353
		c01 = -0.01063
		ipf1 = log(1-exp(c14*p02))
		ipf2 = log(1-exp(c13*p012))
		ipf3 = sqrt((p03-p02)/(p05+0.0001-p04)*4/3.1415926)/p09*(p03-p02)
		exp(p02 * c01+log(p02) * c02+ipf1 * c03+ipf2 * c04+log(p04) * c05+log(p08) * c06+log(p11) * c07+log(p10) * c08+p06 * c09+p07 * c10+ipf3 * c11+c15+c12)
	}

	xmdl.19<-function(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12){
		c15 = 0.0853
		c14 = -2.3
		c13 = -0.04
		c12 = 7.79034
		c11 = 0.004171
		c10 = 0.05474
		c09 = 0.0
		c08 = -0.03683
		c07 = -1.51968
		c06 = 0.17493
		c05 = -0.17205
		c04 = 0.48631
		c03 = -0.58003
		c02 = 1.26649
		c01 = 0.0
		ipf1 = log(1-exp(c14*p02))
		ipf2 = log(1-exp(c13*p012))
		ipf3 = sqrt((p03-p02)/(p05+0.0001-p04)*4/3.1415926)/p09*(p03-p02)
		exp(p02 * c01+log(p02) * c02+ipf1 * c03+ipf2 * c04+log(p04) * c05+log(p08) * c06+log(p11) * c07+log(p10) * c08+p06 * c09+p07 * c10+ipf3 * c11+c15+c12)
	}

	with(data.frame( p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12), 
		ifelse(p01 %in% c('TL'), xmdl.19(p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, p12), 
		NA
	))))))
}
