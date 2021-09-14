#' @title single tree (stem), diameter (random point)
#' @return single tree (stem), diameter (random point)
#' @param p01 single tree (stem), species 
#' @param p02 single tree (stem), diameter (breast height)
#' @param p03 single tree (stem), height 
#' @param p04 single tree (stem), height (random point)
mdl_D_2 <- function(p01, p02, p03, p04){

	xmdl.1<-function(p02, p03, p04){
		c11 = 30
		c10 = -0.0070
		c02 = -3361.780
		c03 = 4419.682
		c04 = -3037.487
		c05 = 1140.525
		c06 = -277.578
		c07 = 118.981
		c08 = 0.0070
		c09 = 26
		c01 =  997.657
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.2<-function(p02, p03, p04){
		c11 = 26
		c10 = -0.0197
		c02 = -1699.667
		c03 = 2732.076
		c04 = -2161.251
		c05 =  827.209
		c06 = -203.061
		c07 = 113.939
		c08 = 0.0087
		c09 = 35
		c01 =  390.755
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.3<-function(p02, p03, p04){
		c11 = 28
		c10 =  0.0000
		c02 = -3788.858
		c03 = 5197.005
		c04 = -3725.819
		c05 = 1388.288
		c06 = -312.074
		c07 = 120.567
		c08 = 0.0210
		c09 = 20
		c01 = 1120.891
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.4<-function(p02, p03, p04){
		c11 = 20
		c10 =  0.0002
		c02 = -2212.940
		c03 = 2606.605
		c04 = -1643.304
		c05 =  530.481
		c06 = -143.288
		c07 = 110.428
		c08 = 0.0074
		c09 = 18
		c01 =  752.018
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.5<-function(p02, p03, p04){
		c11 = 12
		c10 = -0.0017
		c02 = -5408.312
		c03 = 6644.011
		c04 = -4238.703
		c05 = 1450.125
		c06 = -310.985
		c07 = 120.224
		c08 = 0.0264
		c09 = 14
		c01 = 1743.640
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.6<-function(p02, p03, p04){
		c11 = 16
		c10 = -0.0103
		c02 = -2137.684
		c03 = 3045.214
		c04 = -2376.874
		c05 = 988.135
		c06 = -263.482
		c07 = 118.560
		c08 = 0.0168
		c09 = 16
		c01 = 626.131
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.7<-function(p02, p03, p04){
		c11 = 20
		c10 = 0.0005
		c02 = -9254.632
		c03 = 11231.250
		c04 = -6736.346
		c05 = 2022.206
		c06 = -354.769
		c07 = 120.958
		c08 = 0.0263
		c09 = 14
		c01 = 2971.333
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	xmdl.8<-function(p02, p03, p04){
		c11 = 21
		c10 = -0.0021
		c02 = -7175.007
		c03 = 7964.660
		c04 = -4542.395
		c05 = 1411.064
		c06 = -282.941
		c07 = 117.999
		c08 = 0.0000
		c09 = 20
		c01 = 2506.62
		ipf1 = p04/p03
		ipf2 = 1.3/p03
		p02 * ((((((c01 * ipf1+c02) * ipf1+c03) * ipf1+c04) * ipf1+c05) * ipf1+c06) * ipf1+c07) * (1+(ipf1^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))) / (((((((c01 * ipf2+c02) * ipf2+c03) * ipf2+c04) * ipf2+c05) * ipf2+c06) * ipf2+c07) * (1+(ipf2^2-0.01) * (c08 * (p03-c09)+c10 * (p02-c11))))
	}

	with(data.frame( p01, p02, p03, p04 ),
		ifelse(p01 %in% c('TO','SD','LH','MA'), xmdl.1(p02, p03, p04), 
		ifelse(p01 %in% c('NU','TS','KU'), xmdl.2(p02, p03, p04), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03, p04), 
		ifelse(p01 %in% c('TL','PP','RE','HB'), xmdl.4(p02, p03, p04), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03, p04), 
		ifelse(p01 %in% c('LV'), xmdl.6(p02, p03, p04), 
		ifelse(p01 %in% c('KP','JA','VA','TA'), xmdl.7(p02, p03, p04), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03, p04), 
		ifelse(p01 %in% c('PN'), xmdl.14(p02, p03, p04), 
		NA
	))))))))))
}
