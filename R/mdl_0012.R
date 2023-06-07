#' @title species level (stem), price 
#' @return species level (stem), price 
#' @param p01 species level (stem), species 
#' @param p02 species level (stem), diameter (breast height)
#' @param p03 species level (stem), height 
#' @export
#' @rdname mdl_Price_12
mdl_Price_12 <- function(p01, p02, p03){

	xmdl.1<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 1
		c11 = 0.005833
		c10 = 36
		c02 = 0.134295
		c03 = 0.246396
		c04 = 30.303
		c05 = 168.0912
		c06 = -241.133
		c07 = 1
		c08 = 1
		c09 = 1
		c01 = 0.002464
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.2<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 1
		c11 = 1
		c10 = 0.0
		c02 = 0.126974
		c03 = 0.325657
		c04 = 33.727
		c05 = 182.5350
		c06 = -260.599
		c07 = 0.0
		c08 = 32
		c09 = 0.25
		c01 = 0.002847
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.3<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 1
		c11 = 0.010833
		c10 = 24
		c03 = 0.185838
		c04 = 15.017
		c05 = 72.26222
		c06 = -57.8556
		c07 = 1
		c08 = 1
		c09 = 1
		c02 = 0.036905
		c01 = 0.003486
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.4<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 1
		c11 = 1
		c10 = 0.0
		c03 = 0.333748
		c04 = 23.950
		c05 = 33.72777
		c06 = 229.0458
		c07 = 0.0
		c08 = 28
		c09 = 0.25
		c01 = 0.003564
		c02 = 0.272266
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.5<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 0.84
		c11 = 0.010833
		c10 = 24
		c03 = 0.185838
		c04 = 15.017
		c05 = 72.26222
		c06 = -57.8556
		c07 = 1
		c08 = 1
		c09 = 1
		c01 = 0.003486
		c02 = 0.036905
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.6<-function(p02, p03){
		c14 = 0.0
		c13 = 0.0
		c12 = 1
		c11 = 0.03
		c10 = 18
		c02 = 0.794369
		c03 = 0.897887
		c04 = 30.874
		c05 = 0.0
		c06 = 346.4830
		c07 = 1
		c08 = 1
		c09 = 1
		c01 = -0.017812
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.7<-function(p02, p03){
		c14 = 0.04375
		c13 = 20
		c12 = 1
		c11 = 0.005833
		c10 = 36
		c02 = 0.134295
		c03 = 0.246396
		c04 = 30.303
		c05 = 168.0912
		c06 = -241.133
		c07 = 1
		c08 = 1
		c09 = 1
		c01 = 0.002464
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
	}

	xmdl.8<-function(p02, p03){
		c14 = 0.035
		c13 = 16
		c12 = 1
		c11 = 0.005833
		c10 = 36
		c02 = 0.134295
		c03 = 0.246396
		c04 = 30.303
		c05 = 168.0912
		c06 = -241.133
		c07 = 1
		c08 = 1
		c09 = 1
		c01 = 0.002464
		ipf1 = (c07+((p02/c08)^c09-c10)*c11)*(c12+(p02-c13)*c14)
		ipf1 * p02^2 * p03 / (((c01 * p02+c02) * p03+c03 * p02-c04) * p02+c05 * sqrt(p02)+c06)
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
		NA
	)))))))))
}
