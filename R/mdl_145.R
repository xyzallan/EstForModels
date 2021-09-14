#' @title Stem G
#' @return Stem G
#' @param p01 Stem Spec
#' @param p02 Stem A
#' @param p03 Stem H
mdl_G_145 <- function(p01, p02, p03){
	xmdl.1<-function(p02, p03){
		c02 =  -0.0008
		c03 =  -0.5845
		c04 =   0.0692
		c05 =   5.2681
		c01 = 0.6112
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.2<-function(p02, p03){
		c02 =   0.0026
		c03 =  -0.8734
		c04 =   0.0625
		c05 =   4.3380
		c01 =  0.5434
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.3<-function(p02, p03){
		c02 =   0.0055
		c03 = -0.2490
		c04 = 0.1444
		c05 = 5.9079
		c01 =   0.2707
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.4<-function(p02, p03){
		c04 = 0.1133
		c05 = 5.1115
		c01 = 0.3100
		c02 = 0.0056
		c03 = -0.3994
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.5<-function(p02, p03){
		c02 = 0.0056
		c03 = -0.3994
		c04 = 0.1133
		c05 = 5.1115
		c01 = 0.3100
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.7<-function(p02, p03){
		c02 = 0.0035
		c03 = -0.8436
		c04 = 0.0600
		c05 = 4.8153
		c01 = 0.6403
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}
	xmdl.8<-function(p02, p03){
		c02 = 0.0035
		c03 = -0.8436
		c04 = 0.0600
		c05 = 4.8153
		c01 = 0.6403
		((c01+c02 * p02) * p03+c03 * p02)+(p02 / (c04 * p02+c05))^2
	}

	with(data.frame( p01, p02, p03 ),
		ifelse(p01 %in% c('MA'), xmdl.1(p02, p03), 
		ifelse(p01 %in% c('KU'), xmdl.2(p02, p03), 
		ifelse(p01 %in% c('KS'), xmdl.3(p02, p03), 
		ifelse(p01 %in% c('HB'), xmdl.4(p02, p03), 
		ifelse(p01 %in% c('LM'), xmdl.5(p02, p03), 
		ifelse(p01 %in% c('TA'), xmdl.7(p02, p03), 
		ifelse(p01 %in% c('SA'), xmdl.8(p02, p03), 
		NA
		))))))))
}
