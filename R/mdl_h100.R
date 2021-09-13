#' @title Site index model
#' @author Allan Sims
#' @param Species tree species
#' @param Stand_Age main species age
#' @param Stand_Height main species average height
#' @param Base_Age Age of site index height

mdl_h100 <- function(Species, Stand_Age, Stand_Height, Base_Age)
{
        grp.1 <- c("MA"  ,  "TA"  ,  "SA"  ,  "VA"  ,  "JA"  ,  "KP")
        H.mdl.180.1<-function(p02, p03, p04){
         ipf1 = p03*(1+(0.7283)*((50/p02)^(1.3925)-1))/(1-(-0.0109)*p03*((50/p02)^(1.3925)-1))
         return(ipf1 / (1+((0.7283)+(-0.0109) * ipf1) * ((50 / p04)^(1.3925)-1)))
        }
        
        grp.2 <- c("KU"  ,  "TS"  ,  "NU"  ,  "TO"  ,  "LH")
        H.mdl.180.2<-function(p02, p03, p04){
         ipf1 = p03*(1+(0.7977)*((50/p02)^(1.6116)-1))/(1-(-0.0137)*p03*((50/p02)^(1.6116)-1))
         return(ipf1 / (1+((0.7977)+(-0.0137) * ipf1) * ((50 / p04)^(1.6116)-1)))
        }
        
        # "KS"  ,  "HB"  ,  "LM"  ,  "LV"  ,  "RE"  ,  "PP"  ,  "PN"  ,  "TL" 
        H.mdl.180.3<-function(p02, p03, p04){
         ipf1 = p03*(1+(0.7298)*((50/p02)^(1.346)-1))/(1-(-0.0161)*p03*((50/p02)^(1.346)-1))
         return(ipf1 / (1+((0.7298)+(-0.0161) * ipf1) * ((50 / p04)^(1.346)-1)))
        }
        
        with(data.frame(xPl = Species, Ax = Stand_Age, Hx = Stand_Height, A100 = Base_Age),
        ifelse(xPl %in% grp.1,
                H.mdl.180.1(Ax, Hx, A100),
        ifelse(xPl %in% grp.2,
                H.mdl.180.2(Ax, Hx, A100),
                H.mdl.180.3(Ax, Hx, A100)
        )
        ))


}
