NPV<-function(paym,pdates,IRR){
    ptimes<-as.Date(pdates)-min(as.Date(pdates))
    ptimes<-as.numeric(ptimes,units="days")/365.25
    NPV<-sum(paym*(1+IRR)^{-ptimes})
    NPV
}

Calc_IRR<-function(cf.amts,cf.dts,init.guess=0.1){
    nlm(function(p){NPV(cf.amts,cf.dts,p)^2},p=init.guess)
}

IRR<-function(total_pymnt,total_rec_late_fee,collection_recovery_fee,installment,funded_amnt,issue_d){
#     if (!is.numeric(total_pymnt) |
#         !is.numeric(total_rec_late_fee) |
#         !is.numeric(collection_recovery_fee) |
#         !is.numeric(installment) |
#         !is.numeric(funded_amnt)
#         ) {return(NA)}
    temp.total_collected <- total_pymnt - total_rec_late_fee - collection_recovery_fee
    temp.num_pymnts <- temp.total_collected / installment
    if (round(temp.num_pymnts)==temp.num_pymnts){
        cf.amts<-c(funded_amnt,rep(-installment,round(temp.num_pymnts)))
        cf.dts<-seq.Date(as.Date.yearmon(issue_d),by="month",length.out=round(temp.num_pymnts)+1)
    } else {
        cf.amts<-c(funded_amnt,rep(-installment,round(temp.num_pymnts)),-(temp.total_collected-round(temp.num_pymnts)*installment))
        cf.dts<-seq.Date(as.Date.yearmon(issue_d),by="month",length.out=round(temp.num_pymnts)+2)
    }
    if(NPV(cf.amts,cf.dts,-.999)>0){
        return(-.9999)
    } else {
        Calc_IRR(cf.amts,cf.dts)$estimate    
    }
}
#pv<-10000
#pv.date<-as.Date("2010-06-30")
#cf.amts<-c(pv,rep(-331.47,36))
#cf.dts<-seq.Date(pv.date,by="month",length.out=37)
#x<-Calc_IRR(cf.amts,cf.dts)
