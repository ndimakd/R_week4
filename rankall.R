#4 Ranking hospital by outcome in all states
rankall<-function(outcome, num) {
#Make table for result
	rez<-data.frame(0,0)
	colnames(rez)<-c("hospital","state") 
#Read data      
               gf_30<-read.csv("outcome-of-care-measures.csv",
                               colClasses="character")
#        gf_h<-read.csv("hospital-data.csv", colClasses = "character")
#Check that the state and outcome are valid
#Check outcome
               out<-c("heart attack","heart failure","pneumonia")      
               out_ch<-grep(outcome,out)
               out_chl<-length(out_ch)
                       if(!out_chl>0){
                        print("invalid outcome")
                      stop("incvalid outcome!!!")
                    }
#Finding data in state         
        if(outcome==out[1])
        d<-11
        if(outcome==out[2])
        d<-17        
        if(outcome==out[3])
        d<-23
        sh1<-split(gf_30,gf_30[,7])
	
for(i in 1:length(sh1)){        
	sh2<-sh1[[i]]
options(warn=-1)#switch off warnings(its about NA)
        sh2[,d]<-as.numeric(as.character(sh2[,d]))# convert factor to numeric
options(warn=0)#switch on warnings
#Find best
        wmi<-which(sh2[,d]==min(sh2[,d],na.rm=TRUE))
                 sh3<-sh2[wmi,2]
                 sh4<-sort(sh3)
                 best<-sh4[1]
#Find worst
	  wma<-which(sh2[,d]==max(sh2[,d],na.rm=TRUE))
                 sh3ma<-sh2[wma,2]
                 sh4ma<-sort(sh3ma)
                 worst<-sh4ma[1]
#Ranking data
		sh_r<-sh2[,c(2,7,d)]
		sh_r$Rank<-NA
		names(sh_r)<-c(names(sh_r[1]),names(sh_r[2]),"Rate",names(sh_r[4]))
#Clean data from NA
     		sh_rc1<-complete.cases(sh_r[,"Rate"])
		sh_rc<-sh_r[sh_rc1,]
#Make order by Name
		sh_rco1<-order(sh_rc[,"Hospital.Name"])
		sh_rco<-sh_rc[sh_rco1,]
#Make order by Rate
		sh_rco2<-order(sh_rco[,"Rate"])
		sh_rco<-sh_rco[sh_rco2,]
#Make rank
		sh_rr<-as.integer(rank(sh_rco[,"Rate"]))
		sh_rco$Rank<-sh_rr
#sh_rco
	best2<-c("best")
	worst2<-c("worst")
	if(num==best2){
	rez1<-c(best,sh2[,7])
	rez<-rbind(rez,rez1)	 
}else{
	if(num==worst2){
	rez1<-c(worst,sh2[,7])
	rez<-rbind(rez,rez1)
}
	else{
	rez1<-c(sh_rco[num,1],sh2[1,7])
	rez<-rbind(rez,rez1)
	       }
 }
 }
        rez<-rez[complete.cases(rez[,"state"]),]
	  rez<-rez[-c(1),]
	 rownames(rez)<-rez[,"state"]
	rez	
}