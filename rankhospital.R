#3 Ranking hospitals by outcome in a state
rankhospital<-function(state, outcome, num){
        #Read data      
               gf_30<-read.csv("outcome-of-care-measures.csv",
                               colClasses="character")
#        gf_h<-read.csv("hospital-data.csv", colClasses = "character")
#Check that the state and outcome are valid
#Check state        
                      st<-gf_30[,7]
               st_ch<-grep(state, st)
               st_chl<-length(st_ch)
                       if (!st_chl>0){
                   print("invalid state")
                        stop("invalid state")
      }
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
        sh2<-sh1[[state]]
options(warn=-1)#switch off warnings(its about NA)
        sh2[,d]<-as.numeric(as.character(sh2[,d]))# convert factor to numeric
options(warn=0)#switch on warnings
        wmi<-which(sh2[,d]==min(sh2[,d],na.rm=TRUE))
                 sh3<-sh2[wmi,2]
                 sh4<-sort(sh3)
                 best<-sh4[1]
print(best)
	  wma<-which(sh2[,d]==max(sh2[,d],na.rm=TRUE))
                 sh3ma<-sh2[wma,2]
                 sh4ma<-sort(sh3ma)
                 worst<-sh4ma[1]
print(worst)
		sh_r<-sh2[,c(2,d)]
		sh_r$Rank<-NA
		names(sh_r)<-c(names(sh_r[1]),"Rate",names(sh_r[3]))

print(state)
print(outcome)
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
rez<-best }else{
if(num==worst2){
rez<-worst
}
else{
rez<-sh_rco[num,1]}}
rez	
        }