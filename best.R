#2 Finding the best hospital in a state
best<-function(state, outcome){
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
#Finding the best hospital in state         
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
        w<-which(sh2[,d]==min(sh2[,d],na.rm=TRUE))
       #!!!!!!!Проверить m<-sh2[c(w),d]
                 sh3<-sh2[w,2]
                 sh4<-sort(sh3)
                 sh_ok<-sh4[1]
print(state)
print(outcome)
          sh_ok
        }