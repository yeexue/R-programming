setwd("c:/R/data")
hospital_data<-read.csv("hospital-data.csv")
outcome<-read.csv("outcome-of-care-measures.csv")
outcomedata <-read.csv("outcome-of-care-measures.csv", colClasses = "character")


## 1. Plot the 30-day mortality rates for heart attack

outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])

## 2. Finding the best hospital in a state

best <-function(S, O){
        ## Read outcome data
        setwd("c:/R/")
        outcome_data<- read.csv("outcome-of-care-measures.csv")
        state<-S
        outcome<-O

        ## Check that state and outcome are valid
        if(outcome=="heart attack"){
                x<-11
        }else if(outcome=="heart failure"){
                x<-17
        }else if(outcome=="pneumonia"){
                x<-23
        }else{print("invalid outcome")}
        
        print(x)
        
        statedata<-unique(outcome_data$State)
        if(state %in% statedata ){
                
        } else{prine("invalid state")}
        
        outcome_data_state<-outcome_data[outcome_data$State==state,]

        outcome_data_state[,x]<-as.numeric(as.character(outcome_data_state[,x]))
        outcome_data_diseadse <- outcome_data_state[order(outcome_data_state[,x]),]
        y<-as.vector(outcome_data_diseadse[1:1,])
        print(y[2])
}

                             

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")

## 3. Ranking hospitals by outcome in a state
rankhospital<-function(state,outcome,num = "best"){
        ## Read outcome data
        setwd("c:/R/data/")
        outcome_data<- read.csv("outcome-of-care-measures.csv")
        state<-state
        outcome<-outcome
        num<-num
        ## Check that state and outcome are valid
        if(outcome=="heart attack"){
                x<-11
        }else if(outcome=="heart failure"){
                x<-17
        }else if(outcome=="pneumonia"){
                x<-23
        }else{print("invalid outcome")}
        
        
        statedata<-unique(outcome_data$State)
        if(state %in% statedata ){
                
        } else{prine("invalid state")}
        
##生成疾病死亡率表格
        
        outcome_data_state<-outcome_data[outcome_data$State==state,]
        outcome_data_state[,x]<-as.numeric(as.character(outcome_data_state[,x]))
        outcome_data_diseadse <- outcome_data_state[order(outcome_data_state[,x],outcome_data_state[,"Hospital.Name"]),]
        y<-outcome_data_diseadse[c(2,x)]
        y<-y[!is.na(y[2]),]
        cownum<-nrow(y)

        if(num == "best"){
                num<-1
        }else if(num == "worst"){
                num<-cownum
        }else if(num>cownum){
               print("NA")
        } else {num<-num}
        y$rank<-rank(y[,2],ties.method="first")
        print(y[num,])
   
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)

## 4.Ranking hospitals in all states


rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Read outcome data
        setwd("c:/R/data/")
        outcome_data<- read.csv("outcome-of-care-measures.csv")
        state<-state
        outcome<-outcome
        num<-num
        ## Check that state and outcome are valid
        if(outcome=="heart attack"){
                x<-11
        }else if(outcome=="heart failure"){
                x<-17
        }else if(outcome=="pneumonia"){
                x<-23
        }else{print("invalid outcome")}
        
        
        statedata<-unique(outcome_data$State)
        if(state %in% statedata ){
                
        } else{prine("invalid state")}
        
        statedata<-as.vector(statedata)
        
        

        
        
        n=0
        for (i in statedata)
        {
                n=n+1
                
                outcome_data_state<-outcome_data[outcome_data$State==i,]
                outcome_data_state[,x]<-as.numeric(as.character(outcome_data_state[,x]))
                outcome_data_diseadse <- outcome_data_state[order(outcome_data_state[,x],outcome_data_state[,"Hospital.Name"]),]
                y<-outcome_data_diseadse[c(2,x)]
                y$rank<-rank(y[,2],ties.method="first")
                y$state<-i
                cownum<-nrow(y)
                if(num == "best"){
                        num<-1
                        z[n,]=y[num,]
                        z[n,4]=i
                        print(n)
                }else if(num == "worst"){
                        num<-cownum
                        z[n,]=y[num,]
                        z[n,4]=i
                        print(n)
                }else if(num>cownum){
                        print("NA")
                } else {num<-num}
                z[n,]=y[num,]
                z[n,4]=i
                print(n)
        }
        
        
        res<-z[,c(1,4)]
        print(res)
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}


rankall("heart attack", 20)


best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
as.character(subset(r, 2 == "HI")$hospital)

rankhospital("NJ", "pneumonia", "worst")
r <- rankall("pneumonia", "worst")
r <- rankall("heart failure", 10)
