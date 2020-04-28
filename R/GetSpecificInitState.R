GetSpecificInitState<-function(param_and_init,SpecificInitBloc){
  GetInitWithExp<-function(param_and_init,exp,name){
    with(as.list(c(param_and_init)),{
      init<-(eval(parse(text=exp)))
      names(init)<-name
      return(init)
    })
  }
  SpecificInitState<-c()
  StateIdentifBloc<-"_0"
  PackageInitBloc<-SpecificInitBloc
  for (i in 1:length(SpecificInitBloc)){
    PackageInitBloc[i]<-""
    index<-pracma::strfind(SpecificInitBloc[i],StateIdentifBloc)
    state<-rep(0,length(index))
    for (j in 1:length(index)){
      state[j]<-substr(SpecificInitBloc[i],index[j]-1,index[j]-1)
      state[j]<-paste("init",state[j],sep="")
      if (j<length(index)){
        PackageInitBloc[i]<-paste(PackageInitBloc[i],state[j],substr(SpecificInitBloc[i],index[j]+1+length(StateIdentifBloc),index[j+1]-length(StateIdentifBloc)-1),sep="")
      }else{
        PackageInitBloc[i]<-paste(PackageInitBloc[i],state[j],substr(SpecificInitBloc[i],index[j]+1+length(StateIdentifBloc),nchar(SpecificInitBloc[i])),sep="")
      }
    }
    SpecificInitState<-c(SpecificInitState,GetInitWithExp(param_and_init,PackageInitBloc[i],state[1]))
  }
  return(SpecificInitState)
}