
jb_sample_size<-function(proportion1,drops){
    proportion2<-proportion1-drops
    output<-data.frame(NULL)
    for (i in 1:length(proportion2)){
        print(i)
        temp<-power.prop.test(p1=proportion1,p2=proportion2[i],sig.level=0.05,power=0.8,alternative="one.sided")
        output<-rbind(output,
                      data.frame(proportion1=proportion1,
                                 proportion2=proportion2[i],
                                 sample_size=as.numeric(temp[1]),
                                 power=as.numeric(temp[5]),
                                 significance=as.numeric(temp[4])))
    }
    output
}

sample_sizes<-jb_sample_size(proportion1=0.78,
                             drops=c(-0.02,-0.01,-0.009,-0.008,-0.007,-0.006,-0.005,-0.004,-0.003,
                                     -0.002,-0.001,0.001,0.002,0.003,0.004,0.005,0.006,0.007,
                                     0.008,0.009,0.01,0.02))

View(sample_sizes)


copy_to_clipboard <- function(x,row.names=FALSE,col.names=TRUE,...) {
    write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

copy_to_clipboard(sample_sizes)