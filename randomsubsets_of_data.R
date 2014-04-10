sample_record=sample(1:nrow(cocos),10000,replace=F)
sample_record=sample_record[order(sample_record)]
subset=cocos[sample_record,]
