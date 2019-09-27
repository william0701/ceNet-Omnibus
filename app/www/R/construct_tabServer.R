condition=data.frame(abbr=c('PCC','LA','MS'),
                     used=F,
                     description=c('Pearson Correlation Coefficient','Liquid Association','MicroRNA Significance'),
                     core=0,
                     task="",
                     stringsAsFactors = F
                    )
rownames(condition)=condition$abbr
chosed_condition=c()
validcore=detectCores()