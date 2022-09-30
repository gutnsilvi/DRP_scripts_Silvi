##this script is used to change the plate layout for theDB in cases were detected contamination or other technical problems


#change working folder accordingly 

wd = ("/Users/silvi/Documents/01_KiSpi_DRP/R_scripts")
      
setwd(wd)


#this specifies the output_file you want to change (coming from perl-script)

file = ("/Users/silvi/Documents/01_KiSpi_DRP/perl/output_file_LK2022106_107.txt")

layout = read.table(file, sep="\t", header=TRUE, fill = TRUE)
layout1 = layout

#for removal of entire drug treatment example: paste("D", as.character(3:12), sep = ""))
#for removal of single wells "D3"
#for removal of multiple wells of same drug: paste("D", as.character(c(3,4,10)), sep = ""))  

#change code below according to what you want to exclude from your analysis

remove_plate1 = c("C3",paste("D", as.character(3:12), sep = ""), paste("G", as.character(c(3,4,10)), sep = ""))
remove_plate2 = c("D13","J14")
remove_plate3= c("D13","J14")
remove_plate4 = c("J10","J14")
remove_plate5 = c("K13","J14")

#this section searches for the specified lines we want to exlude and saves a list of rows to be excluded in the variable delete_Row

delete_Row = vector()

if (exists("remove_plate1")) {
  
  for (i in 1: length(remove_plate1)) {
     for (j in 1:nrow(layout)) {
      if (layout$Plate[j] == 1 & grepl(remove_plate1[i], layout$Well[j], fixed = TRUE)) {
        
        delete_Row =  c(delete_Row, j)
      }
     }
  }
}

if (exists("remove_plate2")) {
  
  for (i in 1: length(remove_plate2)) {
      for (j in 1:nrow(layout)) {
        if (layout$Plate[j] == 2 & grepl(remove_plate2[i], layout$Well[j], fixed = TRUE)) {
          
          delete_Row =  c(delete_Row, j)
        }
      }  
  }
}

if (exists("remove_plate3")) {    
  
  for (i in 1: length(remove_plate3)) {
      for (j in 1:nrow(layout)) {
        if (layout$Plate[j] == 3 & grepl(remove_plate3[i], layout$Well[j], fixed = TRUE)) {
         
          delete_Row =  c(delete_Row, j)
        }
      }
  }
}

if (exists("remove_plate4")) { 
  
  for (i in 1: length(remove_plate4)) {
      for (j in 1:nrow(layout)) {
        if (layout$Plate[j] == 4 & grepl(remove_plate4[i], layout$Well[j], fixed = TRUE)) {
        
          delete_Row =  c(delete_Row, j)
        }
      }  
  }
} 

if (exists("remove_plate5")) { 
  
  for (i in 1: length(remove_plate4)) {
    for (j in 1:nrow(layout)) {
      if (layout$Plate[j] == 5 & grepl(remove_plate5[i], layout$Well[j], fixed = TRUE)) {
      
        delete_Row =  c(delete_Row, j)
      }
    }  
  }
} 

drugs = unique(layout$Content) 
layout = layout[-c(delete_Row),]

#in some cases there are more columns, to avoid problems downstream:

if (ncol(layout)>5){
  layout = layout[,c(1:5)]
  
}

## remove the na from the analysis and exchange to empty value

layout[is.na(layout)] = ""

## this section generates a missing drugs list from the original drug layout that we intended to screen

drugs_left = unique(layout$Content) 

matched <- intersect(drugs, drugs_left)
all <-  union(drugs, drugs_left)
drugs_missing <- drugs[!drugs %in% matched]



print(paste(c("These drugs are missing from the layout:", drugs_missing), collapse=" ", sep = ","))

## save your files - output file is ready to be copied to thDB - change name

write.table(drugs_missing, "missingDrugs_output_file_LK2022106_adj_test.txt", sep = "\t", quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(layout, "output_file_LK2022106_adj_test.txt", sep = "\t", quote = FALSE,row.names = FALSE)

