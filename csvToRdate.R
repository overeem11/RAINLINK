library("rio")

srilanka <- import("/home/gayan/Desktop/CMLARE/Data/Processed/processed_file.csv")
export(srilanka,"/home/gayan/Desktop/CMLARE/RAINLINK/data/sriLanka.RData")

#import("/home/gayan/Desktop/CMLARE/RAINLINK/data/sriLanka.RData")