## P1 check empty fields
data=read.csv(file=file.choose())


flag=(data=="N/A")
data[flag]=NA

flag=(data=="Icons")
data[flag]=NA


flag=(data=="Icon")
data[flag]=NA

emptyarray=data[is.na(data)]
if(length(emptyarray)==0)
{
  print("thare is no empty fields")
}else{
  print("there are ")
  print(length(emptyarray) )
  print("empty fields")
}