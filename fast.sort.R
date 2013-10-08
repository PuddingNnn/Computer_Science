



fast.sort<-function(vec)
{
  k=vec[1]
  len=length(vec)
  i=1; j=len
  while(i<j)
  {
    while (k<=vec[j]&i<j)
    {
      j=j-1
    }
    vec[i]=vec[j]
    
    while (vec[i]<k&i<j)
    {
      i=i+1
    }
    vec[j]=vec[i]
  }
  vec[j]=k
  
  if (len<=3)
  {
    return(vec)
  }
  if (j-1>=1) 
  {
    show(vec); show(j)
    vec[1:(j-1)]=fast.sort(vec[1:(j-1)])
  }
   
  if (j+1<=len)
  {
    show(vec); show(j)
    vec[(j+1):len]=fast.sort(vec[(j+1):len])
  }
    
  return(vec)
}

x=sample(1:20)
fast.sort(x)
res=fast.sort(c(1,2,3,1,3))
