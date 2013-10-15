

#### dynamic programming
### max increasing sequence
Max.seq<-function(vec)
{
  pointer=0
  len=1
  for (i in 2:length(vec))
  {
    tmp_max=0
    tmp_pointer=0
    for (j in 1:(i-1))
    {
      if (vec[j]<=vec[i]&len[j]>tmp_max)
      {
        tmp_pointer=j
        tmp_max=len[j]
      }
    }
    len[i]=tmp_max+1
    pointer[i]=tmp_pointer
  }
  max_len=which.max(len)
  j=max_len
  max_seq=NULL
  while (j!=0)
  {
    max_seq=c(max_seq,vec[j])
    j=pointer[j]
  }
  res=max_seq[length(max_seq):1]
  return(res)
}



Max.seq(c(5,6,7,1,2,8,3,5))