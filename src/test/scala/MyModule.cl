int i=1, n=10, h1, h2;
int[] data = alloc(n);
for(j=n-1; j>=i; j--){
   h1 = data[j]; skip;
   h2 = data[j-1]; skip;
   if(h2 > h1){
     data[j-1] = h1; skip;
     data[j] = h2;
   }
}