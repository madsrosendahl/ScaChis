
int n; int m; int h1; int h2; int h3; int i; int j;
int[] data = alloc(100);
n = 10;
m=2;
while(m>0){
  for(i=0; i<n; i++){ 
    h3 = random(); skip; data[i] = h3;}
    for(i=1; i<n; i++){
      for(j=n-1; j>=i; j--){
        h1 = data[j]; skip;
        h2 = data[j-1]; skip;
        if(h2 > h1){
          data[j-1] = h1; skip;
          data[j] = h2;
        }
      }
    }
    for(i=0; i<n; i++){ 
       h1 = data[i]; skip; println(h1); }
    m--;
  }
}
// for experiments: end with
// add spike, jump to start and run forever
