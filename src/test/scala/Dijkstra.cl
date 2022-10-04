 mem grf=[0, 1, 7,  0, 2, 9,  0, 5, 14, 
    1, 2, 10,  1, 3, 15,  2, 3, 11,  2, 5, 2,  
    3, 4, 6,  4, 5, 9];
 gsz = 9;

 void countNodes(){
   nmx = 0;
   for(c=0; c<gsz; c++){
     t = grf[c * 3];
     skip;
     nmx=(nmx < t ? t : nmx);
     t = grf[c * 3 + 1];
     skip;
     nmx=(nmx < t ? t : nmx);
   }
 }

 void initMem(){
   o = 0;
   for(c=0; c <= nmx; c++){
     t = c*3;
     skip;
     heap[t] = 0;
     heap[t+1] = 0;
     heap[t+2] = 1000;
   }
   heap[(o*3)+2] = 0;
 }

 void findCurr(){
   curr = -1;
   wgt = 1000;
   for(c=0; c <= nmx; c++){
     n1 = heap[c*3+1];
     n2 = heap[c*3+2];
     skip;
     if((n1==0)&&(n2<wgt)){
       curr=c;
       wgt=n2;
     }
   }
 }
 void updateMem(){
    for(c=0; c < gsz; c++){
      if(grf[c*3] == curr) {
        n2=grf[c*3+1];
        w=grf[c*3+2];
        skip;
        if (heap[(n2 * 3) + 2] > (wgt + w)) {
          heap[n2*3] = curr;
          heap[(n2 * 3) + 2] = (wgt + w);
        }
      }
    }
    heap[curr*3+1]=1;
 }

 // main

 countNodes();
 initMem();
 for(i=0; i<=nmx; i++){
    findCurr();
    if(curr>=0)  updateMem();
 }

