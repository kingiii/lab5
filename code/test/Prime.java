class Prime { 
    public static void main(String[] a){
		A obj_A;
        int i;
		obj_A = new A();
        i = obj_A.generatePrime();
    }
}

class A{
	public int generatePrime() {
        int[] data; //data[i] is 1 represents i is prime
		int size;
        int k;
		int j;
		int i;
		k = 0;
		size = 51;
		data = new int[51];
        while (k < size){//init
            data[k]=1;
            k=k+1;
        }
        data[0]=0; 
        data[1]=0;
        data[2]=1;
        i=3;
        while(i<size){
            j=i;
            k=2;
            while(k<j){   
                if ( data[j] && (j % k == 0) ){
                    data[j] = 0;
                }else{
				}
                k=k+1;
            }
            i=i+1;
        }
        i=0;
        while(i<size){
            if (data[i] == 1){
                System.out.println(i);
            }else{
			}
            i=i+1;
        }
    }
}