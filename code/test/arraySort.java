class ArraySort{
	public static void main(String[] a){
		A obj_A;
        int i;
        int position;
        int key;
		obj_A = new A();
		i = obj_A.initData();
		i = obj_A.printArray();
		i = obj_A.insertSort();
		i = obj_A.printArray();
		position = obj_A.BinSearch(8);
        System.out.println(position); 
    }
}
    
class A{
    int[] data;
	int n;
	public int initData(){
		int i;
		data = new int[10];
		n = 10;
		i = 0;
		while(i < n){
			data[i] = 10-i;
			i = i+1;
		}
        return 0;  
	}
    public int insertSort(){
        int i;
		int j;
        int temp;
		i = 1;
        while(i < n){
            if(data[i] < data[i-1]){
                temp = data[i];
                j= i-1;
                while(temp < data[j] && (!(j<0))){
                   data[j+1] = data[j];
                    j = j - 1;
                }        
                data[j+1] = temp;
            }else{
			}			
            i = i + 1;  
        }
        return 0;      
    }
    public int printArray() {
        int i;
		i = 0;		
        while (i < n) {
            System.out.println(data[i]); 
            i=i+1;
        }
        return 0;  
    }
    public int BinSearch(int key){
        int low;
        int high;
        int mid;
		low = 0;
		high = n;
        while(!(low > high)){ 
            mid = (low + high) / 2;
            if(data[mid]==key) 
				return (mid + 1);
			else{
			}	
            if(data[mid] > key)
                high = mid -1;
            else 
                low = mid + 1;
        }
        return 0;             
    }
}
