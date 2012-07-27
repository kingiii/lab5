class HeapSort{
    public static void main(String[] argv){
		A obj_A;
		int i;
		obj_A = new A();
		i = obj_A.initHeap();
        i = obj_A.printHeap();
        i = obj_A.sort();
        i = obj_A.printHeap();            
    }
}

class A{   
    int[] heap ;
    int heapSize;
	int len;
    /*
     * 堆排序算法的主函数 
     */ 
    public int sort(){        
        int i;
		int ret;
		i = 0;
        ret = buildMaxHeap();
        i = heapSize;
        while(i>=2){
			ret = swap(1,i);            
            heapSize=heapSize-1;
            ret = maxHeap(1);
            i=i-1;
        }
		return 0;
    }
    
    /*
     * @param i : 待建的子堆的根元素
     * 构建以i为根的子树为大根堆 
     */
    public int maxHeap(int i){
        int lc;
        int rc;
        int largest;
        int temp;
		int ret;
		lc = 2*i;
		rc = 2*i +1;
		largest = i;
		temp = 0;
        
        if( (lc <= heapSize) && (heap[lc] > heap[i]) )
            largest = lc;
        else
            largest = i;
            
        if( (rc <= heapSize) && (heap[rc] > heap[largest]) )
            largest = rc;
		else{
		}
		
        if( largest != i ){
            ret = swap(i,largest);            
            ret = maxHeap(largest);
        }else{
		}
		
		return 0;          
    }
    
    /*
     * 将整个数组构建成一个大根堆
     */
    public int buildMaxHeap(){
		int ret;
        int i;
        i = heapSize/2;
        while(i>=1){
            ret = maxHeap(i);
            i=i-1;
        }     
		return 0;   
    }
    
    /*
     * @param i : 待交换元素的位置
     * @param j : 待交换元素的位置
     * 交换位置i和位置j的元素
     */    
    public int swap(int i,int j){
        int temp;
		temp = heap[i];
        heap[i] = heap[j];
        heap[j] = temp;
		return 0;
    }
	
    public int initHeap(){
		int i;
		len = 11;
		heapSize = len -1;
		i = 0;
		heap = new int[11];
		while(i<heapSize){
			heap[i] = heapSize-i;
			i = i+1;
		}
		return 0;
	}
    /*
     * 输出堆中的元素
     */
    public int printHeap(){
		
        int i;
		i = 1;
        while(i < len){
            System.out.println(heap[i]);
            i=i+1;
        }
		return 0;
    }
}
    