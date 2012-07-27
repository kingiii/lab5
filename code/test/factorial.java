class factorial{
	public static void main(String[] a){
		int k;
		A obj_A;
		obj_A = new A();
		k = obj_A.set(6);
		System.out.println(obj_A.get());
	}
}
class A{
	int n;
	int fact;
	public int set(int a){
		this.n = a;
		this.fact = 1;
		return 0;
	}
	public int get(){
        fact=n;
        while (n>1){
            n = n-1;
            fact = fact * n;
        }
        return fact;
	}
}







