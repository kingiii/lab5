class getSet{
	public static void main(String[] a){
		int k;
		A obj_A;
		obj_A = new A();
		k = obj_A.set(10);
		System.out.println(obj_A.get());
	}
}
class A{
	int a;
	public int set(int a){
		this.a = a;
		return 0;
	}
	public int get(){
		return a;
	}
	public int display(){
		System.out.println(a);
		return 0;
	}
}