class getSet{
	public static void main(String[] a){
		int k;
		B obj_B;
		obj_B = new B();
		k = obj_B.set(10);
		System.out.println(obj_B.get());
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
}

class B extends A{
}