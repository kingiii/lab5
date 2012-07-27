class Gcd {
    public static void main(String[] a){
        int g;
		A obj_A;
		obj_A = new A();
        g = obj_A.gcd(6,24);
        System.out.println(g); 
    }
}

class A{
    public int gcd(int p, int q) {
        if(p%q==0) 
			return q;
        else{ 
			return gcd(q, p%q);
		}
    }

}
