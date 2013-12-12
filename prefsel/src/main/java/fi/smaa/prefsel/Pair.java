package fi.smaa.prefsel;

public class Pair {
	private int v1;
	private int v2;
	
	public Pair(int v1, int v2) {
		this.v1 = v1;
		this.v2 = v2;
	}
	
	public int getFirst() {
		return v1;
	}
	
	public int getSecond() {
		return v2;
	}
	
	@Override
	public String toString() {
		return v1 + " " + v2;
	}
}
