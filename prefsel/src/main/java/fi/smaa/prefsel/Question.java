package fi.smaa.prefsel;

public class Question {

	private int a1;
	private int a2;

	public Question(int a1, int a2) {
		this.a1 = a1;
		this.a2 = a2;
	}
	
	public int getA1() {
		return a1;
	}
	
	public int getA2() {
		return a2;
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof Question) {
			Question qo = (Question) o;
			return qo.a1 == a1 && qo.a2 == a2;
		}
		return false;
	}

	public String dotString(Sequencer seq) {
		return "q" + Integer.toString(a1) + "or" + a2 + "seq" + seq.next();
	}
}
