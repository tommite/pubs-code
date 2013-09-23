package fi.smaa.prefsel;

public class Sequencer {
	
	private int current = 1;
	
	public Sequencer() {	
	}
	
	public int next() {
		return current++;
	}

}
