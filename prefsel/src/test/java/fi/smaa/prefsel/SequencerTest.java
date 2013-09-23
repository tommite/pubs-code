package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Test;

public class SequencerTest {

	@Test
	public void testSequencer() {
		Sequencer s = new Sequencer();
		assertEquals(1, s.next());
		assertEquals(2, s.next());
		assertEquals(3, s.next());
	}
}
