package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class TransitiveAntisymmetricRelationTest {
	
	private TransitiveAntisymmetricRelation rel;

	@Before
	public void setUp() {
		rel = new TransitiveAntisymmetricRelation(3);
	}
	
	@Test
	public void testEmptyRelation() {
		for (int i=0;i<3;i++) {
			for (int j=0;i<3;i++) {
				assertFalse(rel.getRelation(i, j));
			}
		}
	}

	@Test
	public void testTransitivity() {
		rel.addRelation(0, 1);
		rel.addRelation(1, 2);
		assertTrue(rel.getRelation(0, 2));
	}
	
	@Test
	public void testTransitivity2() {
		rel.addRelation(1, 2);
		rel.addRelation(0, 1);
		assertTrue(rel.getRelation(0, 2));
	}
	
	@Test
	public void testTrueCount() {
		assertEquals(0, rel.getTrueCount());
	}
	
	@Test
	public void testFalseCount() {
		assertEquals((3 * 3), rel.getFalseCount());
	}
	
	@Test
	public void testIteratorCount() {
		int c = 0;
		for (@SuppressWarnings("unused") Pair p : rel.iterator()) { c++; }
		assertEquals(0, c);
	}
	
	@Test
	public void testGetNrBothDirectionsFalse() {
		assertEquals(3, rel.getNrBothDirectionsFalse());
	}


}
