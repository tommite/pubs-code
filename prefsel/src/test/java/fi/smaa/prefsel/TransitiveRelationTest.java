package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

public class TransitiveRelationTest {
	
	private TransitiveRelation rel;

	@Before
	public void setUp() {
		rel = new TransitiveRelation(3);
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

}
