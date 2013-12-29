package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class SamplerTest {
	
	private Sampler sampler;

	@Before
	public void setUp() {
		this.sampler = new Sampler();
	}
	
	@Test
	public void testRandomPointFromHyperSphere1stQuadrant() {
		double[] pt = sampler.randomPointFromHyperSphere1stQuadrant(2);
		assertEquals(1.0, Math.sqrt(pt[0] * pt[0] + pt[1] * pt[1]), 0.01);
	}
	
	@Test
	public void testRandomParetoPerformances() {
		RealMatrix mat = sampler.randomParetoPerformances(2, 3);
		assertEquals(2, mat.getRowDimension());
		assertEquals(3, mat.getColumnDimension());
		assertTrue(mat.getEntry(0, 0) > 0.0 || mat.getEntry(0, 1) > 0.0); // very unlikely both are 0 if randomly sampled
	}

}
