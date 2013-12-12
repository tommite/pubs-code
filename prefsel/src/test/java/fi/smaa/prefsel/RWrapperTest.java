package fi.smaa.prefsel;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

import cern.colt.Arrays;

public class RWrapperTest {
	
	private TransitiveAntisymmetricRelation rel;
	private RealMatrix im;
	private RWrapper wrap;

	@Before
	public void setUp() {
		rel = new TransitiveAntisymmetricRelation(3);
		im = new Array2DRowRealMatrix(new double[][]{ {0.9, 0.0}, {0.0, 0.9}, {0.8, 0.2}});
		wrap = new RWrapper(rel, im);
	}
	
	@Test
	public void testComputeMetrics() {
		wrap.computeMetrics(0, 2);
		assertEquals(0.33, wrap.getHVDF(), 0.02);
		wrap.computeMetrics(1, 2);
		assertEquals(0.46, wrap.getHVDF(), 0.02);
	}
	
	@Test
	public void testGetMetricPairs() {
		double[][] pairs = wrap.createMetricPairs(rel);
		assertEquals(3, pairs.length);

		assertArrayEquals(new double[]{0.0, 1.0}, pairs[0], 0.0001);
		assertArrayEquals(new double[]{0.0, 2.0}, pairs[1], 0.0001);
		assertArrayEquals(new double[]{1.0, 2.0}, pairs[2], 0.0001);
	}
}	
