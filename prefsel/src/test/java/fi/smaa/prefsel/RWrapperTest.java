package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class RWrapperTest {
	
	private TransitiveAntisymmetricIrreflexiveRelation rel;
	private RealMatrix im;
	private RWrapper wrap;

	@Before
	public void setUp() {
		rel = new TransitiveAntisymmetricIrreflexiveRelation(3);
		im = new Array2DRowRealMatrix(new double[][]{ {0.9, 0.0}, {0.0, 0.9}, {0.8, 0.2}});
		wrap = RWrapper.initInstance(rel, im);
	}
	
	@Test
	public void testComputeMetrics() {
		wrap.computeMetrics(0, 2);
		assertEquals(0.33, wrap.getHVDF(), 0.02);
		wrap.computeMetrics(1, 2);
		assertEquals(0.48, wrap.getHVDF(), 0.03); // TODO: these are not manually computed numbers, so should re-check the test
	}
	
	@Test
	public void testHangingBug() {
		im = new Array2DRowRealMatrix(new double[][]{{1.0,2.0,3.0},{2.0,1.0,2.0},{3.0,1.0,1.0}});
		rel = new TransitiveAntisymmetricIrreflexiveRelation(3);
		rel.addRelation(1, 0);
		rel.addRelation(1, 2);
		wrap = RWrapper.initInstance(rel, im);
		wrap.computeMetrics(0, 2);
	}
	
}	
