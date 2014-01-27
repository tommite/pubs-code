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
		im = new Array2DRowRealMatrix(new double[][]{ {1.0, 0.0}, {0.0, 1.0}, {0.75, 0.75}});
		rel.addRelation(0, 1);
		wrap = new RWrapper(rel, im, true);
		wrap.computeMetrics(0, 2);
	}
	
	@Test
	public void testComputeDvf() {
		assertEquals(0.5, wrap.getDvf(), 0.01);
	}
	
	@Test
	public void testComputeWin() {
		assertEquals(1, wrap.getWin());
	}
	
	@Test
	public void testComputeApn() {
		assertEquals(2, wrap.getApn());
	}
	
	@Test
	public void testComputeEra() {
		assertEquals(2.0/3.0, wrap.getEra(), 0.01);
	}
	
	@Test
	public void testComputeWpe() {
		assertEquals(1.0/6.0, wrap.getWpe(), 0.01);
	}
	
	@Test
	public void testComputeWre() {
		assertEquals(2.0/3.0, wrap.getWre(), 0.01);
	}
	
	@Test
	public void testHangingBug() {
		im = new Array2DRowRealMatrix(new double[][]{{1.0,2.0,3.0},{2.0,1.0,2.0},{3.0,1.0,1.0}});
		rel = new TransitiveAntisymmetricIrreflexiveRelation(3);
		rel.addRelation(1, 0);
		rel.addRelation(1, 2);
		wrap = new RWrapper(rel, im);
		wrap.computeMetrics(0, 2);
	}
	
}	
