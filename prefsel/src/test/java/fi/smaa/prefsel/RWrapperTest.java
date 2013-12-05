package fi.smaa.prefsel;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Test;

public class RWrapperTest {
	
	@Test
	public void testComputeMetrics() {
		TransitiveAntisymmetricRelation rel = new TransitiveAntisymmetricRelation(2);
		rel.addRelation(0, 1);
		RealMatrix im = new Array2DRowRealMatrix(new double[][]{ {1.0, 0.0}, {0.0, 2.0}});
			
		RWrapper r = new RWrapper(rel, im);
		
	}
}	
