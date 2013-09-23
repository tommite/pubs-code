package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Test;

public class LinearVFPreferenceModelTest {

	
	@Test
	public void testPrerenceModelIncomparable() {
		RealMatrix im = new Array2DRowRealMatrix(new double[][]{
				{1.0, 0.0},
				{0.0, 1.0},
				{0.2, 0.2}
		});
		TransitiveRelation prefs = new TransitiveRelation(3);
		LinearVFPreferenceModel p = new LinearVFPreferenceModel(im, prefs);
		assertEquals(PreferenceModel.PreferenceRelation.INCOMPARABLE, p.compare(im.getRow(0), im.getRow(2)));		
	}

	@Test
	public void testPrerenceModelFirstPreferred() {
		RealMatrix im = new Array2DRowRealMatrix(new double[][]{
				{1.0, 0.0},
				{0.0, 1.0},
				{0.2, 0.2}
		});
		TransitiveRelation prefs = new TransitiveRelation(3);
		LinearVFPreferenceModel p = new LinearVFPreferenceModel(im, prefs);
		prefs.addRelation(0, 1);
		assertEquals(PreferenceModel.PreferenceRelation.FIRST_PREFERRED, p.compare(im.getRow(0), im.getRow(2)));
	}
	
	@Test
	public void testPrerenceModelSecondPreferred() {
		RealMatrix im = new Array2DRowRealMatrix(new double[][]{
				{0.0, 1.0},
				{1.0, 0.0},
				{0.2, 0.2}
		});
		TransitiveRelation prefs = new TransitiveRelation(3);
		LinearVFPreferenceModel p = new LinearVFPreferenceModel(im, prefs);
		
		prefs.addRelation(1, 0);
		assertEquals(PreferenceModel.PreferenceRelation.SECOND_PREFERRED, p.compare(im.getRow(0), im.getRow(2)));
	}

}
