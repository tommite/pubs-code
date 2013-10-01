package fi.smaa.prefsel;

import org.junit.Assert;
import org.junit.Test;

public class NullPreferenceModelTest {

	@Test
	public void testCompare() {
		Assert.assertEquals(PreferenceModel.PreferenceRelation.INCOMPARABLE, 
				new NullPreferenceModel().compare(null, null, new double[]{1.0}, new double[]{1.0}));
		
	}
}
