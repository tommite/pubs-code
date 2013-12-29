package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

/**
 * Preference model that cannot establish any orders (everything is incomparable).
 * 
 * @author tommi
 *
 */
public class NullPreferenceModel implements PreferenceModel {

	public PreferenceRelation compare(TransitiveAntisymmetricIrreflexiveRelation rel, RealMatrix impactMatrix, double[] a1, double[] a2) {
		return PreferenceModel.PreferenceRelation.INCOMPARABLE;
	}

}
