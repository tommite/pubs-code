package fi.smaa.prefsel;

/**
 * Preference model that cannot establish any orders (everything is incomparable).
 * 
 * @author tommi
 *
 */
public class NullPreferenceModel implements PreferenceModel {

	public PreferenceRelation compare(double[] a1, double[] a2) {
		return PreferenceModel.PreferenceRelation.INCOMPARABLE;
	}

	public PreferenceModel copyWithRelation(TransitiveRelation newPrefs) {
		return new NullPreferenceModel();
	}

}
