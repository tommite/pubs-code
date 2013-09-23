package fi.smaa.prefsel;


public interface PreferenceModel {
	
	public PreferenceRelation compare(double[] a1, double[] a2);
	
	public enum PreferenceRelation {
		FIRST_PREFERRED,
		SECOND_PREFERRED,
		EQUAL,
		INCOMPARABLE
	}

	public PreferenceModel copyWithRelation(TransitiveRelation newPrefs);
}
