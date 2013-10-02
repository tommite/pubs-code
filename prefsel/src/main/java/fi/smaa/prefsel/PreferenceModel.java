package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;


public interface PreferenceModel {
	
	public PreferenceRelation compare(TransitiveAntisymmetricRelation rel, RealMatrix impactMatrix, double[] a1, double[] a2);
	
	public enum PreferenceRelation {
		FIRST_PREFERRED,
		SECOND_PREFERRED,
		EQUAL,
		INCOMPARABLE
	}
}
