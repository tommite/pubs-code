package fi.smaa.prefsel;

import org.apache.commons.math3.linear.ArrayRealVector;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;

public class LinearVFPreferenceModel implements PreferenceModel {

	public PreferenceRelation compare(TransitiveRelation prefs, RealMatrix impactMatrix, double[] a1, double[] a2) {
		if (a1.length != a2.length) {
			throw new IllegalArgumentException("PRECOND violation");
		}
		RealVector v1 = new ArrayRealVector(a1);
		RealVector v2 = new ArrayRealVector(a2);
		
		if (checkLinearInequality(v1.subtract(v2), prefs, impactMatrix)) {
			return PreferenceRelation.FIRST_PREFERRED;
		}
		if (checkLinearInequality(v2.subtract(v1), prefs, impactMatrix)) {
			return PreferenceRelation.SECOND_PREFERRED;
		}
		return PreferenceRelation.INCOMPARABLE;
	}

	private boolean checkLinearInequality(RealVector point, TransitiveRelation prefs, RealMatrix im) {
		for (Pair p : prefs.iterator()) {
			int b1 = p.getFirst();
			int b2 = p.getSecond();
			RealVector v1 = new ArrayRealVector(im.getRow(b1));
			RealVector v2 = new ArrayRealVector(im.getRow(b2));
			RealVector sub = v1.subtract(v2);
			
			if (point.dotProduct(sub) > 0.0) {
				return true;
			}
		}
		return false;
	}
}
