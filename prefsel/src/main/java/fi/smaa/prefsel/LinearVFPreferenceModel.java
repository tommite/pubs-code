package fi.smaa.prefsel;

import org.apache.commons.math3.linear.ArrayRealVector;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.linear.RealVector;
import org.apache.commons.math3.util.Pair;

public class LinearVFPreferenceModel implements PreferenceModel {
	
	private TransitiveRelation prefs;
	private RealMatrix im;

	public LinearVFPreferenceModel(RealMatrix impactMatrix, TransitiveRelation preferences) {
		this.im = impactMatrix;
		this.prefs = preferences;
	}
	
	public PreferenceRelation compare(double[] a1, double[] a2) {
		if (a1.length != a2.length) {
			throw new IllegalArgumentException("PRECOND violation");
		}
		RealVector v1 = new ArrayRealVector(a1);
		RealVector v2 = new ArrayRealVector(a2);
		
		if (checkLinearInequality(v1.subtract(v2))) {
			return PreferenceRelation.FIRST_PREFERRED;
		}
		if (checkLinearInequality(v2.subtract(v1))) {
			return PreferenceRelation.SECOND_PREFERRED;
		}
		return PreferenceRelation.INCOMPARABLE;
	}

	private boolean checkLinearInequality(RealVector point) {
		for (Pair<Integer, Integer> p : prefs.iterator()) {
			int b1 = p.getKey();
			int b2 = p.getValue();
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
