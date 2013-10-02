package fi.smaa.prefsel;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.optimization.GoalType;
import org.apache.commons.math3.optimization.PointValuePair;
import org.apache.commons.math3.optimization.linear.LinearConstraint;
import org.apache.commons.math3.optimization.linear.LinearObjectiveFunction;
import org.apache.commons.math3.optimization.linear.NoFeasibleSolutionException;
import org.apache.commons.math3.optimization.linear.Relationship;
import org.apache.commons.math3.optimization.linear.SimplexSolver;

public class LinearVFPreferenceModel implements PreferenceModel {

	private static final Double EPS = 1E-6;
	private static final double SLACK_BOUND = 1.0;

	public PreferenceRelation compare(TransitiveAntisymmetricRelation prefs, RealMatrix impactMatrix, double[] a1, double[] a2) {
		if (a1.length != a2.length) {
			throw new IllegalArgumentException("PRECOND violation");
		}
		int nrCrit = impactMatrix.getColumnDimension();
		
		LinearObjectiveFunction obj = buildObjectiveFunc(nrCrit);
		List<LinearConstraint> con = new ArrayList<LinearConstraint>();
		
		con.add(buildVarsAddToUnityConstraint(nrCrit));
		con.add(buildSlackBoundedConstraint(nrCrit));
		con.add(buildSlackStrictlyPositiveConstraint(nrCrit));
		
		for (int i=0;i<nrCrit;i++) {
			con.add(buildVarPositiveConstraint(i, nrCrit));
		}
		for (Pair p : prefs.iterator()) {
			int b1 = p.getFirst();
			int b2 = p.getSecond();
			con.add(buildAltLargerThanConstraint(impactMatrix.getRow(b1), impactMatrix.getRow(b2)));
		}
		
		// allocate the other base model
		List<LinearConstraint> con2 = new ArrayList<LinearConstraint>(con);
		
		con.add(buildAltLargerThanConstraint(a1, a2));
		con2.add(buildAltLargerThanConstraint(a2, a1));
		
		PointValuePair p1 = null, p2 = null;
				
		SimplexSolver s = new SimplexSolver();
		try {
			p1 = s.optimize(obj, con, GoalType.MAXIMIZE, true); // a1 >= a2
		} catch (NoFeasibleSolutionException e) { }
		
		try {
			p2 = s.optimize(obj, con2, GoalType.MAXIMIZE, true); // a2 >= a1
		} catch (NoFeasibleSolutionException e) { }
										
		if (p1 == null && p2 != null) {
			return PreferenceRelation.SECOND_PREFERRED;
		} else if (p1 != null && p2 == null) {
			return PreferenceRelation.FIRST_PREFERRED;
		} else if (p1 != null && p2 != null && p1.getValue() < EPS && p2.getValue() < EPS) {
			return PreferenceRelation.EQUAL;
		} else if (p1 != null && p2 != null){
			return PreferenceRelation.INCOMPARABLE;
		} else {
			throw new IllegalStateException("Infeasible model, should not happen");
		}
	}

	private LinearConstraint buildSlackStrictlyPositiveConstraint(int nrCrit) {
		double[] v = allocVarVec(nrCrit);
		v[v.length-1] = 1.0;
		return new LinearConstraint(v, Relationship.GEQ, EPS);
	}

	private LinearObjectiveFunction buildObjectiveFunc(int nrCrit) {
		double[] objv = allocVarVec(nrCrit);
		objv[objv.length-1] = 1.0;
		LinearObjectiveFunction obj = new LinearObjectiveFunction(objv, 0.0);
		return obj;
	}

	private LinearConstraint buildSlackBoundedConstraint(int nrCrit) {
		double[] v = allocVarVec(nrCrit);
		v[v.length-1] = 1.0;
		return new LinearConstraint(v, Relationship.LEQ, SLACK_BOUND);
	}

	private LinearConstraint buildVarsAddToUnityConstraint(int nrCrit) {
		double[] v = allocVarVec(nrCrit);
		for (int i=0;i<nrCrit;i++) {
			v[i] = 1.0;
		}
		return new LinearConstraint(v, Relationship.EQ, 1.0);
	}

	private LinearConstraint buildAltLargerThanConstraint(double[] row, double[] row2) {
		if (row.length != row2.length) {
			throw new IllegalArgumentException("PRECOND violation");
		}
		int nrCrit = row.length;
		double[] v = allocVarVec(nrCrit);
		for (int i=0;i<row.length;i++) {
			v[i] = row[i] - row2[i];
		}
		//v[v.length-1] = 1.0; // add slack
		return new LinearConstraint(v, Relationship.GEQ, EPS);
	}

	private LinearConstraint buildVarPositiveConstraint(int i, int nrCrit) {
		double[] v = allocVarVec(nrCrit);
		v[i] = 1.0;
		return new LinearConstraint(v, Relationship.GEQ, 0.0);
	}

	private double[] allocVarVec(int nrCrit) {
		return new double[nrCrit+1];
	}
}
