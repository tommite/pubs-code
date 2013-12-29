package fi.smaa.prefsel;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.apache.commons.math3.random.MersenneTwister;
import org.apache.commons.math3.random.RandomGenerator;

public class Sampler {
	
	private static final int SEED = 1911;
	private RandomGenerator rnd;
	
	public Sampler() {
		rnd = new MersenneTwister(SEED);
	}
	
	public double[] randomPointFromHyperSphere1stQuadrant(int dim) {
		double[] pt = new double[dim];
		
		double mul = 0.0;
		
		for (int i=0;i<dim;i++) {
			pt[i] = Math.abs(rnd.nextGaussian());
			mul += pt[i] * pt[i];
		}
		
		mul = 1.0 / Math.sqrt(mul);
		
		for (int i=0;i<dim;i++) {
			pt[i] *= mul;
		}
		
		return pt;
	}

	public RealMatrix randomParetoPerformances(int nAlts, int nCrit) {
		RealMatrix m = new Array2DRowRealMatrix(nAlts, nCrit);
		for (int i=0;i<nAlts;i++) {
			m.setRow(i, randomPointFromHyperSphere1stQuadrant(nCrit));
		}
		return m;
	}
	
	

}
