package fi.smaa.prefsel;

import java.util.Iterator;

import org.apache.commons.math3.linear.MatrixUtils;
import org.apache.commons.math3.linear.RealMatrix;

public class TransitiveAntisymmetricRelation {

	private static final double RELATION_TRUE = 1.0;
	
	private RealMatrix matrix;

	public TransitiveAntisymmetricRelation(int nrAlts) {
		matrix = MatrixUtils.createRealMatrix(nrAlts, nrAlts);
		for (int i=0;i<nrAlts;i++) {
			matrix.setEntry(i, i, 1.0);
		}
	}
	
	/**
	 * Gets the amount of trues in the relation 
	 * @return the number of trues in the relation
	 */
	public int getTrueCount() {
		int sum = 0;
		for (int i=0;i<matrix.getRowDimension();i++) {
			for (int j=0;j<matrix.getColumnDimension();j++) {
				if (getRelation(i, j)) {
					sum++;
				}
			}
		}
		return sum;
	}
	
	public boolean getRelation(int a1, int a2) {
		checkA1A2(a1, a2);
		return matrix.getEntry(a1, a2) == RELATION_TRUE;
	}

	private void checkA1A2(int a1, int a2) {
		if (a1 < 0 || a2 < 0 || a1 >= matrix.getRowDimension() || a2 >= matrix.getColumnDimension()) {
			throw new IllegalArgumentException("PRECOND violation");
		}
	}
	
	public void addRelation(int a1, int a2) {
		checkA1A2(a1, a2);
		if (getRelation(a1, a2)) {
			throw new IllegalArgumentException("a1 > a2 already in the relation");
		}
		matrix.setEntry(a1, a2, RELATION_TRUE);
		addDominances(a1, a2);
	}

	private void addDominances(int a1, int a2) {
		boolean adds = false;
		do {
			adds = false;
			for (int i=0;i<matrix.getColumnDimension();i++) {
				if (i != a1 && i != a2) {
					if (getRelation(a2, i) && !getRelation(a1, i)) {
						addRelation(a1, i);
						adds = true;
					} else if (getRelation(i, a1) && !getRelation(i, a2)) {
						addRelation(i, a2);
						adds = true;
					}
				}
			}
		} while (adds);
	}

	public TransitiveAntisymmetricRelation deepCopy() {
		TransitiveAntisymmetricRelation t = new TransitiveAntisymmetricRelation(matrix.getColumnDimension());
		t.matrix = matrix.copy();
		return t;
	}
	
	public Iterable<Pair> iterator() {
		return new Iterable<Pair>() {
			public Iterator<Pair> iterator() {
				return new PreferenceIterator(true);
			}
		};
	}
	
	public Iterable<Pair> negativeIterator() {
		return new Iterable<Pair>() {
			public Iterator<Pair> iterator() {
				return new PreferenceIterator(false);
			}
		};
	}
	
	private class PreferenceIterator implements Iterator<Pair> {
		
		private int i=-1;
		private int j=0;
		private boolean ones;
		
		public PreferenceIterator(boolean ones) {
			this.ones = ones;
			findNext();
		}
		
		private void findNext() {
			i++;
			for (;i<matrix.getRowDimension();i++) {
				for (;j<matrix.getColumnDimension();j++) {
					if (ones) {
						if (getRelation(i, j) && !getRelation(j, i)) {
							return;
						}
					} else {
						if (!getRelation(i, j) && !getRelation(j, i)) {
							return;
						}
					}
				}
				j = 0;
			}
		}

		public boolean hasNext() {
			return i < matrix.getRowDimension();
		}

		public Pair next() {
			Pair ret = new Pair(i, j);
			findNext();
			return ret;
		}

		public void remove() {
			throw new IllegalStateException("remove() not supported");
		}
	}
	
	@Override
	public String toString() {
		return this.matrix.toString();
	}

	public int getDim() {
		return matrix.getColumnDimension();
	}
}
