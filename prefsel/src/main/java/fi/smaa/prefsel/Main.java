package fi.smaa.prefsel;

import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix2D;

public class Main {

	public static void main(String[] args) {
		DoubleMatrix2D imp = DoubleFactory2D.dense.make(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		);
		
		ExhaustiveQuestionTreeSearch.buildTree(imp, null);
		
	}

}	
