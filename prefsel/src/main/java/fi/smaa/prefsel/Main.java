package fi.smaa.prefsel;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;

public class Main {

	public static void main(String[] args) {
		RealMatrix imp = new Array2DRowRealMatrix(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		); 
		
		AnswerNode fullTree = QuestionTreeSearch.buildTree(imp, new NullPreferenceModel(), new ExpandAllChoiceStrategy());
		AnswerNode hdvfTree = QuestionTreeSearch.buildTree(imp, new LinearVFPreferenceModel(), new HDVFChoiceStrategy());		
		
		System.out.println(TreeTraverser.toDOT(hdvfTree));
	}
	
	public static void computationalTest(int nAlts, int nCrit) {
		
	}
}
