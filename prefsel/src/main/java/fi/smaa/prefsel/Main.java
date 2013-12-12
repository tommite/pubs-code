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
		
		AnswerNode tree = QuestionTreeSearch.buildTree(imp, new NullPreferenceModel(), new ExpandAllChoiceStrategy());
		System.out.println(TreeTraverser.toDOT(tree));
	}
}	
