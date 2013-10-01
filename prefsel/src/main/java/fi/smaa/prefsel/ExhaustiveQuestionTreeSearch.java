package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

/**
 * Class that implements the exhaustive search for the question minimizing problem
 * 
 * @author tommi
 *
 */
public class ExhaustiveQuestionTreeSearch {
	
	public static AnswerNode buildTree(RealMatrix impactMatrix, PreferenceModel prefModel) {
		int nrAlts = impactMatrix.getRowDimension();
		Question[] qs = QuestionGenerator.makeAllQuestions(nrAlts);
		AnswerNode root = new AnswerNode(qs, nrAlts);
		expandChildren(root, impactMatrix, prefModel);
		return root;
	}

	private static void expandChildren(AnswerNode node, RealMatrix impactMatrix, PreferenceModel prefModel) {
		for (QuestionNode n : node.getChildren()) {
			expandAllAnswers(n, impactMatrix, prefModel);
			for (AnswerNode an : n.getChildren()) {
				expandChildren(an, impactMatrix, prefModel);
			}
		}
	}

	private static void expandAllAnswers(QuestionNode n, RealMatrix impactMatrix, PreferenceModel prefModel) {
		int a1 = n.getQuestion().getA1();
		int a2 = n.getQuestion().getA2();
	
		TransitiveRelation newRelationLeft = constructRelation(a1, a2, n.getRelation(), impactMatrix, prefModel);
	
	}

	/**
	 * Construct a new preference relation by adding a new preference statement.
	 * 
	 * @param a1
	 * @param a2
	 * @param relation
	 * @param impactMatrix
	 * @param prefModel
	 * @return
	 */
	private static TransitiveRelation constructRelation(int a1, int a2, 
			TransitiveRelation relation, RealMatrix impactMatrix, PreferenceModel prefModel) {
		TransitiveRelation newRel = relation.deepCopy();
		
		relation.addRelation(a1, a2);
		
		// TODO: add filtering of values with the preference model
		
		return newRel;
	}

}
