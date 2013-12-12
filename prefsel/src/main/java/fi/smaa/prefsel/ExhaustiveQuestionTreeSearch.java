package fi.smaa.prefsel;

import java.util.ArrayList;

import org.apache.commons.math3.linear.RealMatrix;

import fi.smaa.prefsel.PreferenceModel.PreferenceRelation;

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
		
		if (n.getRelation().getRelation(a1, a2) || n.getRelation().getRelation(a2, a1)) {
			throw new IllegalStateException("Question for a relation that is already set");
		}
		
		TransitiveAntisymmetricRelation newRelationLeft = constructRelation(a1, a2, n.getRelation(), impactMatrix, prefModel);
		TransitiveAntisymmetricRelation newRelationRight = constructRelation(a2, a1, n.getRelation(), impactMatrix, prefModel);
		
		Question[] qsLeft = filterQuestions(n.getRemainingQuestions(), newRelationLeft);
		Question[] qsRight = filterQuestions(n.getRemainingQuestions(), newRelationRight);
	
		n.expandLeft(qsLeft, newRelationLeft);
		n.expandRight(qsRight, newRelationRight);
	}

	private static Question[] filterQuestions(Question[] qs, TransitiveAntisymmetricRelation rel) {
		ArrayList<Question> ql = new ArrayList<Question>();
		for (Question q : qs) {
			if (!rel.getRelation(q.getA1(), q.getA2()) && !rel.getRelation(q.getA2(), q.getA1())) {
				ql.add(q);
			}
		}
		return ql.toArray(new Question[0]);
	}

	/**
	 * Construct a new preference relation by adding a new preference statement a1 > a2.
	 * 
	 * @param a1 the alternative being preferred
	 * @param a2 the alternative being not preferred
	 * @param relation the relation to use as base
	 * @param impactMatrix impact matrix of alternative evaluations
	 * @param prefModel the applied preference model
	 * @return A new preference relation including (a1, a2) and possible other pairs inferred through the preference model 
	 */
	private static TransitiveAntisymmetricRelation constructRelation(int a1, int a2, TransitiveAntisymmetricRelation relation, RealMatrix impactMatrix, PreferenceModel prefModel) {
		TransitiveAntisymmetricRelation newRel = relation.deepCopy();
		
		newRel.addRelation(a1, a2);
		for (Pair p : newRel.negativeIterator()) {
			if (newRel.getRelation(p.getFirst(), p.getSecond()) || newRel.getRelation(p.getSecond(), p.getFirst())) {
				continue;
			}
			PreferenceRelation val = prefModel.compare(newRel, impactMatrix, impactMatrix.getRow(p.getFirst()), impactMatrix.getRow(p.getSecond()));
			if (val == PreferenceRelation.FIRST_PREFERRED) {
				newRel.addRelation(p.getFirst(), p.getSecond());
			}
			if (val == PreferenceRelation.SECOND_PREFERRED) {
				newRel.addRelation(p.getSecond(), p.getFirst());
			}
		}
		
		return newRel;
	}

}
