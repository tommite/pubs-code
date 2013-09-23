package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;
import org.paukov.combinatorics.Factory;
import org.paukov.combinatorics.Generator;
import org.paukov.combinatorics.ICombinatoricsVector;

/**
 * Class that implements the exhaustive search for the question minimizing problem
 * 
 * @author tommi
 *
 */
public class ExhaustiveQuestionTreeSearch {
	
	public static AnswerNode buildTree(RealMatrix impactMatrix, PreferenceModel prefModel) {
		return buildRootNode(impactMatrix, prefModel);
	}

	private static AnswerNode buildRootNode(RealMatrix impactMatrix, PreferenceModel prefModel) {

		// initialize the combinatorial generation
		int nrAlts = impactMatrix.getRowDimension();
		
		Integer[] intVec = new Integer[nrAlts];
		for (int i=0;i<nrAlts;i++) {
			intVec[i] = i;
		}
		
		ICombinatoricsVector<Integer> vec = Factory.createVector(intVec);
		Generator<Integer> gen = Factory.createSimpleCombinationGenerator(vec, 2);
				
		AnswerNode root = new AnswerNode(buildQuestions(gen), nrAlts, prefModel, impactMatrix);
		
		for (Node n : root.getChildren()) {
			expandNode((QuestionNode) n);
		}
		
		return root;
	}

	private static void expandNode(QuestionNode n) {
		n.expandLeft();
		n.expandRight();
		
		for (Node cn : n.getLeftChild().getChildren()) {
			expandNode((QuestionNode) cn);
		}
		for (Node cn : n.getRightChild().getChildren()) {
			expandNode((QuestionNode) cn);
		}
	}

	private static Question[] buildQuestions(Generator<Integer> gen) {
		int nrObjs = (int) gen.getNumberOfGeneratedObjects();
		Question[] ret = new Question[nrObjs];
		int idx = 0;
		for (ICombinatoricsVector<Integer> vec : gen) {
			if(vec.getSize() != 2) {
				throw new IllegalStateException("invalid vec size != 2");
			}
			ret[idx] = new Question(vec.getValue(0), vec.getValue(1));
			
			idx++;
		}
		return ret;
	}

}
