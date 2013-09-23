package fi.smaa.prefsel;

import org.paukov.combinatorics.Factory;
import org.paukov.combinatorics.Generator;
import org.paukov.combinatorics.ICombinatoricsVector;

import cern.colt.matrix.DoubleMatrix2D;

/**
 * Class that implements the exhaustive search for the question minimizing problem
 * 
 * @author tommi
 *
 */
public class ExhaustiveQuestionTreeSearch {
	
	public static AnswerNode buildTree(DoubleMatrix2D impactMatrix, PreferenceModel prefModel) {
		return buildRootNode(impactMatrix);
	}

	private static AnswerNode buildRootNode(DoubleMatrix2D impactMatrix) {

		// initialize the combinatorial generation
		int nrAlts = impactMatrix.rows();
		
		Integer[] intVec = new Integer[nrAlts];
		for (int i=0;i<nrAlts;i++) {
			intVec[i] = i;
		}
		
		ICombinatoricsVector<Integer> vec = Factory.createVector(intVec);
		Generator<Integer> gen = Factory.createSimpleCombinationGenerator(vec, 2);
				
		AnswerNode root = new AnswerNode(buildQuestions(gen));
		
		for (QuestionNode n : root.getChildren()) {
			expandNode(n);
		}
		
		return root;
	}

	private static void expandNode(QuestionNode n) {
		n.expandLeft();
		n.expandRight();
		
		for (QuestionNode cn : n.getLeftChild().getChildren()) {
			expandNode(cn);
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
