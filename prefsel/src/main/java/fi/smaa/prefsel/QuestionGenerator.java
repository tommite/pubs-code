package fi.smaa.prefsel;

import org.paukov.combinatorics.Factory;
import org.paukov.combinatorics.Generator;
import org.paukov.combinatorics.ICombinatoricsVector;

public class QuestionGenerator {

	public static Question[] makeAllQuestions(int nrAlts) {
		Integer[] intVec = new Integer[nrAlts];
		for (int i=0;i<nrAlts;i++) {
			intVec[i] = i;
		}
		
		ICombinatoricsVector<Integer> vec = Factory.createVector(intVec);
		Generator<Integer> gen = Factory.createSimpleCombinationGenerator(vec, 2);
		int nrObjs = (int) gen.getNumberOfGeneratedObjects();
		Question[] ret = new Question[nrObjs];
		int idx = 0;
		for (ICombinatoricsVector<Integer> vec1 : gen) {
			if(vec1.getSize() != 2) {
				throw new IllegalStateException("invalid vec size != 2");
			}
			ret[idx] = new Question(vec1.getValue(0), vec1.getValue(1));
			
			idx++;
		}
				
		Question[] qs = ret;
		return qs;
	}

}
