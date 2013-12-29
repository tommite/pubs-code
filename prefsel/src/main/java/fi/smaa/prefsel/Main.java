package fi.smaa.prefsel;

import org.apache.commons.math3.linear.RealMatrix;

public class Main {
	
	private static Sampler sampler = new Sampler(); 

	public static void main(String[] args) {
		int nIters = 10000;
		int nAlts = 4;
		int nCrit = 5;
		TestResult[] res = computationalTest(nIters, nAlts, nCrit);
	}
	
	public static TestResult[] computationalTest(int nIterations, int nAlts, int nCrit) {
		TestResult[] res = new TestResult[nIterations];
	
		for (int i=0;i<nIterations;i++) {
			System.out.println("Iteration " + i);
			res[i] = performTest(nAlts, nCrit);
		}
		return res;
	}

	private static TestResult performTest(int nAlts, int nCrit) {
		RealMatrix perfs = sampler.randomParetoPerformances(nAlts, nCrit);
		AnswerNode fullTree = QuestionTreeSearch.buildTree(perfs, new LinearVFPreferenceModel(), new ExpandAllChoiceStrategy());
		AnswerNode hdvfTree = QuestionTreeSearch.buildTree(perfs, new LinearVFPreferenceModel(), new HDVFChoiceStrategy());
		
		TestResult res = new TestResult();
		
		res.fullMax = TreeTraverser.getMaxNrQuestions(fullTree);
		res.fullMin = TreeTraverser.getMinNrQuestions(fullTree);
		res.hdvfMax = TreeTraverser.getMaxNrQuestions(hdvfTree);
		res.hdvfMin = TreeTraverser.getMinNrQuestions(hdvfTree);
		
		return res;
	}
}
