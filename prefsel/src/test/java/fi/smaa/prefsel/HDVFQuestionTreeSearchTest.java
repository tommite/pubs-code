package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class HDVFQuestionTreeSearchTest {
	private AnswerNode root;

	@Before
	public void setUp() {
		RealMatrix imp = new Array2DRowRealMatrix(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		);
		
		root = QuestionTreeSearch.buildTree(imp, new NullPreferenceModel(), new HDVFChoiceStrategy());
	}
	
	@Test
	public void testMaxOneNodeExpandedAtRoot() {
		QuestionNode[] ch = root.getChildren();
		int nrAnswersExpanded = 0;
		for (QuestionNode qn : ch) {
			if (qn.getLeftChild() != null) {
				nrAnswersExpanded++;
			}
			if (qn.getRightChild() != null) {
				nrAnswersExpanded++;
			}
		}
		assertEquals(2, nrAnswersExpanded);
	}
}
