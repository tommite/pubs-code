package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class HDVFQuestionTreeSearchTest {
	private ConcreteAnswerNode root;

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
	public void testOneNodeExpandedAtRoot() {
		QuestionNode[] ch = root.getChildren();
		int nrAnswersExpanded = 0;
		assertEquals(3, ch.length);
		for (QuestionNode qn : ch) {
			if (!(qn.getLeftChild() instanceof UnexpandedNode)) {
				nrAnswersExpanded++;
			}
			if (!(qn.getRightChild() instanceof UnexpandedNode)) {
				nrAnswersExpanded++;
			}
		}
		assertEquals(2, nrAnswersExpanded);
	}
}
