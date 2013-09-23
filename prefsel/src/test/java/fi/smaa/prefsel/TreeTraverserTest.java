package fi.smaa.prefsel;

import static org.junit.Assert.assertEquals;

import org.apache.commons.math3.linear.Array2DRowRealMatrix;
import org.apache.commons.math3.linear.RealMatrix;
import org.junit.Before;
import org.junit.Test;

public class TreeTraverserTest {

	private AnswerNode root;

	@Before
	public void setUp() {
		RealMatrix imp = new Array2DRowRealMatrix(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		);
		
		root = ExhaustiveQuestionTreeSearch.buildTree(imp, new NullPreferenceModel());
	}

	@Test
	public void testNrNodes() {
		assertEquals(13 * 6 + 4, TreeTraverser.nrNodes(root)); // should be this amount due to transitivity..
	}
}
