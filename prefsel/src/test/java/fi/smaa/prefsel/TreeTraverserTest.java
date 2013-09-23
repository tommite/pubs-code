package fi.smaa.prefsel;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;

import cern.colt.matrix.DoubleFactory2D;
import cern.colt.matrix.DoubleMatrix2D;

public class TreeTraverserTest {

	private AnswerNode root;

	@Before
	public void setUp() {
		DoubleMatrix2D imp = DoubleFactory2D.dense.make(new double[][]{
				{1, 2, 3},
				{2, 1, 2},
				{3, 1, 1}}
		);
		
		root = ExhaustiveQuestionTreeSearch.buildTree(imp, new NullPreferenceModel());
	}

	@Test
	public void testNrNodes() {
		assertEquals(58, TreeTraverser.nrNodes(root));
	}
}
